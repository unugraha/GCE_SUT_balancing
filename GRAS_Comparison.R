# ============================================================
# GRAS COMPARISON FOR THE GCE SUT BALANCING EXAMPLE
# ============================================================
#
# Balances the same initial matrix A (GCE_SUT_Input.xlsx) with
# the Generalized RAS (GRAS) method and compares the result with
# the GCE solution in GCE_SUT_Results.xlsx.
#
# Run GCE_SUT_Balancing.R first.
#
# GRAS (Junius and Oosterhaven, 2003; Lenzen et al., 2007):
#   A = P - N, with P = max(A, 0) and N = max(-A, 0)
#   x_ij = r_i * p_ij * s_j - n_ij / (r_i * s_j)
#   r and s are updated alternately until the row and column
#   targets are met.
#
# Two variants are computed:
#   GRAS-1 : all non-zero cells may change.
#   GRAS-2 : cells with r = 0 in matrix R (here X, M, COE, GOS)
#            are held fixed by removing them from the matrix and
#            subtracting them from the targets. This is the only
#            way GRAS can use reliability information: a cell is
#            either fully fixed or fully adjustable.
#
# Note: standard RAS cannot be applied to this matrix because it
# contains negative entries (supply side and changes in
# inventories).
# ============================================================

required_packages <- c("readxl", "writexl")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(readxl)
library(writexl)

input_file  <- "GCE_SUT_Input.xlsx"
gce_file    <- "GCE_SUT_Results.xlsx"
output_file <- "GRAS_Comparison_Results.xlsx"

read_mat <- function(file, sheet, col_names = FALSE) {
  x <- suppressMessages(read_excel(file, sheet = sheet, col_names = col_names))
  x <- as.matrix(x)
  dimnames(x) <- NULL
  x
}

A <- read_mat(input_file, "A")
R <- read_mat(input_file, "R")
row_target <- as.vector(read_mat(input_file, "ROW_TARGET"))
col_target <- as.vector(read_mat(input_file, "COL_TARGET"))
X_gce <- read_mat(gce_file, "BALANCED_X", col_names = TRUE)

# ------------------------------------------------------------
# GRAS algorithm
# ------------------------------------------------------------

gras <- function(A, u, v, fixed = matrix(FALSE, nrow(A), ncol(A)),
                 tol = 1e-10, maxit = 10000) {

  F <- A * fixed
  Z <- A
  Z[fixed] <- 0

  # Targets net of fixed cells
  u <- u - rowSums(F)
  v <- v - colSums(F)

  P <- pmax(Z, 0)
  N <- pmax(-Z, 0)

  r <- rep(1, nrow(A))
  s <- rep(1, ncol(A))

  inv <- function(z) ifelse(z == 0, 0, 1 / z)

  # Positive root of  r * p - n / r = u  (Lenzen et al., 2007)
  solve_factor <- function(p, n, u) {
    ifelse(
      p > 0,
      (u + sqrt(u^2 + 4 * p * n)) / (2 * p),
      ifelse(n > 0, -n / u, 1)
    )
  }

  for (it in 1:maxit) {

    r <- solve_factor(as.vector(P %*% s), as.vector(N %*% inv(s)), u)
    s <- solve_factor(as.vector(t(P) %*% r), as.vector(t(N) %*% inv(r)), v)

    X <- diag(r) %*% P %*% diag(s) - diag(inv(r)) %*% N %*% diag(inv(s))

    if (max(abs(rowSums(X) - u)) < tol &&
        max(abs(colSums(X) - v)) < tol) break
  }

  list(X = X + F, iterations = it)
}

gras1 <- gras(A, row_target, col_target)
gras2 <- gras(A, row_target, col_target, fixed = (R == 0))

cat("GRAS-1 iterations:", gras1$iterations, "\n")
cat("GRAS-2 iterations:", gras2$iterations, "\n")

# ------------------------------------------------------------
# Comparison indicators
# ------------------------------------------------------------

nz <- A != 0

summarise <- function(X, method) {

  rel <- (X - A) / abs(A)

  grp <- function(r_value, f) {
    m <- nz & R == r_value
    round(100 * f(abs(rel[m])), 2)
  }

  data.frame(
    Method                      = method,
    Max_row_residual            = max(abs(rowSums(X) - row_target)),
    Max_col_residual            = max(abs(colSums(X) - col_target)),
    Mean_pct_change_r0.1        = grp(0.1, mean),
    Max_pct_change_r0.1         = grp(0.1, max),
    Mean_pct_change_r0.3        = grp(0.3, mean),
    Max_pct_change_r0.3         = grp(0.3, max),
    Mean_pct_change_r0.5        = grp(0.5, mean),
    Max_pct_change_r0.5         = grp(0.5, max),
    Cells_r0_changed            = sum(abs(X - A)[nz & R == 0] > 1e-6),
    Max_pct_change_r0           = round(100 * max(abs(rel[nz & R == 0])), 2),
    Cells_outside_support       = sum(abs(rel[nz]) > R[nz] + 1e-9),
    Sign_changes                = sum(sign(X[nz]) != sign(A[nz])),
    Total_abs_adjustment        = round(sum(abs(X - A)), 2)
  )
}

comparison <- rbind(
  summarise(gras1$X, "GRAS-1 (all cells adjustable)"),
  summarise(gras2$X, "GRAS-2 (cells with r = 0 fixed)"),
  summarise(X_gce,   "GCE (dual, Newton)")
)

print(t(comparison))

write_xlsx(
  list(
    COMPARISON = comparison,
    GRAS1_X    = as.data.frame(gras1$X),
    GRAS2_X    = as.data.frame(gras2$X),
    GCE_X      = as.data.frame(X_gce),
    INITIAL_A  = as.data.frame(A)
  ),
  output_file
)

cat("Output file:", output_file, "\n")
