# ============================================================
# SENSITIVITY ANALYSIS FOR GCE SUT BALANCING
# ============================================================
#
# Re-runs the GCE balancing of GCE_SUT_Input.xlsx under
# alternative specifications of:
#   (A) the support-points matrix R (all r scaled by a factor),
#   (B) the prior probabilities Q (weight on the central point),
#   (C) the reliability ranking (r for intermediate consumption
#       and for household consumption swapped).
#
# The GCE function below implements Steps 3, 5, 6 and 7 of
# GCE_SUT_Balancing.R (support points, prior, dual/Newton
# optimization, balanced matrix) in compact form. The baseline
# scenario reproduces GCE_SUT_Results.xlsx.
# ============================================================

required_packages <- c("readxl", "writexl", "MASS")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(readxl)
library(writexl)

input_file  <- "GCE_SUT_Input.xlsx"
output_file <- "Sensitivity_Results.xlsx"

read_mat <- function(file, sheet) {
  x <- as.matrix(suppressMessages(read_excel(file, sheet = sheet, col_names = FALSE)))
  dimnames(x) <- NULL
  x
}

A  <- read_mat(input_file, "A")
R0 <- read_mat(input_file, "R")
row_target <- as.vector(read_mat(input_file, "ROW_TARGET"))
col_target <- as.vector(read_mat(input_file, "COL_TARGET"))

n <- nrow(A); k <- ncol(A); M <- 3

# ------------------------------------------------------------
# GCE balancing (dual formulation, Newton's method)
# ------------------------------------------------------------

gce_balance <- function(A, R, u, v, q = c(0.025, 0.95, 0.025),
                        tol = 1e-9, maxit = 200) {

  B <- array(0, c(n, k, M))
  B[, , 1] <- A - R * abs(A)
  B[, , 2] <- A
  B[, , 3] <- A + R * abs(A)

  sc <- max(1, max(abs(B)) / 10)
  Bs <- B / sc; us <- u / sc; vs <- v / sc

  post <- function(th) {
    Z <- outer(th[1:n], th[(n + 1):(n + k)], "+")
    E <- array(0, c(n, k, M))
    for (m in 1:M) E[, , m] <- Z * Bs[, , m]
    Emax <- apply(E, c(1, 2), max)            # log-sum-exp stabilization
    for (m in 1:M) E[, , m] <- q[m] * exp(E[, , m] - Emax)
    Om <- apply(E, c(1, 2), sum)
    list(P = sweep(E, c(1, 2), Om, "/"), logOm = log(Om) + Emax)
  }
  L <- function(th) sum(post(th)$logOm) - sum(th[1:n] * us) - sum(th[(n + 1):(n + k)] * vs)

  th <- rep(0, n + k)
  for (it in 1:maxit) {
    P  <- post(th)$P
    Xs <- apply(P * Bs, c(1, 2), sum)
    Vs <- apply(P * Bs^2, c(1, 2), sum) - Xs^2
    g  <- c(rowSums(Xs) - us, colSums(Xs) - vs)
    res <- max(abs(g)) * sc
    if (res < tol) break
    H <- matrix(0, n + k, n + k)
    diag(H)[1:n] <- rowSums(Vs); diag(H)[(n + 1):(n + k)] <- colSums(Vs)
    H[1:n, (n + 1):(n + k)] <- Vs; H[(n + 1):(n + k), 1:n] <- t(Vs)
    step <- -as.vector(MASS::ginv(H) %*% g)
    # Backtracking line search. Near the optimum, rounding error
    # can make the Armijo test fail; a step is then also accepted
    # if it reduces the residuals.
    f0 <- L(th); t <- 1
    repeat {
      th_new <- th + t * step
      if (L(th_new) <= f0 + 1e-4 * t * sum(g * step)) break
      P_new <- post(th_new)$P
      X_new <- apply(P_new * Bs, c(1, 2), sum)
      if (max(abs(c(rowSums(X_new) - us, colSums(X_new) - vs))) < max(abs(g))) break
      t <- t / 2
      if (t < 1e-10) break
    }
    th <- th_new
  }

  P <- post(th)$P
  X <- apply(P * B, c(1, 2), sum)
  Qa <- array(rep(q, each = n * k), c(n, k, M))
  ps <- pmax(P, 1e-15)
  S <- -apply(P * log(ps), c(1, 2), sum) / log(M)

  list(X = X, converged = res < tol, iterations = it - 1,
       objective = sum(P * log(ps / Qa)), entropy = S)
}

# ------------------------------------------------------------
# Cell groups used for reporting (defined on the baseline R)
# ------------------------------------------------------------

nz <- A != 0
grp <- matrix("", n, k)
grp[, 1:4]  <- "Intermediate consumption"
grp[, 5:6]  <- "HH and GFCF"
grp[, 7]    <- "Changes in inventories"
grp[, 9:16] <- "Supply side (excl. M)"
adjustable0 <- nz & R0 > 0

summarise <- function(res, scenario, X_base) {
  rel <- abs(res$X - A) / abs(A)
  g <- function(name) round(100 * mean(rel[adjustable0 & grp == name]), 2)
  data.frame(
    Scenario                   = scenario,
    Converged                  = res$converged,
    Iterations                 = res$iterations,
    Objective                  = round(res$objective, 3),
    Mean_pct_Intermediate      = g("Intermediate consumption"),
    Mean_pct_HH_GFCF           = g("HH and GFCF"),
    Mean_pct_CI                = g("Changes in inventories"),
    Mean_pct_Supply            = g("Supply side (excl. M)"),
    Max_pct_change             = round(100 * max(rel[nz]), 2),
    Total_abs_adjustment       = round(sum(abs(res$X - A)), 1),
    Mean_reliability_adjustable = round(1 - mean(res$entropy[adjustable0]), 3),
    Mean_abs_diff_vs_baseline  = round(mean(abs(res$X - X_base)[adjustable0]), 2)
  )
}

# ------------------------------------------------------------
# Scenarios
# ------------------------------------------------------------

base <- gce_balance(A, R0, row_target, col_target)
Xb <- base$X

R_swap <- R0
R_swap[1:5, 1:4] <- 0.1   # intermediate consumption treated as reliable
R_swap[1:5, 5]   <- 0.3   # household consumption treated as less reliable

scenarios <- list(
  list("Baseline (R as in Figure 4, q = 0.95)", R0, c(0.025, 0.95, 0.025)),
  list("A1: R x 0.5",                           R0 * 0.5, c(0.025, 0.95, 0.025)),
  list("A2: R x 1.5",                           R0 * 1.5, c(0.025, 0.95, 0.025)),
  list("A3: R x 2",                             R0 * 2,   c(0.025, 0.95, 0.025)),
  list("B1: q = (0.10, 0.80, 0.10)",            R0, c(0.10, 0.80, 0.10)),
  list("B2: q = (0.005, 0.99, 0.005)",          R0, c(0.005, 0.99, 0.005)),
  list("B3: uniform q = (1/3, 1/3, 1/3)",       R0, rep(1/3, 3)),
  list("C1: r swapped (IC 0.1, HH 0.3)",        R_swap, c(0.025, 0.95, 0.025))
)

results <- list(); balanced <- list()

for (s in scenarios) {
  res <- gce_balance(A, s[[2]], row_target, col_target, q = s[[3]])
  results[[length(results) + 1]] <- summarise(res, s[[1]], Xb)
  balanced[[substr(gsub("[^A-Za-z0-9]", "_", s[[1]]), 1, 31)]] <- as.data.frame(round(res$X, 4))
}

summary_df <- do.call(rbind, results)
print(summary_df, row.names = FALSE)

write_xlsx(c(list(SENSITIVITY = summary_df), balanced), output_file)
cat("Output file:", output_file, "\n")
