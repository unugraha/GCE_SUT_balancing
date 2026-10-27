# ============================================================
# GCE SUT BALANCING - 10-STEP AUTOMATIC PROCEDURE
# Generalized Cross-Entropy (GCE) balancing for Supply-Use Tables
# ============================================================
#
# 10-STEP ALGORITHM
# -----------------
#
# STEP 1  : INPUT & DATA PREPARATION
# STEP 2  : ORIENTATION & CONSISTENCY CHECK
# STEP 3  : DEFINE GCE SUPPORT POINTS
# STEP 4  : FEASIBILITY SCREENING
# STEP 5  : FIND FEASIBLE BALANCED MATRIX (LP / GLPK)
# STEP 6  : CONSTRUCT FEASIBLE INITIAL PROBABILITIES (P0)
# STEP 7  : SPECIFY PRIOR PROBABILITIES & GCE OBJECTIVE
# STEP 8  : GCE CONSTRAINED OPTIMIZATION (SLSQP)
# STEP 9  : DERIVE & VERIFY BALANCED SUT
# STEP 10 : UNCERTAINTY, RELIABILITY & OUTPUT DIAGNOSTICS

rm(list = ls())

# ============================================================
# STEP 1 - INPUT & DATA PREPARATION
# ============================================================
#
# Objective:
#   Read the initial SUT, row targets, column targets, and
#   cell-specific adjustment parameters R.
#
# STANDARD INPUT WORKBOOK
# ----------------------
# The standard GCE_SUT_Input.xlsx workbook is organized as:
#
#   A            : 7 x 16
#   R            : 7 x 16
#   COL_TARGET   : 1 x 16
#   ROW_TARGET   : 16 x 1, with only the first 7 non-empty
#                  values representing the row targets.
#
# The code below is deliberately orientation-robust. It first
# removes empty rows/columns, then determines whether A/R are
# already in analytical orientation or need to be transposed.
#
# Analytical orientation used by the rest of the script:
#
#   A : n x k
#   R : n x k
#   row_target : length n
#   col_target : length k
#
# ============================================================

cat("\n============================================================\n")
cat("GCE SUT BALANCING - 10 STEP AUTOMATIC PROCEDURE\n")
cat("============================================================\n")
cat("STEP 1/10 - INPUT & DATA PREPARATION\n")

required_packages <- c(
  "readxl",
  "writexl",
  "Rglpk",
  "nloptr"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(readxl)
library(writexl)
library(Rglpk)
library(nloptr)

# ------------------------------------------------------------
# File settings
# ------------------------------------------------------------

# Change working directory if necessary.
# setwd("YOUR_WORKING_DIRECTORY")
input_file  <- "GCE_SUT_Input.xlsx"
output_file <- "GCE_SUT_Results.xlsx"

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------
# Remove completely empty rows and columns.
trim_matrix <- function(x) {

  x <- as.matrix(x)

  keep_row <- apply(
    x,
    1,
    function(z) any(!is.na(z) & z != "")
  )

  keep_col <- apply(
    x,
    2,
    function(z) any(!is.na(z) & z != "")
  )

  x[
    keep_row,
    keep_col,
    drop = FALSE
  ]
}

# Read a worksheet and convert it to a clean numeric matrix.
read_numeric_sheet <- function(file, sheet) {

  x <- read_excel(
    file,
    sheet = sheet,
    col_names = FALSE
  )

  x <- trim_matrix(x)

  x <- matrix(
    as.numeric(x),
    nrow = nrow(x),
    ncol = ncol(x)
  )

  x
}

# ------------------------------------------------------------
# Read workbook
# ------------------------------------------------------------

A_raw <- read_numeric_sheet(
  input_file,
  "A"
)

C_raw <- read_numeric_sheet(
  input_file,
  "COL_TARGET"
)

R_raw <- read_numeric_sheet(
  input_file,
  "R"
)

T_raw <- read_numeric_sheet(
  input_file,
  "ROW_TARGET"
)

cat(
  "  Raw A dimensions        :",
  paste(dim(A_raw), collapse = " x "),
  "\n"
)

cat(
  "  Raw R dimensions        :",
  paste(dim(R_raw), collapse = " x "),
  "\n"
)

cat(
  "  Raw COL_TARGET dimensions:",
  paste(dim(C_raw), collapse = " x "),
  "\n"
)

cat(
  "  Raw ROW_TARGET dimensions:",
  paste(dim(T_raw), collapse = " x "),
  "\n"
)

# ============================================================
# STEP 2 - ORIENTATION & CONSISTENCY CHECK
# ============================================================
#
# Objective:
#   Put the matrices into the analytical orientation required
#   by the balancing problem and verify that the accounting
#   system is internally consistent.
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 2/10 - ORIENTATION & CONSISTENCY CHECK\n")

# ------------------------------------------------------------
# Extract target vectors
# ------------------------------------------------------------

row_target_raw <- as.vector(T_raw)
col_target_raw <- as.vector(C_raw)

# ------------------------------------------------------------
# Validate A and R before orientation
# ------------------------------------------------------------

if (!all(dim(A_raw) == dim(R_raw))) {

  stop(
    paste0(
      "A and R must have identical dimensions. ",
      "A = ",
      paste(dim(A_raw), collapse = " x "),
      ", R = ",
      paste(dim(R_raw), collapse = " x "),
      "."
    )
  )
}

# ------------------------------------------------------------
# Determine analytical orientation automatically
# ------------------------------------------------------------
#
# Case 1:
#   A already has dimensions:
#       length(row_target) x length(col_target)
#
# Case 2:
#   A is transposed and must be rotated:
#       length(col_target) x length(row_target)
#
# ------------------------------------------------------------

a_rows <- nrow(A_raw)
a_cols <- ncol(A_raw)

nr_target <- length(row_target_raw)
nc_target <- length(col_target_raw)

if (
  a_rows == nr_target &&
  a_cols == nc_target
) {

  # Standard GCE_SUT_Input.xlsx orientation
  A <- A_raw
  R <- R_raw

  cat(
    "  Orientation detected     : A/R already analytical\n"
  )

} else if (
  a_rows == nc_target &&
  a_cols == nr_target
) {

  # Legacy/transposed orientation
  A <- t(A_raw)
  R <- t(R_raw)

  cat(
    "  Orientation detected     : A/R transposed automatically\n"
  )

} else {

  stop(
    paste0(
      "Cannot determine A/R orientation.\n",
      "A/R dimensions = ",
      a_rows,
      " x ",
      a_cols,
      "\n",
      "ROW_TARGET length = ",
      nr_target,
      "\n",
      "COL_TARGET length = ",
      nc_target,
      "\n",
      "Expected either:\n",
      "  A/R = ROW_TARGET x COL_TARGET, or\n",
      "  A/R = COL_TARGET x ROW_TARGET."
    )
  )
}

row_target <- row_target_raw
col_target <- col_target_raw

n <- nrow(A)
k <- ncol(A)

cat(
  "  Analytical A dimensions :",
  n,
  "x",
  k,
  "\n"
)

cat(
  "  Analytical R dimensions :",
  nrow(R),
  "x",
  ncol(R),
  "\n"
)

cat(
  "  ROW_TARGET length       :",
  length(row_target),
  "\n"
)

cat(
  "  COL_TARGET length       :",
  length(col_target),
  "\n"
)

# ------------------------------------------------------------
# Dimension checks
# ------------------------------------------------------------

if (!all(dim(R) == dim(A))) {
  stop("R and A must have identical analytical dimensions.")
}

if (length(row_target) != n) {
  stop(
    paste0(
      "ROW_TARGET length (",
      length(row_target),
      ") does not equal number of rows in A (",
      n,
      ")."
    )
  )
}

if (length(col_target) != k) {
  stop(
    paste0(
      "COL_TARGET length (",
      length(col_target),
      ") does not equal number of columns in A (",
      k,
      ")."
    )
  )
}

# ------------------------------------------------------------
# Missing-value checks
# ------------------------------------------------------------

if (
  anyNA(A) ||
  anyNA(R) ||
  anyNA(row_target) ||
  anyNA(col_target)
) {
  stop(
    "NA values detected in A, R, ROW_TARGET, or COL_TARGET."
  )
}

# ------------------------------------------------------------
# R must be non-negative
# ------------------------------------------------------------

if (any(R < 0)) {
  stop("R contains negative values.")
}

# ------------------------------------------------------------
# Basic SUT consistency:
# total row targets must equal total column targets
# ------------------------------------------------------------

row_target_total <- sum(row_target)
col_target_total <- sum(col_target)

cat(
  "  Total row targets       :",
  row_target_total,
  "\n"
)

cat(
  "  Total column targets    :",
  col_target_total,
  "\n"
)

if (
  abs(row_target_total - col_target_total) > 1e-8
) {
  stop(
    paste0(
      "Row and column targets do not have the same total. ",
      "Difference = ",
      row_target_total - col_target_total
    )
  )
}

cat("  Consistency checks passed.\n")


# ============================================================
# STEP 3 - DEFINE GCE SUPPORT POINTS
# ============================================================
#
# Objective:
#   Translate the initial estimate A_ij and its allowable
#   relative adjustment R_ij into discrete support values.
#
# For each cell:
#
#   b1 = (1-r_ij) * a_ij
#   b2 = a_ij
#   b3 = (1+r_ij) * a_ij
#
# These support points define the feasible range within which
# the final GCE estimate may lie.
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 3/10 - DEFINE GCE SUPPORT POINTS\n")

M <- 3

B <- array(
  0,
  dim = c(n, k, M)
)

for (i in 1:n) {

  for (j in 1:k) {

    r_ij <- R[i, j]
    a_ij <- A[i, j]

    B[i, j, 1] <- (1 - r_ij) * a_ij
    B[i, j, 2] <- a_ij
    B[i, j, 3] <- (1 + r_ij) * a_ij
  }
}

cat("  Number of support points per cell :", M, "\n")
cat("  Support 1 = (1-r)A\n")
cat("  Support 2 = A\n")
cat("  Support 3 = (1+r)A\n")

# R uses column-major ordering.
idx <- function(i, j) {
  i + (j - 1) * n
}

N <- n * k

cat("  Number of SUT cells :", N, "\n")


# ============================================================
# STEP 4 - FEASIBILITY SCREENING
# ============================================================
#
# Objective:
#   Determine the minimum and maximum feasible value of every
#   cell based on its support points, then perform an aggregate
#   screening of row and column targets.
#
# This is a preliminary screening only. The formal feasibility
# test is performed in STEP 5 using GLPK.
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 4/10 - FEASIBILITY SCREENING\n")

lower_x <- matrix(
  NA_real_,
  n,
  k
)

upper_x <- matrix(
  NA_real_,
  n,
  k
)

for (i in 1:n) {

  for (j in 1:k) {

    lower_x[i, j] <- min(B[i, j, ])
    upper_x[i, j] <- max(B[i, j, ])
  }
}

# Aggregate feasible ranges
row_lower <- rowSums(lower_x)
row_upper <- rowSums(upper_x)

col_lower <- colSums(lower_x)
col_upper <- colSums(upper_x)

row_screen_fail <- (
  row_target < row_lower - 1e-8 |
  row_target > row_upper + 1e-8
)

col_screen_fail <- (
  col_target < col_lower - 1e-8 |
  col_target > col_upper + 1e-8
)

cat(
  "  Rows outside aggregate support bounds    :",
  sum(row_screen_fail),
  "\n"
)

cat(
  "  Columns outside aggregate support bounds :",
  sum(col_screen_fail),
  "\n"
)

if (any(row_screen_fail)) {
  cat(
    "  WARNING: Some row targets lie outside support bounds.\n"
  )
}

if (any(col_screen_fail)) {
  cat(
    "  WARNING: Some column targets lie outside support bounds.\n"
  )
}

cat("  Feasibility screening completed.\n")


# ============================================================
# STEP 5 - FIND FEASIBLE BALANCED MATRIX
#           USING LINEAR PROGRAMMING (GLPK)
# ============================================================
#
# Objective:
#   Find ANY matrix X_feasible satisfying:
#
#       sum_j X_ij = ROW_TARGET_i
#       sum_i X_ij = COL_TARGET_j
#
#   subject to:
#
#       lower_x <= X <= upper_x
#
# The LP objective is zero. Therefore, GLPK is not yet doing
# GCE. It is only finding a feasible point that can be used
# as the starting point for the nonlinear GCE optimization.
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 5/10 - FIND FEASIBLE BALANCED MATRIX (LP / GLPK)\n")

# Construct equality constraint matrix
Aeq <- matrix(
  0,
  nrow = n + k,
  ncol = N
)

# Row constraints
for (i in 1:n) {

  for (j in 1:k) {

    Aeq[
      i,
      idx(i, j)
    ] <- 1
  }
}

# Column constraints
for (j in 1:k) {

  for (i in 1:n) {

    Aeq[
      n + j,
      idx(i, j)
    ] <- 1
  }
}

rhs <- c(
  row_target,
  col_target
)

lb_x <- as.vector(lower_x)
ub_x <- as.vector(upper_x)

cat("  Solving feasibility LP with GLPK...\n")

lp_result <- Rglpk_solve_LP(
  obj = rep(0, N),
  mat = Aeq,
  dir = rep("==", n + k),
  rhs = rhs,
  bounds = list(
    lower = list(
      ind = seq_len(N),
      val = lb_x
    ),
    upper = list(
      ind = seq_len(N),
      val = ub_x
    )
  ),
  max = FALSE
)

if (lp_result$status != 0) {

  stop(
    paste0(
      "GLPK could not find a feasible starting point. ",
      "GLPK status = ",
      lp_result$status,
      ".\n",
      "Check support bounds and row/column targets."
    )
  )
}

X_feasible <- matrix(
  lp_result$solution,
  nrow = n,
  ncol = k
)

# Verify LP solution
row_resid_lp <-
  rowSums(X_feasible) - row_target

col_resid_lp <-
  colSums(X_feasible) - col_target

max_row_resid_lp <-
  max(abs(row_resid_lp))

max_col_resid_lp <-
  max(abs(col_resid_lp))

cat(
  "  GLPK status                         :",
  lp_result$status,
  "\n"
)

cat(
  "  Maximum absolute row residual       :",
  max_row_resid_lp,
  "\n"
)

cat(
  "  Maximum absolute column residual    :",
  max_col_resid_lp,
  "\n"
)

if (
  max_row_resid_lp > 1e-7 ||
  max_col_resid_lp > 1e-7
) {
  stop("GLPK solution failed row/column verification.")
}

cat("  Feasible balanced matrix obtained.\n")


# ============================================================
# STEP 6 - CONSTRUCT FEASIBLE INITIAL PROBABILITIES (P0)
# ============================================================
#
# Objective:
#   Convert the feasible X into probability distributions
#   over the GCE support points.
#
# For each cell:
#
#   X_ij = sum_m p_ijm b_ijm
#
# and:
#
#   sum_m p_ijm = 1
#
# The resulting P0 is a feasible starting point for the
# constrained GCE optimization.
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 6/10 - CONSTRUCT FEASIBLE INITIAL PROBABILITIES (P0)\n")

P0 <- array(
  0,
  dim = c(n, k, M)
)

tol <- 1e-12

for (i in 1:n) {

  for (j in 1:k) {

    b1 <- B[i, j, 1]
    b2 <- B[i, j, 2]
    b3 <- B[i, j, 3]

    x <- X_feasible[i, j]

    # If the support has zero width, all probability
    # is assigned to the initial support.
    if (abs(b3 - b1) < tol) {

      P0[i, j, 1] <- 0
      P0[i, j, 2] <- 1
      P0[i, j, 3] <- 0

    } else {

      # Interpolate between lower and upper support.
      p3 <- (x - b1) / (b3 - b1)

      p3 <- max(
        0,
        min(1, p3)
      )

      P0[i, j, 1] <- 1 - p3
      P0[i, j, 2] <- 0
      P0[i, j, 3] <- p3
    }
  }
}

# Verify that P0 reproduces X_feasible
P0_X <- array(
  0,
  dim = c(n, k, M)
)

for (m in 1:M) {

  P0_X[, , m] <-
    P0[, , m] * B[, , m]
}

X0 <- apply(
  P0_X,
  c(1, 2),
  sum
)

max_P0_X_error <-
  max(abs(X0 - X_feasible))

max_P0_row_error <-
  max(abs(rowSums(X0) - row_target))

max_P0_col_error <-
  max(abs(colSums(X0) - col_target))

cat(
  "  Maximum |X0 - X_feasible| :",
  max_P0_X_error,
  "\n"
)

cat(
  "  Maximum row residual      :",
  max_P0_row_error,
  "\n"
)

cat(
  "  Maximum column residual   :",
  max_P0_col_error,
  "\n"
)

if (max_P0_X_error > 1e-8) {
  stop("P0 does not reproduce the feasible X.")
}

cat("  Feasible starting probability distribution P0 obtained.\n")


# ============================================================
# STEP 7 - SPECIFY PRIOR PROBABILITIES & GCE OBJECTIVE
# ============================================================
#
# Objective:
#   Specify the prior probability distribution Q and define
#   the GCE objective function.
#
# In this version:
#
#   Q = (0.025, 0.950, 0.025)
#
# This is a spike prior that places most prior probability on
# the initial estimate A.
#
# GCE minimizes:
#
#   D(P||Q)
#   = sum p_ijm log(p_ijm / q_ijm)
#
# subject to the balancing constraints in STEP 8.
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 7/10 - PRIOR PROBABILITIES & GCE OBJECTIVE\n")

# Spike prior
Q <- array(
  0,
  dim = c(n, k, M)
)

Q[, , 1] <- 0.025
Q[, , 2] <- 0.950
Q[, , 3] <- 0.025

P0_vec <- as.vector(P0)
Q_vec  <- as.vector(Q)

# Numerical floor used ONLY inside logarithms.
# Exact zero probabilities remain allowed.
log_floor <- 1e-15

# GCE / KL objective
gce_objective <- function(p) {

  p_safe <- p

  p_safe[p_safe < log_floor] <-
    log_floor

  sum(
    p_safe *
      log(p_safe / Q_vec)
  )
}

# Analytical gradient
gce_gradient <- function(p) {

  p_safe <- p

  p_safe[p_safe < log_floor] <-
    log_floor

  log(p_safe / Q_vec) + 1
}

cat("  Prior probabilities:\n")
cat("    Lower support :", 0.025, "\n")
cat("    Initial       :", 0.950, "\n")
cat("    Upper support :", 0.025, "\n")
cat("  GCE objective = KL divergence / cross-entropy criterion.\n")


# ============================================================
# STEP 8 - GCE CONSTRAINED OPTIMIZATION
#           USING SLSQP / NLOPTR
# ============================================================
#
# Objective:
#   Find posterior probabilities P that are as close as possible
#   to the prior Q while satisfying all balancing constraints.
#
# Constraints:
#
#   A. For every cell:
#        sum_m p_ijm = 1
#
#   B. Row constraints:
#        sum_j sum_m p_ijm b_ijm = ROW_TARGET_i
#
#   C. Column constraints:
#        sum_i sum_m p_ijm b_ijm = COL_TARGET_j
#
# Probability bounds:
#
#        0 <= p_ijm <= 1
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 8/10 - GCE CONSTRAINED OPTIMIZATION (SLSQP)\n")

n_constraints <- N + n + k
n_variables <- N * M

# ------------------------------------------------------------
# Equality constraints
# ------------------------------------------------------------

eval_g_eq <- function(p) {

  P <- array(
    p,
    dim = c(n, k, M)
  )

  g <- numeric(
    n_constraints
  )

  counter <- 1

  # A. Probability sum constraints
  for (i in 1:n) {

    for (j in 1:k) {

      g[counter] <-
        sum(P[i, j, ]) - 1

      counter <- counter + 1
    }
  }

  # B. Row constraints
  for (i in 1:n) {

    value <- 0

    for (j in 1:k) {

      for (m in 1:M) {

        value <-
          value +
          P[i, j, m] *
          B[i, j, m]
      }
    }

    g[counter] <-
      value - row_target[i]

    counter <- counter + 1
  }

  # C. Column constraints
  for (j in 1:k) {

    value <- 0

    for (i in 1:n) {

      for (m in 1:M) {

        value <-
          value +
          P[i, j, m] *
          B[i, j, m]
      }
    }

    g[counter] <-
      value - col_target[j]

    counter <- counter + 1
  }

  g
}

# ------------------------------------------------------------
# Equality constraint Jacobian
# ------------------------------------------------------------

eval_jac_g_eq <- function(p) {

  J <- matrix(
    0,
    nrow = n_constraints,
    ncol = n_variables
  )

  counter <- 1

  # A. Probability sum constraints
  for (i in 1:n) {

    for (j in 1:k) {

      base <- idx(i, j)

      for (m in 1:M) {

        variable_index <-
          base + (m - 1) * N

        J[
          counter,
          variable_index
        ] <- 1
      }

      counter <- counter + 1
    }
  }

  # B. Row constraints
  for (i in 1:n) {

    for (j in 1:k) {

      base <- idx(i, j)

      for (m in 1:M) {

        variable_index <-
          base + (m - 1) * N

        J[
          counter,
          variable_index
        ] <- B[i, j, m]
      }
    }

    counter <- counter + 1
  }

  # C. Column constraints
  for (j in 1:k) {

    for (i in 1:n) {

      base <- idx(i, j)

      for (m in 1:M) {

        variable_index <-
          base + (m - 1) * N

        J[
          counter,
          variable_index
        ] <- B[i, j, m]
      }
    }

    counter <- counter + 1
  }

  J
}

# ------------------------------------------------------------
# Starting point and bounds
# ------------------------------------------------------------

x0 <- as.vector(P0)

# P0 may contain exact zeros, therefore:
# lower probability bound = 0
lower_p <- rep(
  0,
  n_variables
)

upper_p <- rep(
  1,
  n_variables
)

if (
  any(x0 < lower_p - 1e-14)
) {
  stop(
    "Starting point x0 violates lower probability bounds."
  )
}

if (
  any(x0 > upper_p + 1e-14)
) {
  stop(
    "Starting point x0 violates upper probability bounds."
  )
}

# Verify constraints at P0
g0 <- eval_g_eq(x0)

max_initial_constraint_error <-
  max(abs(g0))

cat(
  "  Number of optimization variables :",
  n_variables,
  "\n"
)

cat(
  "  Number of equality constraints   :",
  n_constraints,
  "\n"
)

cat(
  "  Maximum P0 constraint residual   :",
  max_initial_constraint_error,
  "\n"
)

if (
  max_initial_constraint_error > 1e-7
) {
  stop(
    "P0 is not feasible for the GCE constraints."
  )
}

# ------------------------------------------------------------
# Run GCE optimization
# ------------------------------------------------------------

cat("  Running SLSQP optimization...\n")

opts <- list(
  algorithm = "NLOPT_LD_SLSQP",
  xtol_rel = 1e-10,
  xtol_abs = 1e-10,
  ftol_rel = 1e-10,
  ftol_abs = 1e-10,
  maxeval = 20000,
  print_level = 1
)

gce_result <- nloptr(
  x0 = x0,
  eval_f = gce_objective,
  eval_grad_f = gce_gradient,
  lb = lower_p,
  ub = upper_p,
  eval_g_eq = eval_g_eq,
  eval_jac_g_eq = eval_jac_g_eq,
  opts = opts
)

cat(
  "  NLOpt status    :",
  gce_result$status,
  "\n"
)

cat(
  "  NLOpt message   :",
  gce_result$message,
  "\n"
)

cat(
  "  GCE objective   :",
  gce_result$objective,
  "\n"
)

if (
  gce_result$status < 0
) {

  warning(
    paste0(
      "NLOpt returned a negative status: ",
      gce_result$status,
      ". Inspect convergence diagnostics."
    )
  )
}

# Posterior probabilities
P_vec <- gce_result$solution

P <- array(
  P_vec,
  dim = c(n, k, M)
)


# ============================================================
# STEP 9 - DERIVE & VERIFY BALANCED SUT
# ============================================================
#
# Objective:
#   Convert posterior probabilities P and support values B into
#   the final balanced SUT:
#
#       X_ij = sum_m p_ijm b_ijm
#
# Then verify that:
#
#       sum_j X_ij = ROW_TARGET_i
#       sum_i X_ij = COL_TARGET_j
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 9/10 - DERIVE & VERIFY BALANCED SUT\n")

X <- matrix(
  0,
  n,
  k
)

for (m in 1:M) {

  X <-
    X +
    P[, , m] *
    B[, , m]
}

row_residual <-
  rowSums(X) -
  row_target

col_residual <-
  colSums(X) -
  col_target

max_row_residual <-
  max(abs(row_residual))

max_col_residual <-
  max(abs(col_residual))

cat(
  "  Maximum absolute row residual    :",
  max_row_residual,
  "\n"
)

cat(
  "  Maximum absolute column residual :",
  max_col_residual,
  "\n"
)

if (
  max_row_residual > 1e-6 ||
  max_col_residual > 1e-6
) {

  warning(
    paste0(
      "Final solution is not balanced within 1e-6. ",
      "Max row residual = ",
      max_row_residual,
      "; Max column residual = ",
      max_col_residual
    )
  )

} else {

  cat(
    "  SUCCESS: SUT is balanced within 1e-6.\n"
  )
}


# ============================================================
# STEP 10 - UNCERTAINTY, RELIABILITY & OUTPUT DIAGNOSTICS
# ============================================================
#
# Objective:
#   Quantify the information contained in the posterior
#   probability distribution and prepare diagnostic outputs.
#
# Cell-level measures:
#
#   Variance
#   Standard deviation
#   Normalized Shannon entropy
#   Entropy-based reliability
#   Absolute change
#   Relative change
#
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("STEP 10/10 - UNCERTAINTY, RELIABILITY & OUTPUT DIAGNOSTICS\n")

# ------------------------------------------------------------
# 10A. Variance, SD, entropy and reliability
# ------------------------------------------------------------

variance_X <- matrix(
  0,
  n,
  k
)

sd_X <- matrix(
  0,
  n,
  k
)

entropy_X <- matrix(
  0,
  n,
  k
)

reliability_X <- matrix(
  0,
  n,
  k
)

for (i in 1:n) {

  for (j in 1:k) {

    p <- P[i, j, ]
    b <- B[i, j, ]

    # Expected value
    x <- sum(
      p * b
    )

    # Variance
    second_moment <-
      sum(
        p * b^2
      )

    variance <-
      second_moment - x^2

    variance <-
      max(
        0,
        variance
      )

    variance_X[i, j] <-
      variance

    sd_X[i, j] <-
      sqrt(variance)

    # Shannon entropy
    p_safe <- p

    p_safe[
      p_safe < log_floor
    ] <- log_floor

    entropy <-
      -sum(
        p *
          log(p_safe)
      )

    normalized_entropy <-
      entropy /
      log(M)

    entropy_X[i, j] <-
      normalized_entropy

    # Entropy-based reliability / concentration
    reliability_X[i, j] <-
      1 - normalized_entropy
  }
}

# Summary diagnostics
mean_entropy <-
  mean(entropy_X)

median_entropy <-
  median(entropy_X)

mean_reliability <-
  mean(reliability_X)

median_reliability <-
  median(reliability_X)

cat(
  "  Mean normalized entropy   :",
  mean_entropy,
  "\n"
)

cat(
  "  Median normalized entropy :",
  median_entropy,
  "\n"
)

cat(
  "  Mean reliability          :",
  mean_reliability,
  "\n"
)

cat(
  "  Median reliability        :",
  median_reliability,
  "\n"
)

# ------------------------------------------------------------
# 10B. Cell-level changes
# ------------------------------------------------------------

change_X <-
  X - A

relative_change <-
  matrix(
    NA_real_,
    n,
    k
  )

for (i in 1:n) {

  for (j in 1:k) {

    if (
      abs(A[i, j]) > 1e-12
    ) {

      relative_change[i, j] <-
        (
          X[i, j] -
          A[i, j]
        ) /
        abs(A[i, j])

    } else {

      relative_change[i, j] <-
        NA_real_
    }
  }
}

# ------------------------------------------------------------
# 10C. Prepare output tables
# ------------------------------------------------------------

A_df <-
  as.data.frame(A)

X_df <-
  as.data.frame(X)

change_df <-
  as.data.frame(change_X)

relative_change_df <-
  as.data.frame(relative_change)

row_target_df <- data.frame(
  Row = 1:n,
  Target = row_target,
  Balanced = rowSums(X),
  Residual = row_residual
)

col_target_df <- data.frame(
  Column = 1:k,
  Target = col_target,
  Balanced = colSums(X),
  Residual = col_residual
)

variance_df <-
  as.data.frame(variance_X)

sd_df <-
  as.data.frame(sd_X)

entropy_df <-
  as.data.frame(entropy_X)

reliability_df <-
  as.data.frame(reliability_X)

R_df <-
  as.data.frame(R)

# ------------------------------------------------------------
# 10D. Cell diagnostics
# ------------------------------------------------------------

cell_diag <- expand.grid(
  Row = 1:n,
  Column = 1:k
)

cell_diag$Initial_A <-
  as.vector(A)

cell_diag$Balanced_X <-
  as.vector(X)

cell_diag$Change <-
  as.vector(change_X)

cell_diag$Relative_Change <-
  as.vector(relative_change)

cell_diag$Variance <-
  as.vector(variance_X)

cell_diag$SD <-
  as.vector(sd_X)

cell_diag$Entropy <-
  as.vector(entropy_X)

cell_diag$Reliability <-
  as.vector(reliability_X)

cell_diag$R <-
  as.vector(R)

# Posterior probabilities
for (m in 1:M) {
  cell_diag[[paste0("P", m)]] <- as.vector(P[, , m])
}

# Support values
for (m in 1:M) {
  cell_diag[[paste0("B", m)]] <- as.vector(B[, , m])
}

# ------------------------------------------------------------
# 10E. Summary
# ------------------------------------------------------------

summary_df <- data.frame(

  Step = c(
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10"
  ),

  Stage = c(
    "Input & data preparation",
    "Orientation & consistency check",
    "Define GCE support points",
    "Feasibility screening",
    "Feasible balanced matrix using LP/GLPK",
    "Feasible initial probabilities P0",
    "Prior probabilities & GCE objective",
    "GCE constrained optimization",
    "Balanced SUT derivation & verification",
    "Uncertainty, reliability & diagnostics"
  ),

  Status = c(
    "Completed",
    "Completed",
    "Completed",
    "Completed",
    ifelse(lp_result$status == 0, "Completed", "Failed"),
    ifelse(max_P0_X_error <= 1e-8, "Completed", "Failed"),
    "Completed",
    ifelse(gce_result$status >= 0, "Completed", "Check"),
    ifelse(
      max_row_residual <= 1e-6 &&
      max_col_residual <= 1e-6,
      "Balanced",
      "Check"
    ),
    "Completed"
  )
)

model_summary_df <- data.frame(

  Metric = c(
    "Rows",
    "Columns",
    "Number of cells",
    "Number of support points",
    "GLPK status",
    "NLOpt status",
    "GCE objective",
    "Maximum absolute row residual",
    "Maximum absolute column residual",
    "Mean normalized entropy",
    "Median normalized entropy",
    "Mean reliability",
    "Median reliability"
  ),

  Value = c(
    n,
    k,
    N,
    M,
    lp_result$status,
    gce_result$status,
    gce_result$objective,
    max_row_residual,
    max_col_residual,
    mean_entropy,
    median_entropy,
    mean_reliability,
    median_reliability
  )
)

# ------------------------------------------------------------
# 10F. Export results
# ------------------------------------------------------------

write_xlsx(

  list(

    PROCEDURE_10_STEPS = summary_df,

    MODEL_SUMMARY = model_summary_df,

    INITIAL_A = A_df,

    BALANCED_X = X_df,

    CHANGE = change_df,

    RELATIVE_CHANGE = relative_change_df,

    ROW_CHECK = row_target_df,

    COL_CHECK = col_target_df,

    VARIANCE = variance_df,

    SD = sd_df,

    ENTROPY = entropy_df,

    RELIABILITY = reliability_df,

    R = R_df,

    CELL_DIAGNOSTICS = cell_diag
  ),

  output_file
)

cat("\n============================================================\n")
cat("10-STEP GCE SUT BALANCING COMPLETED\n")
cat("============================================================\n")
cat("Output file:", output_file, "\n")
cat("Maximum row residual   :", max_row_residual, "\n")
cat("Maximum column residual:", max_col_residual, "\n")
cat("Mean reliability       :", mean_reliability, "\n")
cat("============================================================\n")
