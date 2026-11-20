# GCE_SUT_balancing

R code and data for the paper **"A Step-by-Step Guide to Implementing Generalized Cross-Entropy (GCE) for Supply and Use Tables (SUT) Automatic Balancing"** (Usep Nugraha, BPS-Statistics Indonesia).

The code balances a Supply and Use Table (SUT) with the Generalized Cross-Entropy (GCE) method, following the eight-step procedure described in the paper. Step 6 solves the GCE problem in its dual (Lagrangian) form with Newton's method.

## Contents

| File | Description |
|---|---|
| `GCE_SUT_Balancing.R` | Main script: the eight-step GCE balancing procedure (Steps 1–8 of the paper). |
| `GCE_SUT_Input.xlsx` | Input workbook for the worked example: sheets `A` (initial matrix), `R` (support-points matrix), `ROW_TARGET`, and `COL_TARGET`. |
| `GCE_SUT_Results.xlsx` | Output of `GCE_SUT_Balancing.R`: balanced matrix, row and column checks, posterior probabilities, entropy, reliability, and Lagrange multipliers. |
| `GRAS_Comparison.R` | Balances the same example with GRAS and compares the results with GCE (Table 2 of the paper). |
| `GRAS_Comparison_Results.xlsx` | Output of `GRAS_Comparison.R`. |
| `Sensitivity_Analysis.R` | Re-estimates the example under alternative support points and priors (Table 3 of the paper). |
| `Sensitivity_Results.xlsx` | Output of `Sensitivity_Analysis.R`. |

## Requirements

- R (tested with R 4.4.1)
- Packages: `readxl`, `writexl`, `MASS` (installed automatically if missing)

## How to reproduce the results in the paper

Set the working directory to this folder and run the scripts in this order:

```r
source("GCE_SUT_Balancing.R")    # Figure 5, Table 1, Steps 6-8 results
source("GRAS_Comparison.R")      # Table 2
source("Sensitivity_Analysis.R") # Table 3
```

## Using your own data

Replace the four sheets in `GCE_SUT_Input.xlsx` with your own data, keeping the same layout:

- `A`: initial matrix, with the supply-table entries multiplied by −1 so that each product row sums to zero when balanced;
- `R`: one relative adjustment parameter *r* per cell (0 = fixed cell);
- `ROW_TARGET`: target row totals;
- `COL_TARGET`: target column totals.

The script stops with a message if the targets cannot be reached within the support bounds (Step 4) or if the optimization does not converge (Step 6), which usually means that `R` or the targets need to be revised.

## Citation

If you use this code, please cite the paper and the archived version of this repository (see `CITATION.cff`).

## License

See `LICENSE`.
