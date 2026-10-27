**Generalized Cross-Entropy (GCE) for Supply and Use Table Balancing**

Overview

This repository provides an R implementation of the Generalized Cross-Entropy (GCE) approach for balancing Supply and Use Tables (SUTs).

The script presents the GCE balancing procedure in 10 sequential steps, from input data preparation to the final balanced SUT and cell-level diagnostics.

The repository is intended for researchers and practitioners interested in applying GCE to SUT balancing in a transparent and reproducible way.

Repository Contents

The repository contains three main files:

File	Description
GCE_SUT_Input.xlsx	Input data for the GCE balancing procedure
GCE_SUT_Balancing.R	R script implementing the 10-step GCE balancing procedure
GCE_SUT_Result.xlsx	Output of the GCE balancing procedure

Input File

The GCE_SUT_Input.xlsx file contains four worksheets:

* A – Initial, unbalanced SUT matrix.
* R – Cell-specific adjustment or reliability information.
* ROW_TARGET – Target totals for each row.
* COL_TARGET – Target totals for each column.

The script automatically checks the dimensions and consistency of these inputs before performing the balancing procedure.

GCE Balancing Procedure

The R script implements the following 10 steps:

1. Input and Data Preparation
    Reads and prepares the input data.
2. Orientation and Consistency Check
    Checks matrix dimensions, target vectors, missing values, and consistency of row and column targets.
3. Definition of GCE Support Points
    Defines the support points used to represent possible cell adjustments.
4. Prior Probabilities
    Specifies the prior probability distribution for the support points.
5. Construction of the GCE Optimization Problem
    Sets up the optimization problem using the initial SUT and balancing constraints.
6. Feasibility and Adjustment Bounds
    Defines and checks the permissible adjustment range for each cell.
7. GCE Estimation
    Estimates the posterior probabilities and cell adjustments.
8. Construction of the Balanced SUT
    Applies the estimated adjustments to obtain the balanced SUT.
9. Balancing Diagnostics
    Checks whether the resulting SUT satisfies the row and column targets.
10. Cell-Level Diagnostic Output
    Produces detailed information on cell adjustments, support values, and posterior probabilities.

How to Run

1. Install R

Install R and, optionally, RStudio.

2. Place the Files in the Same Folder

Make sure the following files are in the same working directory:

GCE_SUT_Input.xlsx
GCE_SUT_Balancing.R

3. Run the R Script

Open the R script in R or RStudio and run:

source("GCE_SUT_Balancing.R")

The script will read the input workbook, perform the 10-step GCE balancing procedure, and generate the output workbook.

4. Check the Result

The resulting file will be saved as:

GCE_SUT_Result.xlsx

The output contains the balanced SUT and diagnostic information that can be used to evaluate the balancing results.

Required R Packages

The script uses the following R packages:

* readxl
* writexl
* Rglpk
* nloptr

The script checks for the required packages and installs them if they are not available.

Methodological Background

The implementation is based on the Generalized Cross-Entropy (GCE) approach for constrained estimation and its application to Supply and Use Table balancing.

The approach allows the balancing process to incorporate information about cell reliability and adjustment bounds and provides cell-level information about the resulting adjustments.

The implementation is designed to make the GCE procedure easier to understand, reproduce, and adapt for different SUT datasets.

Notes

The example input and output files provided in this repository are intended to demonstrate the implementation.

For applications to other SUT datasets, users should carefully review and adjust:

* the input SUT;
* row and column targets;
* cell-specific adjustment information;
* support points;
* prior probabilities; and
* optimization settings.

The choice of these parameters can affect the resulting balanced SUT.

Citation

If you use this code in your research, please cite the relevant methodological literature on Generalized Cross-Entropy (GCE) and SUT balancing, as well as this repository.

License

This repository is provided for research and educational purposes. Please refer to the repository license for the applicable terms of use.
