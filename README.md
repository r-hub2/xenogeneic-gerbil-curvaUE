# curvaUE 0.3.2 — Experimental Unit Sizing by Bivariate Curvature

`curvaUE` is an R package for analyzing agricultural uniformity trials. The
**public API, help pages, messages, plots, output columns, and execution scripts are in English**.
The project/package name `curvaUE` is retained.

## Input format

Supply a numeric yield grid: one row per physical row and one column per
physical column of basic units. When importing CSV, the first line contains
**column headers only**; do not include a row-number column.

```csv
C1,C2,C3,C4
390,404,308,354
460,310,362,400
320,474,424,452
```

```r
library(curvaUE)
M <- read_trial_matrix("path/to/trial.csv")
validate_trial_matrix(M)
head(as_long_trial_data(M))
```

The number of rows and columns is detected automatically; grids need not be square.
The package includes rice (20x20), maize (20x20), Nambi common bean (10x20)
and cowpea (10x20): `example_trial("rice")`, etc.

## Main analysis

```r
M <- example_trial("rice")
result <- analyze_uniformity_trial(
  trial = M, unit_width_m = 1, unit_length_m = 1,
  min_plots = 2, criterion = "both", smith_method = "smith1938"
)
result$dimensions
result$smith$b_prime
result$surface[c("a", "m", "n")]
result$solution
```

`result$configurations` lists all exact rectangular/square configurations with
at least **two** independent experimental units, including 200-BU plots with
only two replicates in the 20x20 example.

The main Smith estimate fits
`log(unit_variance) ~ log(area)` by weighted least squares with weights
`n_plots - 1`, a free intercept, and the conventional sample unit variance.
It reports the *finite-field* exponent `b_prime` without an infinite-field correction.
The CV surface is fitted by **unweighted** nonlinear least squares:
`CV = a * width^(-m) * length^(-n)`.
Curvatures use the number of **basic units (BU)**; dimensions are converted
later to physical metres and square metres. The CV is expressed in percentage points.

## Figures

```r
cfg <- result$configurations
surface <- result$surface
plot_trial_heatmap(M)
plot_cv_profiles(cfg, surface)
plot_cv_contours(surface, cfg)
plot_curvature_sections(surface, configurations = cfg)
plot_multicrop_curvature()
```

## Installing and validating

From the **unpacked delivery parent folder**, run `RUN_AND_VALIDATE_CURVAUE.R`
with **Source** in RStudio. The script forces installation of the local package,
loads the four data grids, compares results to independent Python reference
values, runs `testthat`, and exports CSV files and figures into `curvaUE_Results/`.

Run `ANALYZE_MY_MATRIX.R` to analyze your own CSV using the same API.
To check installation separately, run `CHECK_INPUT_DATA.R`.
Before public distribution, run `devtools::check("curvaUE")` or
`R CMD check` in a local R installation.

## Compatibility

Old Spanish function names are available temporarily as aliases and retain
Spanish argument and output field names for legacy scripts. **Use the English
functions and fields in all new work.** Legacy crop identifiers remain in the
original `.rda` files, while the recommended `example_trial()` API and example
CSV names are in English.

## Scope

The methods are methodological geometric criteria, not by themselves an
agronomic optimum under every cost or precision requirement. The derived
continuous dimensions must be considered together with admissible plots and
experimental design requirements. The example plots from different
configurations share original observations.

See `VALIDATION/api_name_mapping.csv` in the delivery folder for the
18 function-name mappings from the previous release.
