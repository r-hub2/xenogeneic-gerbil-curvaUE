# curvaUE 0.3.3 — Experimental-Unit Dimensions from Bivariate CV Surfaces

`curvaUE` analyzes agricultural uniformity trials to estimate continuous experimental-unit dimensions with **three mathematically distinct criteria** on a fitted coefficient-of-variation (CV) surface. The public API and help pages are in English. Legacy Spanish aliases are retained for older scripts.

## Input

Supply a numeric yield matrix (physical rows = matrix rows, physical columns = matrix columns). In a CSV, the first line contains **column headers**, but no separate row-number or index column:

```csv
C1,C2,C3,C4
390,404,308,354
460,310,362,400
320,474,424,452
```

```r
library(curvaUE)
trial <- read_trial_matrix("path/to/trial.csv")
validate_trial_matrix(trial)
```

Grids may be rectangular. Four real example matrices are included: rice and maize (20 x 20 basic units each), Nambí common bean and cowpea (10 x 20 basic units each). For example, `example_trial("rice")` returns the rice grid.

## Mathematical criteria

The nonlinear CV surface is fitted by unweighted nonlinear least squares:

\[ CV(x,y) = a x^{-m}y^{-n}, \qquad a,m,n > 0, \]

with width `x` and length `y` measured in **basic units (BU)**, and CV in percent. The three criteria are:

1. **Gaussian curvature:** the point of maximum Gaussian curvature of the fitted surface.
2. **Simultaneous sectional curvature:** the intersection of the maxima of the two coordinate-section curvatures.
3. **Coordinate-wise marginal efficiency:** the point satisfying `m * CV(x,y) / x = target_slope` and `n * CV(x,y) / y = target_slope`. The target is a **positive CV decrease in percentage points per additional basic unit**, applied independently to width and length. The point has `y/x = n/m` and is not, in general, a maximum-curvature point. It is also **not** a marginal-efficiency optimum defined per unit of total area or cost.

The methods yield continuous model-based solutions. They do not automatically restrict dimensions to admissible discrete plot geometries. Compare the results with `result$configurations` and with experimental-design and agronomic constraints. Any solution beyond the observed field requires extrapolation.

## Main analysis

```r
trial <- example_trial("rice")
result <- analyze_uniformity_trial(
  trial = trial,
  unit_width_m = 1, unit_length_m = 1,
  min_plots = 2,
  criterion = "all",              # gaussian, sectional, marginal
  smith_method = "smith1938",
  target_slope = 1               # CV percentage points per BU
)
result$dimensions
result$smith$b_prime
result$surface[c("a", "m", "n")]
result$solution
```

The default `criterion = "both"` still returns **only Gaussian and sectional curvature**, preserving established workflows. Alternatively select `"gaussian"`, `"sectional"`, `"marginal"` or `"all"`. The new `target_slope` argument was **appended after `smith_method`**, so existing positional calls remain compatible.

Results are calculated in BU coordinates, then converted to metres and square metres in `result$solution`. For the marginal criterion, `target_slope` always refers to BU dimensions, not physical metres. The code enumerates every exact rectangular or square partition that leaves at least two experimental units (for 20 x 20 grids, including 200-BU configurations with two units). The Smith (1938) method uses weighted regression of the log of the conventional unit variance on log area, with weights `n_plots - 1` and a free intercept. Its `b_prime` estimate applies to the observed finite field; it is not corrected to infinite-field conditions.

## Graphics: three criteria

```r
cfg <- result$configurations
surface <- result$surface
plot_trial_heatmap(trial)
plot_cv_profiles(cfg, surface)
plot_cv_contours(surface, cfg, target_slope = 1)
plot_cv_surface(surface, cfg, target_slope = 1)
plot_curvature_sections(surface, criterion = "all",
                        configurations = cfg, target_slope = 1)
plot_multicrop_curvature(target_slope = 1)
```

`plot_cv_contours()` and `plot_cv_surface()` show **all three points**. `plot_curvature_sections()` supports all three separately (`criterion = "marginal"`) or overlaid (`"all"`). The multi-crop figure shows both sectional-curvature loci plus symbols for all three criteria; its dashed guides and numerical labels continue to refer **only to the sectional solution**. Changing `target_slope` changes the marginal solution, not either curvature solution.

### Marginal criterion independently

```r
marginal_efficiency_2d(a = 22.95414, m = 0.60814, n = 0.23216,
                       target_slope = 1)
```

## Install, test and prepare for CRAN

Install from the directory containing `DESCRIPTION`:

```r
install.packages("devtools")
devtools::install("path/to/curvaUE", upgrade = "never")
library(curvaUE)
```

Two example scripts are included within the installed package:

```r
source(system.file("examples", "CHECK_SMITH_1938.R", package = "curvaUE"))
source(system.file("examples", "MIN_TWO_PLOTS_EXAMPLE.R", package = "curvaUE"))
```

Before CRAN submission, run from a checkout or extracted directory:

```r
# Requires an installed LaTeX distribution to build the manual.
devtools::check("path/to/curvaUE", args = "--as-cran",
                remote = TRUE, manual = TRUE, document = FALSE,
                error_on = "never")
```

See `tests/testthat/` for numerical tests. Cross-platform checks for version 0.3.2 **do not validate the added functionality of version 0.3.3**; run fresh checks on 0.3.3 prior to submission.

## Compatibility and limitations

The original Gaussian and sectional algorithms, Smith implementation, and the four example yield grids are unchanged from 0.3.2. Spanish-language function aliases remain for older scripts. Source figures and numerical methods are intended for methodological comparison, not as universal cost-optimal agronomic recommendations. Grouped experimental units reuse original uniformity-trial observations.
