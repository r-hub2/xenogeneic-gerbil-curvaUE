# Changelog

## 0.3.2 — CRAN-check compatibility

- Fix 3D overlay by using `grDevices::trans3d()`, the exported function.
- Use the `.data` pronoun in ggplot2 aesthetics to avoid global-binding notes.
- Qualify `stats::setNames()` in multicrop plotting.
- Wrap usage signatures in four Rd files; add persistent roxygen `@usage` declarations.
- Update roxygen metadata to 8.1.1 and declare `rlang` as a direct import.
- Statistical methods, original datasets, public signatures, and numerical outputs are unchanged.


## 0.3.1 — RStudio test corrections

- Update two CSV import tests to compare un-named numeric values while asserting
  that column headers are preserved and row labels remain absent.
- Remove the deprecated ggplot2 `geom_label(label.size=)` argument and require
  a compatible ggplot2 version.
- Include package-test failures in staged diagnostic logging.
- Numerical methods, four trial matrices, and results are unchanged.


## 0.3.0 — English-first public API

- English function names, arguments, output fields, messages, legends, examples, tests, and CSV names.
- The underlying numerical methods and four original yield grids are unchanged.
- The 1938 Smith estimator remains weighted least squares on log conventional unit variance, using M-1 weights.
- Gaussian and simultaneous sectional curvature operate in basic-unit coordinates before physical conversion.
- All valid exact configurations with at least two experimental units are retained.
- Deprecated Spanish function-name aliases are temporarily retained; returned fields use English names.
- No real R interpreter is available in the build environment: runtime checks must be performed in RStudio.
