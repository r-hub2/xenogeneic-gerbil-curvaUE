#' Describe a uniformity trial
#'
#' Calculates descriptive statistics of basic-unit yields.
#' @param trial Numeric yield matrix.
#' @return A one-row data frame of summary statistics.
#' @export
describe_trial <- function(trial) {
  M <- .validate_trial(trial)
  v <- as.numeric(M)
  m <- mean(v)
  de <- .sd_value(v, "sample")
  dp <- .sd_value(v, "population")
  data.frame(
    n_ub      = length(v),
    rows     = nrow(M),
    columns  = ncol(M),
    mean     = m,
    std_dev      = de,
    cv_ub     = 100 * de / m,
    minimum    = min(v),
    maximum    = max(v),
    skewness = if (dp > 0) mean((v - m)^3) / dp^3 else NA_real_,
    excess_kurtosis  = if (dp > 0) mean((v - m)^4) / dp^4 - 3 else NA_real_
  )
}
