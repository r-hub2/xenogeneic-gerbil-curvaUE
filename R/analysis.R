#' Analyze an experimental uniformity trial
#'
#' Integrates input checks, configurations, weighted Smith estimate, CV surface,
#' and the selected curvature or marginal-efficiency solution(s).
#' @param trial Numeric yield matrix.
#' @param unit_width_m,unit_length_m Physical dimensions of one basic unit, in metres.
#' @param sd sample or population for descriptive CV.
#' @param min_plots At least 2 experimental units per configuration.
#' @param criterion both, gaussian, sectional, marginal, or all. "both" returns
#'   the two curvature solutions; "all" adds the marginal-efficiency solution.
#' @param smith_method Smith estimator method; default smith1938.
#' @param target_slope Positive target slope for the marginal-efficiency
#'   criterion, used when criterion is "marginal" or "all"; default 1.
#'   Measured in CV percentage points per basic unit for each coordinate.
#' @return A list with descriptives, dimensions, long_data, smith, surface, configurations, and solution.
#' @usage
#' analyze_uniformity_trial(
#'   trial, unit_width_m = 1, unit_length_m = 1,
#'   sd = "sample", min_plots = 2L,
#'   criterion = c("both", "gaussian", "sectional", "marginal", "all"),
#'   smith_method = c("smith1938", "nls", "log", "unweighted_log"),
#'   target_slope = 1
#' )
#' @export
analyze_uniformity_trial <- function(trial, unit_width_m = 1, unit_length_m = 1,
                                   sd = "sample", min_plots = 2L,
                                   criterion = c("both", "gaussian", "sectional", "marginal", "all"),
                                   smith_method = c("smith1938", "nls", "log", "unweighted_log"),
                                   target_slope = 1) {
  criterion <- match.arg(criterion)
  smith_method <- match.arg(smith_method)
  M <- .validate_trial(trial)
  cfg <- plot_configurations(M, unit_width_m, unit_length_m, sd, min_plots)
  smith <- fit_smith(cfg, method = smith_method)
  sup <- fit_cv_surface(cfg)
  sol <- switch(criterion,
    gaussian = max_gaussian_curvature(sup$a, sup$m, sup$n),
    sectional = simultaneous_sectional_curvature(sup$a, sup$m, sup$n),
    marginal = marginal_efficiency_2d(sup$a, sup$m, sup$n, target_slope),
    both = rbind(max_gaussian_curvature(sup$a, sup$m, sup$n),
                 simultaneous_sectional_curvature(sup$a, sup$m, sup$n)),
    all = rbind(max_gaussian_curvature(sup$a, sup$m, sup$n),
                simultaneous_sectional_curvature(sup$a, sup$m, sup$n),
                marginal_efficiency_2d(sup$a, sup$m, sup$n, target_slope)))
  sol$width_ub <- sol$width
  sol$length_ub <- sol$length
  sol$area_ub <- sol$area
  sol$width <- sol$width_ub * unit_width_m
  sol$length <- sol$length_ub * unit_length_m
  sol$area <- sol$width * sol$length
  sol$area_m2 <- sol$area
  list(descriptives = describe_trial(M),
       dimensions = validate_trial_matrix(M),
       long_data = as_long_trial_data(M), smith = smith,
       surface = sup, configurations = cfg, solution = sol,
       basic_unit_dimensions_m = c(width = unit_width_m, length = unit_length_m))
}
