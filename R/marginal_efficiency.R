#' Experimental-unit dimensions by two-dimensional marginal efficiency
#'
#' Estimates the width and length of the experimental unit on the fitted
#' coefficient-of-variation surface by a coordinate-wise marginal-efficiency
#' criterion, as a closed-form alternative to the curvature criteria.
#'
#' @param a,m,n Positive parameters of the fitted coefficient-of-variation
#'   surface \eqn{CV(x, y) = a\,x^{-m}\,y^{-n}}, with \code{x} the width and
#'   \code{y} the length in basic units.
#' @param target_slope Positive target slope: the decrease in the coefficient
#'   of variation, in percentage points, per additional basic unit of the
#'   dimension considered. The same target is applied to width and length.
#'   Defaults to 1.
#' @details The marginal decreases of the surface are
#'   \eqn{-\partial CV / \partial x = m\,CV / x} and
#'   \eqn{-\partial CV / \partial y = n\,CV / y}. Setting both equal to
#'   \code{target_slope} yields a unique positive solution with closed form
#'   \eqn{x = (a\,m^{\,n+1} / (\tau\,n^{n}))^{1/(m+n+1)}},
#'   \eqn{y = (a\,n^{\,m+1} / (\tau\,m^{m}))^{1/(m+n+1)}} and
#'   length-to-width ratio \eqn{n/m}. This is a marginal criterion by
#'   coordinates, not a geometric-curvature criterion, and the result depends
#'   on the scale in which the coefficient of variation and the dimensions are
#'   expressed; the computation is in basic units and physical conversion is
#'   applied afterwards.
#' @return A one-row data frame with columns \code{criterion}, \code{width},
#'   \code{length}, \code{area} (all in basic units), \code{cv} (the predicted
#'   coefficient of variation at the solution) and \code{K} (the Gaussian
#'   curvature of the surface at that point).
#' @examples
#' marginal_efficiency_2d(a = 22.95, m = 0.608, n = 0.232)
#' marginal_efficiency_2d(a = 22.95, m = 0.608, n = 0.232, target_slope = 0.5)
#' @export
marginal_efficiency_2d <- function(a, m, n, target_slope = 1) {
  .validate_parameters(a, m, n)
  if (!is.numeric(target_slope) || length(target_slope) != 1L ||
      !is.finite(target_slope) || target_slope <= 0) {
    stop("'target_slope' must be a single positive number.")
  }
  # Compute the closed-form solution on the log scale to avoid overflow
  # of intermediate powers for valid but large positive coefficients.
  D <- m + n + 1
  log_x <- (log(a) - log(target_slope) + (n + 1) * log(m) - n * log(n)) / D
  log_y <- (log(a) - log(target_slope) + (m + 1) * log(n) - m * log(m)) / D
  x <- exp(log_x)
  y <- exp(log_y)
  cv <- exp(log(a) - m * log_x - n * log_y)
  if (any(!is.finite(c(x, y, cv))) || any(c(x, y, cv) <= 0)) {
    stop("The marginal-efficiency solution is outside the representable numeric range.",
         call. = FALSE)
  }
  data.frame(criterion = "marginal", width = x, length = y,
             area = x * y, cv = cv,
             K = gaussian_curvature_at(a, m, n, x, y))
}
