#' Gaussian curvature at a point
#'
#' Calculates Gaussian curvature of z=a*x^(-m)*y^(-n).
#' @param a,m,n Positive power-law surface parameters.
#' @param x,y Positive basic-unit coordinates.
#' @return A numeric Gaussian-curvature value.
#' @export
gaussian_curvature_at <- function(a, m, n, x, y) {
  .validate_parameters(a, m, n)
  .validate_positive_scalar(x, "x")
  .validate_positive_scalar(y, "y")
  f <- a * x^(-m) * y^(-n)
  S <- 1 + m^2 * f^2 / x^2 + n^2 * f^2 / y^2
  m * n * (m + n + 1) * f^2 / (x^2 * y^2 * S^2)
}

#' Maximum Gaussian curvature solution
#'
#' Closed-form maximum of Gaussian curvature for the separable power-law surface.
#' @param a,m,n Positive power-law surface parameters.
#' @return A one-row data frame with continuous dimensions, area, predicted CV, and K.
#' @export
max_gaussian_curvature <- function(a, m, n) {
  .validate_parameters(a, m, n)
  Uc <- (n + 1) / (m + n)
  Vc <- (m + 1) / (m + n)
  fc <- (a * m^(-m) * n^(-n) * Uc^(m / 2) * Vc^(n / 2))^(1 / (m + n + 1))
  xc <- m * fc / sqrt(Uc)
  yc <- n * fc / sqrt(Vc)
  data.frame(criterion = "gaussian", width = xc, length = yc,
             area = xc * yc, cv = fc, K = gaussian_curvature_at(a, m, n, xc, yc))
}

#' Simultaneous sectional curvature solution
#'
#' Closed-form intersection of the two coordinate-section maximum-curvature loci.
#' @param a,m,n Positive power-law surface parameters.
#' @return A one-row data frame with continuous dimensions, area, predicted CV, and K.
#' @export
simultaneous_sectional_curvature <- function(a, m, n) {
  .validate_parameters(a, m, n)
  q <- function(b) b^2 * (2 * b + 1) / (b + 2)
  fs <- (a * q(m)^(-m / 2) * q(n)^(-n / 2))^(1 / (m + n + 1))
  xs <- sqrt(q(m)) * fs
  ys <- sqrt(q(n)) * fs
  data.frame(criterion = "sectional", width = xs, length = ys,
             area = xs * ys, cv = fs, K = gaussian_curvature_at(a, m, n, xs, ys))
}
