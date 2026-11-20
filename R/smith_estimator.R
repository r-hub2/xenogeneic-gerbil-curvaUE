#' Estimate Smith heterogeneity using weighted log variance
#'
#' The main method is Smith (1938) WLS with weights M-1 and a free intercept; b_prime
#' applies to the finite observed field.
#' @param configurations Output of plot_configurations().
#' @param method smith1938 (default), nls, log, or unweighted_log.
#' @return A list with b_prime, model, weights, and fit diagnostics.
#' @export
fit_smith <- function(configurations, method = c("smith1938", "nls", "log", "unweighted_log")) {
  method <- match.arg(method)
  if (!all(c("area", "n_plots", "smith_normalized_variance", "unit_variance") %in% names(configurations)))
    stop("Expected area, n_plots, smith_normalized_variance, and unit_variance columns.",
         call. = FALSE)
  v1 <- configurations$smith_normalized_variance[configurations$area == 1]
  if (length(v1) != 1L || !is.finite(v1) || v1 <= 0)
    stop("A unique one-basic-unit configuration with V1 > 0 is required.",
         call. = FALSE)
  V1 <- as.numeric(v1)
  if (method %in% c("smith1938", "unweighted_log")) {
    d2 <- data.frame(X = configurations$area, Vx = configurations$unit_variance,
                     M = configurations$n_plots)
    d2 <- d2[is.finite(d2$Vx) & d2$Vx > 0 &
               is.finite(d2$X) & d2$X >= 1 &
               is.finite(d2$M) & d2$M >= 2, , drop = FALSE]
    if (nrow(d2) < 3L || length(unique(d2$X)) < 2L)
      stop("Not enough distinct plot sizes have positive conventional unit variance for Smith regression.",
           call. = FALSE)
    weights <- if (method == "smith1938") d2$M - 1 else rep(1, nrow(d2))
    model <- stats::lm(log(Vx) ~ log(X), data = d2, weights = weights)
    co <- stats::coef(model)
    if (length(co) < 2L || any(!is.finite(co)))
      stop("Smith regression returned nonfinite coefficients.", call. = FALSE)
    b <- -unname(co[[2L]])
    d_aux <- data.frame(X = configurations$area, Vx = configurations$smith_normalized_variance)
    d_aux <- d_aux[is.finite(d_aux$Vx) & d_aux$Vx > 0 &
                     is.finite(d_aux$X) & d_aux$X > 1, , drop = FALSE]
    if (nrow(d_aux) >= 2L && length(unique(d_aux$X)) >= 2L) {
      lx <- log(d_aux$X)
      b_log <- sum(lx * log(V1 / d_aux$Vx)) / sum(lx^2)
    } else b_log <- NA_real_
    return(list(b = b, b_prime = b, b_corrected = NA_real_,
                V1 = V1, V1_fitted = exp(unname(co[[1L]])),
                method = method, n_configurations = nrow(d2),
                weights = weights, fit_data = d2, b_log = b_log,
                model = model,
                warning = if (method == "smith1938")
                  "Smith (1938): log-variance WLS weighted by M-1; b_prime refers to the observed finite field and has not been adjusted for an infinite field. Configurations share observations."
                else "Unweighted log-variance regression; b_prime refers to the observed finite field and has not been adjusted for an infinite field."))
  }
  d <- data.frame(X = configurations$area, Vx = configurations$smith_normalized_variance)
  d <- d[is.finite(d$Vx) & d$Vx > 0 & is.finite(d$X) & d$X > 1,
         , drop = FALSE]
  if (nrow(d) < 2L || length(unique(d$X)) < 2L)
    stop("Not enough distinct areas have positive auxiliary variance.",
         call. = FALSE)
  log_X <- log(d$X)
  b_log <- sum(log_X * log(V1 / d$Vx)) / sum(log_X^2)
  if (method == "log") {
    model <- stats::lm(log(V1 / Vx) ~ 0 + log(X), data = d)
    b <- unname(stats::coef(model)[[1L]])
  } else {
    model <- stats::nls(Vx ~ V1 * X^(-b), data = d,
                         start = list(b = max(1e-4, b_log)),
                         algorithm = "port", lower = c(b = 0),
                         control = stats::nls.control(maxiter = 500))
    b <- unname(stats::coef(model)[["b"]])
  }
  list(b = b, V1 = V1, method = method,
       n_configurations = nrow(d) + 1L, b_log = b_log, model = model)
}
