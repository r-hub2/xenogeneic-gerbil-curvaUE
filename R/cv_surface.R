#' Fit a bivariate power-law CV surface
#'
#' Fits CV=a*width^(-m)*length^(-n) by unweighted nonlinear least squares, with dimensions
#' in basic units.
#' @param configurations Output of plot_configurations().
#' @return A list with a, m, n, model, residuals, and descriptive RMSE.
#' @export
fit_cv_surface <- function(configurations) {
  if (!all(c("cv", "width_ub", "length_ub") %in% names(configurations)))
    stop("Missing 'cv', 'width_ub', or 'length_ub' from plot_configurations().", call. = FALSE)
  d <- data.frame(cv = configurations$cv, x = configurations$width_ub, y = configurations$length_ub)
  d <- d[is.finite(d$cv) & d$cv > 0 &
           is.finite(d$x) & d$x > 0 &
           is.finite(d$y) & d$y > 0, , drop = FALSE]
  if (nrow(d) < 4L || length(unique(d$x)) < 2L ||
      length(unique(d$y)) < 2L)
    stop("Not enough configurations to fit the CV surface.",
         call. = FALSE)
  design_matrix <- cbind(1, log(d$x), log(d$y))
  if (qr(design_matrix)$rank < 3L)
    stop("The bivariate CV surface is not identifiable: too few distinct widths or lengths.",
         call. = FALSE)
  initial_fit <- stats::lm(log(cv) ~ log(x) + log(y), data = d)
  initial_coef <- stats::coef(initial_fit)
  if (any(!is.finite(initial_coef)))
    stop("The surface initialization produced nonfinite coefficients.", call. = FALSE)
  a0 <- max(1e-5, exp(unname(initial_coef[[1L]])))
  m0 <- max(0.01, -unname(initial_coef[[2L]]))
  n0 <- max(0.01, -unname(initial_coef[[3L]]))
  fit <- tryCatch(
    stats::nls(cv ~ a * x^(-m) * y^(-n), data = d,
               start = list(a = a0, m = m0, n = n0),
               algorithm = "port",
               lower = c(a = 1e-10, m = 1e-10, n = 1e-10),
               control = stats::nls.control(maxiter = 500)),
    error = function(e)
      stop("The nonlinear CV-surface fit failed to converge: ",
           conditionMessage(e), call. = FALSE))
  if (!isTRUE(fit$convInfo$isConv))
    stop("The CV surface fit did not report convergence.",
         call. = FALSE)
  co <- stats::coef(fit)
  if (any(!is.finite(co)))
    stop("The CV surface fit produced nonfinite parameters.",
         call. = FALSE)
  r <- stats::residuals(fit)
  list(a = unname(co[["a"]]), m = unname(co[["m"]]),
       n = unname(co[["n"]]), model = fit,
       residuals = unname(r), rmse_descriptive = sqrt(mean(r^2)),
       n_configurations = nrow(d))
}
