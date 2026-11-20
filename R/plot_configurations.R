#' Enumerate admissible experimental-unit geometries
#'
#' Forms all exact rectangular and square partitions with at least two experimental units.
#' Smith uses sample variance regardless of the optional CV denominator.
#' @param trial Numeric yield matrix.
#' @param unit_width_m,unit_length_m Physical basic-unit width and length in metres; only output conversion.
#' @param sd sample (default) or population for descriptive CV.
#' @param min_plots Minimum number of experimental units per configuration (at least 2).
#' @return A data frame of configurations, conventional and auxiliary variances, and CVs.
#' @usage
#' plot_configurations(
#'   trial, unit_width_m = 1, unit_length_m = 1,
#'   sd = c("sample", "population"), min_plots = 2L
#' )
#' @export
plot_configurations <- function(trial, unit_width_m = 1, unit_length_m = 1,
                            sd = c("sample", "population"),
                            min_plots = 2L) {
  sd <- match.arg(sd)
  M <- .validate_trial(trial)
  .validate_positive_scalar(unit_width_m, "unit_width_m")
  .validate_positive_scalar(unit_length_m, "unit_length_m")
  if (!is.numeric(min_plots) || length(min_plots) != 1L ||
      !is.finite(min_plots) || min_plots < 2L ||
      min_plots != floor(min_plots))
    stop("'min_plots' must be an integer >= 2.", call. = FALSE)

  nf <- nrow(M); nc <- ncol(M); N <- length(M); mu <- mean(M)
  rows <- list()
  for (w in .divisors(nc)) for (l in .divisors(nf)) {
    nbc <- nc %/% w; nbr <- nf %/% l; P <- nbc * nbr
    if (P < min_plots) next
    X <- w * l
    tot <- numeric(P); k <- 0L
    for (br in seq_len(nbr)) for (bc in seq_len(nbc)) {
      rr <- ((br - 1L) * l + 1L):(br * l)
      cc <- ((bc - 1L) * w + 1L):(bc * w)
      k <- k + 1L
      tot[k] <- sum(M[rr, cc, drop = FALSE])
    }
    s <- .sd_value(tot, sd)
    sample_variance <- stats::var(tot)
    vs <- X * sum((tot / X - mu)^2) / (N - 1L)
    rows[[length(rows) + 1L]] <- data.frame(
      width_ub = w, length_ub = l,
      width = w, length = l, area = X,
      width_m = w * unit_width_m, length_m = l * unit_length_m,
      area_m2 = X * unit_width_m * unit_length_m,
      n_plots = P, mean_total = mean(tot),
      total_variance = sample_variance,
      unit_variance = sample_variance / X^2,
      smith_normalized_variance = vs,
      cv = 100 * s / mean(tot),
      smith_normalized_cv = 100 * sqrt(vs) / mu)
  }
  if (!length(rows))
    stop("No admissible configurations meet the requested minimum number of plots.", call. = FALSE)
  cfg <- do.call(rbind, rows)
  cfg <- cfg[order(cfg$area, cfg$length, cfg$width), , drop = FALSE]
  rownames(cfg) <- NULL
  cfg
}
