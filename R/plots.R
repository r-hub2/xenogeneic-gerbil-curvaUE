
#' Plot a yield-grid heatmap
#'
#' Displays the yield associated with each basic unit.
#' @param trial Numeric yield matrix.
#' @return A ggplot object.
#' @importFrom rlang .data
#' @export
plot_trial_heatmap <- function(trial) {
  M <- .validate_trial(trial)
  d <- as_long_trial_data(M)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$column, y = .data$row, fill = .data$yield)) +
    ggplot2::geom_raster() +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_gradientn(colours = grDevices::hcl.colors(30, 'YlGnBu'),
                                  name = 'Yield\nper BU') +
    ggplot2::labs(x = 'Column (width, BU)', y = 'Row (length, BU)') +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(colour = '#243e50'),
                   legend.title = ggplot2::element_text(size = 10))
}

#' Plot empirical CV profiles
#'
#' Shows CV against width and length with the power-law curves.
#' @param configurations Output of plot_configurations().
#' @param surface Output of fit_cv_surface().
#' @return A ggplot object.
#' @export
plot_cv_profiles <- function(configurations, surface) {
  a <- surface$a; m <- surface$m; n <- surface$n
  d1 <- configurations[configurations$length == min(configurations$length), , drop = FALSE]
  d2 <- configurations[configurations$width == min(configurations$width), , drop = FALSE]
  obs <- rbind(data.frame(dim = 'A. Width (length = 1 BU)', X = d1$width,
                          CV = d1$cv),
               data.frame(dim = 'B. Length (width = 1 BU)', X = d2$length,
                          CV = d2$cv))
  x1 <- seq(min(d1$width), max(d1$width), length.out = 250)
  x2 <- seq(min(d2$length), max(d2$length), length.out = 250)
  pre <- rbind(data.frame(dim = 'A. Width (length = 1 BU)', X = x1,
                          CV = a * x1^(-m) * min(configurations$length)^(-n)),
               data.frame(dim = 'B. Length (width = 1 BU)', X = x2,
                          CV = a * min(configurations$width)^(-m) * x2^(-n)))
  ggplot2::ggplot() +
    ggplot2::geom_line(data = pre, ggplot2::aes(x = .data$X, y = .data$CV),
                       linewidth = 0.95, colour = '#58836c') +
    ggplot2::geom_point(data = obs, ggplot2::aes(x = .data$X, y = .data$CV),
                        size = 2.3, colour = '#243e50') +
    ggplot2::facet_wrap(~dim, nrow = 1) +
    ggplot2::labs(x = 'Dimension (BU)', y = 'Coefficient of variation (%)') +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = 'bold', colour = '#243e50'),
                   axis.title = ggplot2::element_text(colour = '#243e50'))
}

#' Plot CV-surface contours
#'
#' Displays CV contours and the Gaussian, simultaneous sectional, and
#' coordinate-wise marginal-efficiency solutions. The axes are in basic units.
#' @param surface Output of fit_cv_surface().
#' @param configurations Output of plot_configurations().
#' @param grid_size Number of grid points per axis.
#' @param target_slope Positive target decrease in CV percentage points per
#'   basic unit along either coordinate, for the marginal-efficiency solution.
#' @return A ggplot object. Points outside the displayed configuration range
#'   correspond to extrapolation; the plotting window is extended to show them.
#' @export
plot_cv_contours <- function(surface, configurations, grid_size = 120,
                             target_slope = 1) {
  if (!is.numeric(grid_size) || length(grid_size) != 1L ||
      !is.finite(grid_size) || grid_size != floor(grid_size) || grid_size < 30L)
    stop("'grid_size' must be an integer >= 30.", call. = FALSE)
  a <- surface$a; m <- surface$m; n <- surface$n
  pts <- rbind(max_gaussian_curvature(a, m, n),
               simultaneous_sectional_curvature(a, m, n),
               marginal_efficiency_2d(a, m, n, target_slope))
  # The plot must display all three solutions, including those that lie beyond
  # the range of admissible configurations (model-based extrapolations).
  xmin <- min(configurations$width)
  xmax <- max(max(configurations$width), pts$width) * 1.05
  ymin <- min(configurations$length)
  ymax <- max(max(configurations$length), pts$length) * 1.05
  grd <- expand.grid(width = seq(xmin, xmax, length.out = grid_size),
                     length = seq(ymin, ymax, length.out = grid_size))
  grd$CV <- a * grd$width^(-m) * grd$length^(-n)
  pts$criterion <- factor(pts$criterion,
                          levels = c('gaussian', 'sectional', 'marginal'))
  ggplot2::ggplot(grd, ggplot2::aes(x = .data$width, y = .data$length, z = .data$CV)) +
    ggplot2::geom_contour_filled(bins = 13, alpha = 0.72) +
    ggplot2::geom_contour(bins = 12, colour = '#243e50', alpha = 0.35,
                          linewidth = 0.26) +
    ggplot2::geom_point(data = pts,
                        ggplot2::aes(x = .data$width, y = .data$length,
                                     shape = .data$criterion, colour = .data$criterion),
                        inherit.aes = FALSE, size = 3.5, stroke = 1.2) +
    ggplot2::scale_colour_manual(values = c(gaussian = '#c17a42',
                                            sectional = '#243e50',
                                            marginal = '#8b3c78')) +
    ggplot2::scale_shape_manual(values = c(gaussian = 16, sectional = 18,
                                           marginal = 17)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = 'Width (BU)', y = 'Length (BU)',
                  fill = 'CV (%)', shape = 'Criterion', colour = 'Criterion') +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(colour = '#243e50'))
}

#' Plot the CV surface in 3D
#'
#' Draws a perspective plot of the CV surface with all three solutions.
#' @param surface Output of fit_cv_surface().
#' @param configurations Output of plot_configurations().
#' @param theta,phi Perspective viewing angles.
#' @param target_slope Positive target decrease in CV percentage points per
#'   basic unit along each coordinate for the marginal-efficiency solution.
#' @return Draws a base-R plot and returns invisibly.
#' @export
plot_cv_surface <- function(surface, configurations, theta = -35, phi = 25,
                            target_slope = 1) {
  a <- surface$a; m <- surface$m; n <- surface$n
  pts <- rbind(max_gaussian_curvature(a, m, n),
               simultaneous_sectional_curvature(a, m, n),
               marginal_efficiency_2d(a, m, n, target_slope))
  # Expand the plotted surface if the selected target implies extrapolation.
  xx <- seq(min(configurations$width),
            max(max(configurations$width), pts$width) * 1.05, length.out = 70)
  yy <- seq(min(configurations$length),
            max(max(configurations$length), pts$length) * 1.05, length.out = 70)
  z <- outer(xx, yy, function(x, y) a * x^(-m) * y^(-n))
  pmat <- graphics::persp(xx, yy, z, theta = theta, phi = phi,
                          xlab = 'Width (BU)', ylab = 'Length (BU)', zlab = 'CV (%)',
                          col = '#b8d7c1', shade = 0.35, ticktype = 'detailed',
                          border = NA, main = 'Power-law CV surface')
  cols <- c(gaussian = '#c17a42', sectional = '#243e50', marginal = '#8b3c78')
  symbols <- c(gaussian = 16, sectional = 18, marginal = 17)
  for (i in seq_len(nrow(pts))) {
    point <- pts[i, , drop = FALSE]
    xy <- grDevices::trans3d(point$width, point$length, point$cv, pmat)
    graphics::points(xy, pch = symbols[[point$criterion]],
                     col = cols[[point$criterion]], cex = 1.6)
  }
  graphics::legend('topright', legend = c('Gaussian', 'Sectional', 'Marginal'),
                   pch = unname(symbols), col = unname(cols), bty = 'n')
  invisible(NULL)
}

#' Plot coordinate sections through a selected solution
#'
#' Shows width and length CV profiles through the selected Gaussian,
#' simultaneous sectional, or marginal-efficiency solution. Use criterion =
#' "all" to overlay profiles and solution markers for all three methods.
#' @param surface Output of fit_cv_surface().
#' @param criterion sectional (default), gaussian, marginal, or all.
#' @param configurations Optional table of configurations.
#' @param target_slope Positive target decrease in CV percentage points per
#'   basic unit, used for the marginal-efficiency solution.
#' @return A ggplot object.
#' @usage
#' plot_curvature_sections(
#'   surface, criterion = c("sectional", "gaussian", "marginal", "all"),
#'   configurations = NULL, target_slope = 1
#' )
#' @export
plot_curvature_sections <- function(surface,
                                    criterion = c('sectional', 'gaussian', 'marginal', 'all'),
                                    configurations = NULL, target_slope = 1) {
  criterion <- match.arg(criterion)
  a <- surface$a; m <- surface$m; n <- surface$n
  pts <- switch(criterion,
    sectional = simultaneous_sectional_curvature(a, m, n),
    gaussian = max_gaussian_curvature(a, m, n),
    marginal = marginal_efficiency_2d(a, m, n, target_slope),
    all = rbind(max_gaussian_curvature(a, m, n),
                simultaneous_sectional_curvature(a, m, n),
                marginal_efficiency_2d(a, m, n, target_slope)))
  xmax <- if (is.null(configurations)) max(pts$width) * 3 else
    max(max(configurations$width), pts$width * 1.1)
  ymax <- if (is.null(configurations)) max(pts$length) * 3 else
    max(max(configurations$length), pts$length * 1.1)
  xmin <- max(0.1, min(1, min(pts$width) / 3))
  ymin <- max(0.1, min(1, min(pts$length) / 3))
  xx <- seq(xmin, xmax, length.out = 260)
  yy <- seq(ymin, ymax, length.out = 260)
  nm1 <- 'A. CV vs width (fixed length)'
  nm2 <- 'B. CV vs length (fixed width)'
  curve_list <- lapply(seq_len(nrow(pts)), function(i) {
    pt <- pts[i, , drop = FALSE]
    rbind(data.frame(section = nm1, X = xx,
                     CV = a * xx^(-m) * pt$length^(-n), criterion = pt$criterion),
          data.frame(section = nm2, X = yy,
                     CV = a * pt$width^(-m) * yy^(-n), criterion = pt$criterion))
  })
  curves <- do.call(rbind, curve_list)
  markers <- do.call(rbind, lapply(seq_len(nrow(pts)), function(i) {
    pt <- pts[i, , drop = FALSE]
    data.frame(section = c(nm1, nm2), X = c(pt$width, pt$length),
               CV = c(pt$cv, pt$cv), criterion = pt$criterion)
  }))
  curves$criterion <- factor(curves$criterion, levels = c('gaussian','sectional','marginal'))
  markers$criterion <- factor(markers$criterion, levels = c('gaussian','sectional','marginal'))
  ggplot2::ggplot(curves, ggplot2::aes(x = .data$X, y = .data$CV,
                                       colour = .data$criterion)) +
    ggplot2::geom_line(linewidth = 0.95) +
    ggplot2::geom_vline(data = markers, ggplot2::aes(xintercept = .data$X,
                                                    colour = .data$criterion),
                        inherit.aes = FALSE, linetype = 'dashed', linewidth = 0.4) +
    ggplot2::geom_point(data = markers, size = 3.0) +
    ggplot2::scale_colour_manual(values = c(gaussian = '#c17a42',
                                            sectional = '#243e50',
                                            marginal = '#8b3c78'), name = 'Criterion') +
    ggplot2::facet_wrap(~section, nrow = 1, scales = 'free_x') +
    ggplot2::labs(x = 'Dimension (BU)', y = 'CV (%)') +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = 'bold', colour = '#243e50'))
}
