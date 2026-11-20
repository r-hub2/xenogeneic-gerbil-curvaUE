
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
#' Displays CV contours and both curvature solutions.
#' @param surface Output of fit_cv_surface().
#' @param configurations Output of plot_configurations().
#' @param grid_size Number of grid points per axis.
#' @return A ggplot object.
#' @export
plot_cv_contours <- function(surface, configurations, grid_size = 120) {
  if (!is.numeric(grid_size) || length(grid_size) != 1L ||
      !is.finite(grid_size) || grid_size != floor(grid_size) || grid_size < 30L)
    stop("'grid_size' must be an integer >= 30.", call. = FALSE)
  a <- surface$a; m <- surface$m; n <- surface$n
  grd <- expand.grid(width = seq(min(configurations$width), max(configurations$width), length.out = grid_size),
                     length = seq(min(configurations$length), max(configurations$length), length.out = grid_size))
  grd$CV <- a * grd$width^(-m) * grd$length^(-n)
  pts <- rbind(max_gaussian_curvature(a, m, n), simultaneous_sectional_curvature(a, m, n))
  pts$criterion <- factor(pts$criterion, levels = c('gaussian', 'sectional'))
  ggplot2::ggplot(grd, ggplot2::aes(x = .data$width, y = .data$length, z = .data$CV)) +
    ggplot2::geom_contour_filled(bins = 13, alpha = 0.72) +
    ggplot2::geom_contour(bins = 12, colour = '#243e50', alpha = 0.35,
                          linewidth = 0.26) +
    ggplot2::geom_point(data = pts,
                        ggplot2::aes(x = .data$width, y = .data$length,
                                     shape = .data$criterion, colour = .data$criterion), inherit.aes = FALSE,
                        size = 3.3, stroke = 1.2) +
    ggplot2::scale_colour_manual(values = c(gaussian = '#c17a42',
                                            sectional = '#243e50')) +
    ggplot2::scale_shape_manual(values = c(gaussian = 16, sectional = 18)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = 'Width (BU)', y = 'Length (BU)',
                  fill = 'CV (%)', shape = 'Criterion', colour = 'Criterion') +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(colour = '#243e50'))
}

#' Plot the CV surface in 3D
#'
#' Draws a perspective plot of the CV surface with curvature solutions.
#' @param surface Output of fit_cv_surface().
#' @param configurations Output of plot_configurations().
#' @param theta,phi Perspective viewing angles.
#' @return Draws a base-R plot and returns invisibly.
#' @export
plot_cv_surface <- function(surface, configurations, theta = -35, phi = 25) {
  a <- surface$a; m <- surface$m; n <- surface$n
  xx <- seq(min(configurations$width), max(configurations$width), length.out = 70)
  yy <- seq(min(configurations$length), max(configurations$length), length.out = 70)
  z <- outer(xx, yy, function(x, y) a * x^(-m) * y^(-n))
  pmat <- graphics::persp(xx, yy, z, theta = theta, phi = phi,
                          xlab = 'Width (BU)', ylab = 'Length (BU)', zlab = 'CV (%)',
                          col = '#b8d7c1', shade = 0.35, ticktype = 'detailed',
                          border = NA, main = 'Power-law CV surface')
  g <- max_gaussian_curvature(a, m, n)
  s <- simultaneous_sectional_curvature(a, m, n)
  for (p in list(list(g, '#c17a42'), list(s, '#243e50'))) {
    pt <- p[[1]]
    xy <- grDevices::trans3d(pt$width, pt$length, pt$cv, pmat)
    graphics::points(xy, pch = 19, col = p[[2]], cex = 1.5)
  }
  graphics::legend('topright', legend = c('Gaussian', 'Sectional'),
                   pch = 19, col = c('#c17a42','#243e50'), bty = 'n')
  invisible(NULL)
}

#' Plot coordinate sections at the curvature solution
#'
#' Displays width and length sections through the selected solution.
#' @param surface Output of fit_cv_surface().
#' @param criterion sectional (default) or gaussian.
#' @param configurations Optional table of configurations.
#' @return A ggplot object.
#' @usage
#' plot_curvature_sections(
#'   surface,
#'   criterion = c("sectional", "gaussian"),
#'   configurations = NULL
#' )
#' @export
plot_curvature_sections <- function(surface, criterion = c('sectional','gaussian'),
                              configurations = NULL) {
  criterion <- match.arg(criterion)
  a <- surface$a; m <- surface$m; n <- surface$n
  pt <- if (criterion == 'sectional') simultaneous_sectional_curvature(a,m,n)
        else max_gaussian_curvature(a,m,n)
  xm <- if (is.null(configurations)) pt$width*3 else max(configurations$width)
  ym <- if (is.null(configurations)) pt$length*3 else max(configurations$length)
  xx <- seq(max(0.1, min(1, pt$width/3)), xm, length.out = 260)
  yy <- seq(max(0.1, min(1, pt$length/3)), ym, length.out = 260)
  nm1 <- 'A. CV vs width (fixed length)'
  nm2 <- 'B. CV vs length (fixed width)'
  curves <- rbind(data.frame(section = nm1, X = xx,
                              CV = a * xx^(-m) * pt$length^(-n)),
                  data.frame(section = nm2, X = yy,
                              CV = a * pt$width^(-m) * yy^(-n)))
  markers <- data.frame(section = c(nm1, nm2), X = c(pt$width, pt$length),
                       CV = c(pt$cv,pt$cv))
  ggplot2::ggplot(curves, ggplot2::aes(x = .data$X, y = .data$CV)) +
    ggplot2::geom_line(linewidth = 0.95, colour = '#58836c') +
    ggplot2::geom_vline(data = markers, ggplot2::aes(xintercept = .data$X),
                        linetype = 'dashed', colour = '#8b9a9a') +
    ggplot2::geom_point(data = markers, size = 3.0, colour = '#243e50') +
    ggplot2::facet_wrap(~section, nrow = 1, scales = 'free_x') +
    ggplot2::labs(x = 'Dimension (BU)', y = 'CV (%)') +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = 'bold', colour = '#243e50'))
}
