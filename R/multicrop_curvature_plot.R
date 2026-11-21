#' Plot multi-crop CV contours and experimental-unit criteria
#'
#' Creates faceted CV contours and the two sectional-curvature loci. Marks
#' the Gaussian, simultaneous sectional, and marginal-efficiency solutions.
#' The dashed guides and numerical labels refer only to the sectional solution.
#' @param trials Named list of trial matrices. NULL uses the four example crops.
#' @param min_plots Minimum number of experimental units per configuration.
#' @param unit_width_m,unit_length_m Physical dimensions of one basic unit, in metres.
#' @param cv_interval CV contour interval in percentage points.
#' @param grid_size Number of grid points per axis.
#' @param target_slope Positive target CV decrease (percentage points per basic
#'   unit) for the marginal-efficiency criterion.
#' @return A faceted ggplot object.
#' @usage
#' plot_multicrop_curvature(
#'   trials = NULL,
#'   min_plots = 2L,
#'   unit_width_m = 1, unit_length_m = 1,
#'   cv_interval = 5, grid_size = 180L, target_slope = 1
#' )
#' @export
plot_multicrop_curvature <- function(
    trials = NULL,
    min_plots = 2L, unit_width_m = 1, unit_length_m = 1,
    cv_interval = 5, grid_size = 180L, target_slope = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install the 'ggplot2' package to draw this figure.", call. = FALSE)
  if (is.null(trials)) {
    crop_ids <- c("rice", "maize", "common_bean", "cowpea")
    trials <- stats::setNames(lapply(crop_ids, example_trial), crop_ids)
  }
  if (!is.list(trials) || !length(trials) || is.null(names(trials)) ||
      any(!nzchar(names(trials))))
    stop("'trials' must be a named list of trial matrices.",
         call. = FALSE)
  .validate_positive_scalar(cv_interval, "cv_interval")
  .validate_positive_scalar(target_slope, "target_slope")
  .validate_positive_scalar(unit_width_m, "unit_width_m")
  .validate_positive_scalar(unit_length_m, "unit_length_m")
  if (!is.numeric(grid_size) || length(grid_size) != 1L ||
      !is.finite(grid_size) || grid_size < 40L || grid_size != floor(grid_size))
    stop("'grid_size' must be an integer >= 40.", call. = FALSE)

  labels <- c(rice = "A. Rice", maize = "B. Maize",
                 common_bean = "C. Nambi common bean", cowpea = "D. Cowpea")
  windows <- c(rice = 12.5, maize = 14, common_bean = 10, cowpea = 10)
  grids <- list(); lines_data <- list(); points_data <- list(); guides_data <- list(); labels_data <- list()
  for (i in seq_along(trials)) {
    crop_name <- names(trials)[i]
    M <- .validate_trial(trials[[i]])
    result <- analyze_uniformity_trial(
      M, unit_width_m = unit_width_m, unit_length_m = unit_length_m,
      min_plots = min_plots, criterion = "all", target_slope = target_slope)
    p <- result$surface
    solutions <- result$solution
    s <- solutions[solutions$criterion == "sectional", , drop = FALSE]
    window <- if (crop_name %in% names(windows)) windows[[crop_name]] else 12
    max_length <- min(window, nrow(M))
    max_width <- min(window, ncol(M))
    in_grid <- solutions$length_ub <= nrow(M) &
               solutions$width_ub <= ncol(M) &
               solutions$length_ub >= 1 & solutions$width_ub >= 1
    if (any(in_grid)) {
      max_length <- min(nrow(M), max(max_length, 1.15 * solutions$length_ub[in_grid]))
      max_width <- min(ncol(M), max(max_width, 1.15 * solutions$width_ub[in_grid]))
    }
    if (any(!in_grid)) {
      warning("Some solutions for '", crop_name,
              "' lie outside the observed grid; interpreting them requires extrapolation.",
              call. = FALSE)
    }
    L <- seq(1, max_length, length.out = grid_size)
    A <- seq(1, max_width, length.out = grid_size)
    g <- expand.grid(length_bu = L, width_bu = A)
    g$CV <- p$a * g$width_bu^(-p$m) * g$length_bu^(-p$n)
    g$x <- g$length_bu * unit_length_m
    g$y <- g$width_bu * unit_width_m
    g$crop <- if (crop_name %in% names(labels)) labels[[crop_name]] else crop_name
    grids[[i]] <- g[, c("x", "y", "CV", "crop")]

    qL <- p$n^2 * (2 * p$n + 1) / (p$n + 2)
    Aseq <- seq(1, max_width, length.out = grid_size * 2L)
    Lstar <- (p$a^2 * qL)^(1 / (2 * p$n + 2)) * Aseq^(-p$m / (p$n + 1))
    d1 <- data.frame(x = Lstar * unit_length_m, y = Aseq * unit_width_m,
                     condition = "Maximum curvature along length")
    qA <- p$m^2 * (2 * p$m + 1) / (p$m + 2)
    Lseq <- seq(1, max_length, length.out = grid_size * 2L)
    Astar <- (p$a^2 * qA)^(1 / (2 * p$m + 2)) * Lseq^(-p$n / (p$m + 1))
    d2 <- data.frame(x = Lseq * unit_length_m, y = Astar * unit_width_m,
                     condition = "Maximum curvature along width")
    d <- rbind(d1, d2)
    d <- d[d$x >= unit_length_m & d$x <= max_length * unit_length_m &
             d$y >= unit_width_m & d$y <= max_width * unit_width_m, , drop = FALSE]
    d$crop <- g$crop[1L]
    lines_data[[i]] <- d

    points_data[[i]] <- data.frame(
      x = solutions$length, y = solutions$width,
      CV = solutions$cv, crop = g$crop[1L], criterion = solutions$criterion)
    labels_data[[i]] <- data.frame(
      x = s$length, y = s$width, crop = g$crop[1L],
      label = sprintf("Sectional: L = %.2f; W = %.2f\nCV = %.2f %%",
                      s$length, s$width, s$cv))
    guides_data[[i]] <- data.frame(
      x = c(unit_length_m, s$length), xend = c(s$length, s$length),
      y = c(s$width, unit_width_m), yend = c(s$width, s$width),
      crop = g$crop[1L])
  }
  G <- do.call(rbind, grids); D <- do.call(rbind, lines_data)
  P <- do.call(rbind, points_data); U <- do.call(rbind, guides_data)
  labels_df <- do.call(rbind, labels_data)
  levels_crop <- unname(labels[names(labels) %in% names(trials)])
  if (length(levels_crop) != length(trials)) levels_crop <- unique(G$crop)
  G$crop <- factor(G$crop, levels = levels_crop)
  D$crop <- factor(D$crop, levels = levels_crop)
  P$crop <- factor(P$crop, levels = levels_crop)
  P$criterion <- factor(P$criterion, levels = c('gaussian', 'sectional', 'marginal'))
  labels_df$crop <- factor(labels_df$crop, levels = levels_crop)
  U$crop <- factor(U$crop, levels = levels_crop)
  cv_upper <- ceiling(max(G$CV) / cv_interval) * cv_interval
  breaks_cv <- seq(0, cv_upper, by = cv_interval)
  if (length(breaks_cv) < 3L) breaks_cv <- seq(0, cv_upper + cv_interval, by = cv_interval)
  palette_cv <- grDevices::colorRampPalette(
    c("#f7fbff", "#d9e8f5", "#9ecae1", "#4292c6", "#08306b"))(
      length(breaks_cv) - 1L)
  format_ticks <- function(x) format(round(x, 2), trim = TRUE)
  condition_levels <- c("Maximum curvature along length", "Maximum curvature along width")
  D$condition <- factor(D$condition, levels = condition_levels)
  # English plots use decimal points by default.
  labels_df$label_x <- labels_df$x + 0.07 * (labels_df$x - unit_length_m + 5 * unit_length_m)
  labels_df$label_y <- labels_df$y + 0.10 * (labels_df$y - unit_width_m + 4 * unit_width_m)
  ggplot2::ggplot(G, ggplot2::aes(x = .data$x, y = .data$y, z = .data$CV)) +
    ggplot2::geom_contour_filled(breaks = breaks_cv) +
    ggplot2::geom_segment(data = U, ggplot2::aes(x = .data$x, xend = .data$xend,
                             y = .data$y, yend = .data$yend), inherit.aes = FALSE,
                             linetype = "dashed", linewidth = 0.3,
                             colour = "#303b43") +
    ggplot2::geom_line(data = D,
      ggplot2::aes(x = .data$x, y = .data$y,
                   colour = .data$condition, linetype = .data$condition),
      inherit.aes = FALSE, linewidth = 1.0) +
    ggplot2::geom_point(data = P,
      ggplot2::aes(x = .data$x, y = .data$y, shape = .data$criterion),
      inherit.aes = FALSE, size = 3.2, stroke = 1.0, colour = "#101820") +
    ggplot2::geom_label(data = labels_df,
      ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$label),
      inherit.aes = FALSE, size = 3.0, hjust = 0, vjust = 0.5,
      fill = "white", linewidth = 0.25) +
    ggplot2::facet_wrap(~crop, ncol = 2, scales = "free") +
    ggplot2::scale_fill_manual(values = palette_cv,
      name = expression(hat(CV)~"(%)"), drop = FALSE) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(c("#c51f32", "#267c39"), condition_levels),
      name = NULL) +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(c("solid", "dashed"), condition_levels),
      name = NULL) +
    ggplot2::scale_shape_manual(values = c(gaussian = 16, sectional = 18,
                                           marginal = 17), name = "Criterion",
                                 labels = c("Gaussian", "Sectional", "Marginal")) +
    ggplot2::scale_x_continuous(labels = format_ticks) +
    ggplot2::scale_y_continuous(labels = format_ticks) +
    ggplot2::labs(x = "Length (m)", y = "Width (m)") +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 2, reverse = TRUE),
                    colour = ggplot2::guide_legend(order = 1),
                    linetype = ggplot2::guide_legend(order = 1),
                    shape = ggplot2::guide_legend(order = 3)) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey92", colour = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e8e8e8"),
      legend.position = "right", axis.title = ggplot2::element_text(face = "bold"))
}
