test_that("existing positional API remains backward compatible", {
  trial <- example_trial("rice")
  named <- analyze_uniformity_trial(trial, smith_method = "smith1938")
  positional <- analyze_uniformity_trial(trial, 1, 1, "sample", 2L,
                                         "both", "smith1938")
  expect_equal(positional$smith$b_prime, named$smith$b_prime)
  expect_identical(as.character(positional$solution$criterion),
                   c("gaussian", "sectional"))
  expect_equal(positional$solution, named$solution)
})

test_that("marginal solution solves independent first-order conditions", {
  for (params in list(c(22.95414, .60814, .23216),
                      c(56.03311, .36025, .26291),
                      c(36.43530, .32730, .35966),
                      c(24.94977, .35910, .31766))) {
    a <- params[1L]; m <- params[2L]; n <- params[3L]
    for (tau in c(.5, 1, 2)) {
      pt <- marginal_efficiency_2d(a, m, n, target_slope = tau)
      expect_equal(m * pt$cv / pt$width, tau, tolerance = 1e-10)
      expect_equal(n * pt$cv / pt$length, tau, tolerance = 1e-10)
      expect_equal(pt$length / pt$width, n / m, tolerance = 1e-10)
      expect_equal(pt$area, pt$width * pt$length, tolerance = 1e-10)
      expect_equal(pt$K, gaussian_curvature_at(a,m,n,pt$width,pt$length))
    }
  }
})

test_that("marginal target changes only marginal point, not curvature methods", {
  trial <- example_trial("rice")
  a <- analyze_uniformity_trial(trial, criterion = "all", target_slope = 1)
  b <- analyze_uniformity_trial(trial, criterion = "all", target_slope = .5)
  expect_equal(a$solution[1:2, ], b$solution[1:2, ])
  expect_gt(b$solution$area_ub[3L], a$solution$area_ub[3L])
  expect_equal(a$surface$a, b$surface$a)
  only <- analyze_uniformity_trial(trial, criterion = "marginal")
  expect_equal(nrow(only$solution), 1L)
  expect_identical(as.character(only$solution$criterion), "marginal")
})

test_that("marginal rejects invalid target slopes and avoids intermediate overflow", {
  for (tau in list(-1, 0, Inf, NA_real_, "1", c(1, 2))) {
    expect_error(marginal_efficiency_2d(10, .6, .3, target_slope = tau))
  }
  x <- marginal_efficiency_2d(a = 1e100, m = 2, n = 1, target_slope = 1)
  expect_true(all(is.finite(unlist(x[c("width", "length", "area", "cv", "K")]))))
})

test_that("all graphics expose marginal-efficiency solution markers", {
  skip_if_not_installed("ggplot2")
  trial <- example_trial("rice")
  cfg <- plot_configurations(trial)
  surface <- fit_cv_surface(cfg)
  find_criteria <- function(plot) {
    unlist(lapply(plot$layers, function(layer) {
      d <- layer$data
      if (is.data.frame(d) && "criterion" %in% names(d))
        return(as.character(d$criterion))
      character(0)
    }), use.names = FALSE)
  }
  methods <- c("gaussian", "sectional", "marginal")
  p <- plot_cv_contours(surface, cfg)
  expect_true(all(methods %in% find_criteria(p)))
  expect_error(ggplot2::ggplot_build(p), NA)
  sections <- plot_curvature_sections(surface, criterion = "all",
                                       configurations = cfg)
  expect_true(all(methods %in% find_criteria(sections)))
  expect_error(ggplot2::ggplot_build(sections), NA)
  marginal_sections <- plot_curvature_sections(surface, criterion = "marginal",
                                               configurations = cfg)
  expect_error(ggplot2::ggplot_build(marginal_sections), NA)
  crops <- plot_multicrop_curvature(grid_size = 40L)
  markers <- find_criteria(crops)
  expect_equal(sum(markers == "marginal"), 4L)
  expect_equal(sum(markers == "gaussian"), 4L)
  expect_equal(sum(markers == "sectional"), 4L)
  expect_error(ggplot2::ggplot_build(crops), NA)
})

test_that("three-dimensional plotting builds for multiple marginal targets", {
  trial <- example_trial("rice")
  cfg <- plot_configurations(trial)
  surface <- fit_cv_surface(cfg)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({grDevices::dev.off(); unlink(path)}, add = TRUE)
  expect_invisible(plot_cv_surface(surface, cfg, target_slope = 1))
  expect_invisible(plot_cv_surface(surface, cfg, target_slope = .5))
})
