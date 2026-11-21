test_that("the Gaussian reference case is exact", {
  g <- max_gaussian_curvature(1,1,1)
  expect_equal(g$width,1,tolerance=1e-10)
  expect_equal(g$length,1,tolerance=1e-10)
  expect_equal(g$K,1/3,tolerance=1e-10)
})
test_that("closed-form solutions match the mathematical reference", {
  g <- max_gaussian_curvature(22.9541,0.6081,0.2322)
  s <- simultaneous_sectional_curvature(22.9541,0.6081,0.2322)
  expect_equal(g$width,4.3348,tolerance=1e-3)
  expect_equal(g$length,1.4489,tolerance=1e-3)
  expect_equal(s$width,4.5996,tolerance=1e-3)
  expect_equal(s$length,1.5432,tolerance=1e-3)
  expect_gte(g$K,s$K)
})
test_that("Smith's weighted regression matches direct WLS", {
  cfg <- plot_configurations(example_trial("rice"))
  s <- fit_smith(cfg)
  reference <- lm(log(unit_variance)~log(area),data=cfg,weights=n_plots-1)
  expect_equal(s$b_prime,.826791,tolerance=1e-4)
  expect_equal(unname(coef(s$model)),unname(coef(reference)),tolerance=1e-11)
  expect_equal(s$weights,cfg$n_plots-1)
  expect_true(is.na(s$b_corrected))
})
test_that("Smith uses sample variance even for population CV", {
  m <- example_trial("rice")
  a <- plot_configurations(m,sd="sample")
  b <- plot_configurations(m,sd="population")
  expect_equal(a$unit_variance,b$unit_variance)
  expect_equal(a$smith_normalized_variance,b$smith_normalized_variance)
  expect_equal(fit_smith(a)$b_prime,fit_smith(b)$b_prime)
  expect_true(all(b$cv<=a$cv))
})
test_that("basic-unit geometry is invariant to physical measurement units", {
  m <- example_trial("rice")
  a <- analyze_uniformity_trial(m,unit_width_m=1,unit_length_m=1)
  b <- analyze_uniformity_trial(m,unit_width_m=2,unit_length_m=.5)
  expect_equal(a$surface$a,b$surface$a)
  expect_equal(a$solution$width_ub,b$solution$width_ub)
  expect_equal(a$solution$length_ub,b$solution$length_ub)
  expect_equal(b$solution$width,2*a$solution$width)
  expect_equal(b$solution$length,.5*a$solution$length)
  expect_equal(a$solution$area,b$solution$area)
})
test_that("all four example surfaces fit within numerical tolerances", {
  values <- list(rice=c(22.95414,.60814,.23216),
                 maize=c(56.03311,.36025,.26291),
                 common_bean=c(36.43530,.32730,.35966),
                 cowpea=c(24.94977,.35910,.31766))
  for (nm in names(values)) {
    s <- fit_cv_surface(plot_configurations(example_trial(nm)))
    expect_equal(c(s$a,s$m,s$n),values[[nm]],tolerance=2e-3)
  }
})
test_that("plotting routines return ggplot objects", {
  skip_if_not_installed("ggplot2")
  m <- example_trial("rice")
  c <- plot_configurations(m)
  s <- fit_cv_surface(c)
  expect_s3_class(plot_trial_heatmap(m),"ggplot")
  expect_s3_class(plot_cv_profiles(c,s),"ggplot")
  expect_s3_class(plot_cv_contours(s,c),"ggplot")
  expect_s3_class(plot_curvature_sections(s,configurations=c),"ggplot")
  expect_s3_class(plot_multicrop_curvature(grid_size=40L),"ggplot")
})


test_that("3D plotting uses the exported perspective transformer", {
  trial <- example_trial("rice")
  cfg <- plot_configurations(trial)
  surface <- fit_cv_surface(cfg)
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  expect_invisible(plot_cv_surface(surface, cfg))
})

test_that("ggplot objects can be built with data-mask column references", {
  skip_if_not_installed("ggplot2")
  trial <- example_trial("rice")
  cfg <- plot_configurations(trial)
  surface <- fit_cv_surface(cfg)
  plots <- list(plot_trial_heatmap(trial),
                plot_cv_profiles(cfg, surface),
                plot_cv_contours(surface, cfg),
                plot_curvature_sections(surface, configurations = cfg),
                plot_multicrop_curvature(grid_size = 40L))
  for (p in plots) {
    expect_error(ggplot2::ggplot_build(p), NA)
  }
})

test_that("marginal_efficiency_2d satisfies the marginal conditions in closed form", {
  a <- 22.95; m <- 0.608; n <- 0.232; tau <- 1
  res <- marginal_efficiency_2d(a, m, n, target_slope = tau)
  x <- res$width; y <- res$length; cv <- res$cv
  expect_equal(cv, a * x^(-m) * y^(-n), tolerance = 1e-8)
  expect_equal(m * cv / x, tau, tolerance = 1e-8)
  expect_equal(n * cv / y, tau, tolerance = 1e-8)
  expect_equal(y / x, n / m, tolerance = 1e-8)
  expect_identical(as.character(res$criterion), "marginal")
  expect_error(marginal_efficiency_2d(a, m, n, target_slope = -1))
})

test_that("analyze_uniformity_trial returns three solutions with criterion = all", {
  res <- analyze_uniformity_trial(example_trial("rice"), criterion = "all")
  expect_equal(nrow(res$solution), 3L)
  expect_true("marginal" %in% as.character(res$solution$criterion))
})
