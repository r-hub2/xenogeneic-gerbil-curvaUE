test_that("example crop grids have the expected matrix dimensions", {
  expected <- list(rice=c(20L,20L),maize=c(20L,20L),
                   common_bean=c(10L,20L),cowpea=c(10L,20L))
  for (nm in names(expected)) {
    m <- example_trial(nm)
    expect_true(is.matrix(m))
    expect_equal(dim(m),expected[[nm]])
    expect_equal(nrow(as_long_trial_data(m)),prod(expected[[nm]]))
    expect_equal(nrow(plot_configurations(m)),
                 if (nm %in% c("rice","maize")) 35L else 23L)
  }
})
test_that("long data preserve the original row-column position", {
  m <- matrix(1:24,4,6,byrow=TRUE)
  d <- as_long_trial_data(m)
  for (i in seq_len(nrow(m))) for (j in seq_len(ncol(m)))
    expect_equal(d$yield[d$row==i & d$column==j],m[i,j])
})
test_that("arbitrary rectangular grids are enumerated exactly", {
  for (dim_ in list(c(4L,4L),c(6L,12L),c(9L,15L),c(12L,6L),c(11L,7L))) {
    m <- matrix(seq_len(prod(dim_)),nrow=dim_[1],ncol=dim_[2])
    nd <- length(which(dim_[1] %% seq_len(dim_[1])==0L)) *
          length(which(dim_[2] %% seq_len(dim_[2])==0L)) - 1L
    expect_equal(nrow(plot_configurations(m)),nd)
    expect_equal(c(validate_trial_matrix(m)$rows,validate_trial_matrix(m)$columns),dim_)
  }
})
test_that("a column-header CSV imports but row indexes are rejected", {
  f <- tempfile(fileext=".csv"); on.exit(unlink(f))
  writeLines(c("C1,C2,C3","11,12,13","21,22,23","31,32,33"),f)
  m <- read_trial_matrix(f)
  expect_equal(dim(m), c(3L, 3L))
  expect_identical(colnames(m), c("C1", "C2", "C3"))
  expect_null(rownames(m))
  expect_equal(unname(m[2, 3]), 23)
  writeLines(c("C1;C2;C3","1,1;2,2;3,3","4,4;5,5;6,6"),f)
  imported <- read_trial_matrix(f)
  expect_identical(colnames(imported), c("C1", "C2", "C3"))
  expect_equal(unname(imported[2, 1]), 4.4)
  writeLines(c("row,C1,C2","1,11,12","2,13,14"),f)
  expect_error(read_trial_matrix(f),"row-index")
  writeLines(c("C1,C2","1,12","2,14","3,16"),f)
  expect_error(read_trial_matrix(f),"row index")
})
test_that("invalid matrices give useful errors", {
  expect_error(plot_configurations(matrix(c(1,NA,2,3),2)),"NA")
  expect_error(plot_configurations(matrix(-1,3,4)),"negative")
  expect_error(plot_configurations(matrix(1,4,4),min_plots=1),"min_plots")
  expect_error(fit_smith(plot_configurations(matrix(1,4,4))),"V1")
})
