.sd_value <- function(x, type = c("population", "sample")) {
  type <- match.arg(type)
  if (type == "sample") stats::sd(x) else sqrt(mean((x - mean(x))^2))
}

.divisors <- function(n) which(n %% seq_len(n) == 0L)

.validate_trial <- function(trial) {
  if (!(is.matrix(trial) || is.data.frame(trial)))
    stop("Input must be a numeric matrix (rows x columns) or a fully numeric data.frame.", call. = FALSE)
  if (is.data.frame(trial) && !all(vapply(trial, is.numeric, logical(1)))) {
    bad_cols <- names(trial)[!vapply(trial, is.numeric, logical(1))]
    stop("Nonnumeric columns: ", paste(bad_cols, collapse = ", "),
         ". Input must contain numeric yields with column headers but no row-label column.",
         call. = FALSE)
  }
  M <- as.matrix(trial)
  if (!is.numeric(M) || length(dim(M)) != 2L || any(dim(M) < 2L))
    stop("Expected a numeric matrix with at least 2 rows x 2 columns. Received: ",
         paste(dim(M), collapse = " x "), call. = FALSE)
  if (any(!is.finite(M)) || any(M < 0))
    stop("The matrix contains NA, Inf, or negative yields.", call. = FALSE)
  if (mean(M) <= 0)
    stop("Mean yield must be positive to calculate the CV.", call. = FALSE)
  rownames(M) <- NULL
  M
}

.validate_positive_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0)
    stop(sprintf("'%s' must be a finite positive number.", name), call. = FALSE)
}

.validate_parameters <- function(a, m, n) {
  .validate_positive_scalar(a, "a")
  .validate_positive_scalar(m, "m")
  .validate_positive_scalar(n, "n")
}
