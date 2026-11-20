#' Read an experimental yield grid from CSV or Windows clipboard
#'
#' Accepts column headers only; rows are assigned internally. Returns a numeric matrix.
#' @param path CSV path or "clipboard" on Windows.
#' @param sep Column delimiter: auto, comma, semicolon, or tab.
#' @param dec Decimal point: auto, period, or comma.
#' @return A numeric matrix containing yields in their original spatial positions.
#' @export
read_trial_matrix <- function(path, sep = "auto", dec = "auto") {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path)) stop("'path' must be a nonempty CSV file path.", call. = FALSE)
  is_clipboard <- identical(tolower(path), "clipboard")
  if (is_clipboard && .Platform$OS.type != "windows")
    stop("Clipboard input is only available on Windows; use a CSV file.", call. = FALSE)
  if (!is_clipboard && !file.exists(path))
    stop("File not found: ", path, call. = FALSE)
  allowed <- c("auto", ",", ";", "\t")
  if (!sep %in% allowed)
    stop("'sep' must be 'auto', ',', ';', or a tab character.", call. = FALSE)
  if (!dec %in% c("auto", ".", ","))
    stop("'dec' must be 'auto', '.' or ','.", call. = FALSE)
  if (is_clipboard && sep == "auto") sep <- "\t"
  if (sep == "auto") {
    first_line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
    if (!length(first_line)) stop("The file is empty.", call. = FALSE)
    counts <- vapply(c(",", ";", "\t"), function(s)
      lengths(regmatches(first_line, gregexpr(s, first_line, fixed = TRUE)))[1L], integer(1))
    if (max(counts) < 1L)
      stop("No column separator was detected. Include a header row.", call. = FALSE)
    sep <- c(",", ";", "\t")[[which.max(counts)]]
  }
  if (dec == "auto") {
    dec <- if (sep == ";") "," else "."
    if (sep == "\t") {
      sample_lines <- readLines(path, n = 3L, warn = FALSE)
      if (length(sample_lines) > 1L) {
        values <- unlist(strsplit(paste(sample_lines[-1L], collapse = "\t"), "\t", fixed = TRUE))
        if (any(grepl("^[+-]?[0-9]+,[0-9]+$", trimws(values)))) dec <- ","
      }
    }
  }
  table_data <- tryCatch(
    utils::read.table(path, header = TRUE, sep = sep, dec = dec,
                      check.names = FALSE, row.names = NULL, fill = FALSE,
                      stringsAsFactors = FALSE, na.strings = c("", "NA"),
                      quote = '\"', comment.char = "", strip.white = TRUE,
                      fileEncoding = if (is_clipboard) "" else "UTF-8-BOM"),
    error = function(e) stop("Could not read the trial matrix: ", conditionMessage(e),
                              ". Check headers, delimiters, and decimal marks.", call. = FALSE))
  if (nrow(table_data) < 2L || ncol(table_data) < 2L)
    stop("The matrix needs at least 2 rows and 2 columns; found ",
         nrow(table_data), " rows x ", ncol(table_data), " columns.", call. = FALSE)
  column_names <- trimws(names(table_data))
  if (anyNA(column_names) || any(!nzchar(column_names)) || anyDuplicated(column_names))
    stop("Column headers must be unique and nonempty (e.g., C1,C2,...).", call. = FALSE)
  if (tolower(column_names[1L]) %in% c("row", "rows", "fila", "filas", "id", "index", "indice", "row.names", "rownames"))
    stop("A row-index column was detected. Remove it; row indices are created internally.", call. = FALSE)
  if (all(grepl("^[0-9]+([.,][0-9]+)?$", column_names)))
    stop("The first row appears to contain observations instead of column headers. Add headers C1,C2,...", call. = FALSE)
  is_numeric <- vapply(table_data, is.numeric, logical(1))
  if (!all(is_numeric))
    stop("The matrix contains nonnumeric columns: ",
         paste(column_names[!is_numeric], collapse = ", "),
         ". Check separators and remove row labels.", call. = FALSE)
  first_column <- as.numeric(table_data[[1L]])
  if (ncol(table_data) >= 2L && identical(first_column, as.numeric(seq_len(nrow(table_data)))))
    stop("The first column is 1,2,...,n and appears to be a row index. Remove it before import.", call. = FALSE)
  M <- .validate_trial(as.matrix(table_data))
  colnames(M) <- column_names
  M
}

#' Validate the dimensions of an experimental yield grid
#'
#' Checks the numeric matrix before modeling.
#' @param trial Numeric rows-by-columns yield matrix.
#' @return A one-row data frame with dimensions, number of basic units, and range.
#' @export
validate_trial_matrix <- function(trial) {
  M <- .validate_trial(trial)
  data.frame(rows = nrow(M), columns = ncol(M),
             n_ub = length(M), minimum = min(M), maximum = max(M))
}

#' Convert a spatial grid into long-format observations
#'
#' Adds row and column indices without changing spatial cell positions.
#' @param trial Numeric yield matrix.
#' @return A data frame with row, column, and yield.
#' @export
as_long_trial_data <- function(trial) {
  M <- .validate_trial(trial)
  data.frame(row = rep(seq_len(nrow(M)), times = ncol(M)),
             column = rep(seq_len(ncol(M)), each = nrow(M)),
             yield = as.numeric(M))
}

#' Load one of four example uniformity trials
#'
#' Loads rice (20x20), maize (20x20), common bean (10x20), or cowpea (10x20).
#' @param crop One of rice, maize, common_bean, or cowpea.
#' @return A numeric yield matrix.
#' @export
example_trial <- function(crop = c("rice", "maize", "common_bean", "cowpea")) {
  crop <- match.arg(crop)
  path <- system.file("extdata", paste0("trial_", crop, ".csv"), package = "curvaUE")
  if (!nzchar(path))
    stop("The example CSV is missing for crop: ", crop, call. = FALSE)
  read_trial_matrix(path)
}
