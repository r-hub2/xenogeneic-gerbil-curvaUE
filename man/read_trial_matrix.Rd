\name{read_trial_matrix}
\alias{read_trial_matrix}
\title{Read an experimental yield grid from CSV or Windows clipboard}
\description{Accepts column headers only; rows are assigned internally. Returns a numeric matrix.}
\usage{
read_trial_matrix(path, sep = "auto", dec = "auto")
}
\arguments{
\item{path}{CSV path or "clipboard" on Windows.}
\item{sep}{Column delimiter: auto, comma, semicolon, or tab.}
\item{dec}{Decimal point: auto, period, or comma.}
}
\value{A numeric matrix containing yields in their original spatial positions.}
\examples{
path <- system.file("extdata", "trial_rice.csv", package = "curvaUE")
M <- read_trial_matrix(path)
dim(M)
}
