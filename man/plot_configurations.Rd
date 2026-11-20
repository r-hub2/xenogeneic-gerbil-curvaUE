\name{plot_configurations}
\alias{plot_configurations}
\title{Enumerate admissible experimental-unit geometries}
\description{Forms all exact rectangular and square partitions with at least two experimental units. Smith uses sample variance regardless of the optional CV denominator.}
\usage{
plot_configurations(
  trial, unit_width_m = 1, unit_length_m = 1,
  sd = c("sample", "population"), min_plots = 2L
)
}
\arguments{
\item{trial}{Numeric yield matrix.}
\item{unit_width_m,unit_length_m}{Physical basic-unit width and length in metres; only output conversion.}
\item{sd}{sample (default) or population for descriptive CV.}
\item{min_plots}{Minimum number of experimental units per configuration (at least 2).}
}
\value{A data frame of configurations, conventional and auxiliary variances, and CVs.}
