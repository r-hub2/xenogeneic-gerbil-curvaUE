\name{analyze_uniformity_trial}
\alias{analyze_uniformity_trial}
\title{Analyze an experimental uniformity trial}
\description{Integrates input checks, configurations, weighted Smith estimate, CV surface, and both curvature solutions.}
\usage{
analyze_uniformity_trial(
  trial, unit_width_m = 1, unit_length_m = 1,
  sd = "sample", min_plots = 2L,
  criterion = c("both", "gaussian", "sectional"),
  smith_method = c("smith1938", "nls", "log", "unweighted_log")
)
}
\arguments{
\item{trial}{Numeric yield matrix.}
\item{unit_width_m,unit_length_m}{Physical dimensions of one basic unit, in metres.}
\item{sd}{sample or population for descriptive CV.}
\item{min_plots}{At least 2 experimental units per configuration.}
\item{criterion}{both, gaussian, or sectional.}
\item{smith_method}{Smith estimator method; default smith1938.}
}
\value{A list with descriptives, dimensions, long_data, smith, surface, configurations, and solution.}
\examples{
result <- analyze_uniformity_trial(example_trial("rice"))
result$solution
}
