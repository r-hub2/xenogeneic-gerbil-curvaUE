\name{analyze_uniformity_trial}
\alias{analyze_uniformity_trial}
\title{Analyze an experimental uniformity trial}
\description{Integrates input checks, configurations, a weighted Smith estimate,
  CV-surface fitting, and the selected curvature or marginal-efficiency solution(s).}
\usage{
analyze_uniformity_trial(
  trial, unit_width_m = 1, unit_length_m = 1,
  sd = "sample", min_plots = 2L,
  criterion = c("both", "gaussian", "sectional", "marginal", "all"),
  smith_method = c("smith1938", "nls", "log", "unweighted_log"),
  target_slope = 1
)
}
\arguments{
\item{trial}{Numeric yield matrix.}
\item{unit_width_m,unit_length_m}{Physical dimensions of one basic unit, in metres.}
\item{sd}{Either \code{"sample"} or \code{"population"} for descriptive CV.}
\item{min_plots}{Minimum number of experimental units per configuration (at least
  two).}
\item{criterion}{Either \code{"both"} (default; Gaussian and sectional),
  \code{"gaussian"}, \code{"sectional"}, \code{"marginal"}, or \code{"all"} (three
  criteria).}
\item{smith_method}{Smith estimator method; \code{"smith1938"} by default.}
\item{target_slope}{Positive target marginal CV decrease in percentage points per
  basic unit along each coordinate. Used when \code{criterion} is \code{"marginal"} or
  \code{"all"}; default 1.}
}
\value{A list with descriptives, dimensions, long_data, smith, surface,
  configurations, solution and basic-unit dimensions. The solution includes widths and
  lengths in basic units and metres.}
\examples{
result <- analyze_uniformity_trial(example_trial("rice"), criterion = "all")
result$solution
}
