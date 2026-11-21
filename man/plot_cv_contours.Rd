\name{plot_cv_contours}
\alias{plot_cv_contours}
\title{Plot CV-surface contours}
\description{Displays fitted CV contours and the Gaussian, simultaneous sectional and
  coordinate-wise marginal-efficiency solutions.}
\usage{
plot_cv_contours(surface, configurations, grid_size = 120,
                 target_slope = 1)
}
\arguments{
\item{surface}{Output of \code{fit_cv_surface()}.}
\item{configurations}{Output of \code{plot_configurations()}.}
\item{grid_size}{Number of grid points per axis.}
\item{target_slope}{Positive target marginal CV decrease (percentage points per basic
  unit) used for the marginal-efficiency point.}
}
\value{A ggplot object. The chart extends beyond the observed configuration range when
  needed to display a model-based solution, which then represents extrapolation.}
