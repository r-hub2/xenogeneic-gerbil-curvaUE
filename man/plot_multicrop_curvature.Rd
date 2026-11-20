\name{plot_multicrop_curvature}
\alias{plot_multicrop_curvature}
\title{Plot four-crop sectional-curvature contours}
\description{Creates faceted CV contours, the two sectional-curvature loci, and their intersection.}
\usage{
plot_multicrop_curvature(
  trials = NULL, min_plots = 2L,
  unit_width_m = 1, unit_length_m = 1,
  cv_interval = 5, grid_size = 180L
)
}
\arguments{
\item{trials}{Named list of trial matrices. NULL uses the four example crops.}
\item{min_plots}{Minimum number of experimental units per configuration.}
\item{unit_width_m,unit_length_m}{Physical dimensions of one basic unit, in metres.}
\item{cv_interval}{CV contour interval in percentage points.}
\item{grid_size}{Number of grid points per axis.}
}
\value{A faceted ggplot object.}
