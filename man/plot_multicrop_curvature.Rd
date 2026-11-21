\name{plot_multicrop_curvature}
\alias{plot_multicrop_curvature}
\title{Plot multi-crop CV contours and experimental-unit criteria}
\description{Creates faceted CV contours and the two sectional-curvature loci with
  markers for Gaussian, sectional and marginal-efficiency solutions. Dashed guides and
  numerical labels refer only to the sectional solution.}
\usage{
plot_multicrop_curvature(
  trials = NULL,
  min_plots = 2L,
  unit_width_m = 1, unit_length_m = 1,
  cv_interval = 5, grid_size = 180L, target_slope = 1
)
}
\arguments{
\item{trials}{Named list of trial matrices. NULL uses the four example crops.}
\item{min_plots}{Minimum number of experimental units per configuration.}
\item{unit_width_m,unit_length_m}{Physical dimensions of one basic unit, in metres.}
\item{cv_interval}{CV contour interval in percentage points.}
\item{grid_size}{Number of grid points per axis.}
\item{target_slope}{Positive target marginal CV decrease in percentage points per
  basic unit.}
}
\value{A faceted ggplot object.}
