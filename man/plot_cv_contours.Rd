\name{plot_cv_contours}
\alias{plot_cv_contours}
\title{Plot CV-surface contours}
\description{Displays CV contours and both curvature solutions.}
\usage{
plot_cv_contours(surface, configurations, grid_size = 120)
}
\arguments{
\item{surface}{Output of fit_cv_surface().}
\item{configurations}{Output of plot_configurations().}
\item{grid_size}{Number of grid points per axis.}
}
\value{A ggplot object.}
