\name{plot_cv_surface}
\alias{plot_cv_surface}
\title{Plot the CV surface in 3D}
\description{Draws a perspective plot of the CV surface with curvature solutions.}
\usage{
plot_cv_surface(surface, configurations, theta = -35, phi = 25)
}
\arguments{
\item{surface}{Output of fit_cv_surface().}
\item{configurations}{Output of plot_configurations().}
\item{theta,phi}{Perspective viewing angles.}
}
\value{Draws a base-R plot and returns invisibly.}
