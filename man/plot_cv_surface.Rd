\name{plot_cv_surface}
\alias{plot_cv_surface}
\title{Plot the CV surface in 3D}
\description{Draws a fitted CV-surface perspective plot with markers for the three
  criteria.}
\usage{
plot_cv_surface(surface, configurations, theta = -35, phi = 25,
                target_slope = 1)
}
\arguments{
\item{surface}{Output of \code{fit_cv_surface()}.}
\item{configurations}{Output of \code{plot_configurations()}.}
\item{theta,phi}{Perspective viewing angles.}
\item{target_slope}{Positive target marginal CV decrease in percentage points per
  basic unit.}
}
\value{Draws a base-R perspective plot and returns invisibly.}
