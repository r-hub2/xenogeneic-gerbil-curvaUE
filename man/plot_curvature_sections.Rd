\name{plot_curvature_sections}
\alias{plot_curvature_sections}
\title{Plot coordinate sections through selected solutions}
\description{Shows CV profiles through one selected solution or all three solutions
  overlaid.}
\usage{
plot_curvature_sections(
  surface, criterion = c("sectional", "gaussian", "marginal", "all"),
  configurations = NULL, target_slope = 1
)
}
\arguments{
\item{surface}{Output of \code{fit_cv_surface()}.}
\item{criterion}{Either \code{"sectional"} (default), \code{"gaussian"},
  \code{"marginal"} or \code{"all"}.}
\item{configurations}{Optional table of configurations to determine the displayed
  domain.}
\item{target_slope}{Positive target marginal CV decrease in percentage points per
  basic unit.}
}
\value{A ggplot object. For \code{criterion = "all"}, sections through each point are
  distinguished by color.}
