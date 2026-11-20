\name{plot_curvature_sections}
\alias{plot_curvature_sections}
\title{Plot coordinate sections at the curvature solution}
\description{Displays width and length sections through the selected solution.}
\usage{
plot_curvature_sections(
  surface, criterion = c("sectional", "gaussian"),
  configurations = NULL
)
}
\arguments{
\item{surface}{Output of fit_cv_surface().}
\item{criterion}{sectional (default) or gaussian.}
\item{configurations}{Optional table of configurations.}
}
\value{A ggplot object.}
