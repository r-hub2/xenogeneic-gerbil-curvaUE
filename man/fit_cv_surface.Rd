\name{fit_cv_surface}
\alias{fit_cv_surface}
\title{Fit a bivariate power-law CV surface}
\description{Fits CV=a*width^(-m)*length^(-n) by unweighted nonlinear least squares, with dimensions in basic units.}
\usage{
fit_cv_surface(configurations)
}
\arguments{
\item{configurations}{Output of plot_configurations().}
}
\value{A list with a, m, n, model, residuals, and descriptive RMSE.}
