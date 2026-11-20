\name{fit_smith}
\alias{fit_smith}
\title{Estimate Smith heterogeneity using weighted log variance}
\description{The main method is Smith (1938) WLS with weights M-1 and a free intercept; b_prime applies to the finite observed field.}
\usage{
fit_smith(configurations, method = c("smith1938", "nls", "log", "unweighted_log"))
}
\arguments{
\item{configurations}{Output of plot_configurations().}
\item{method}{smith1938 (default), nls, log, or unweighted_log.}
}
\value{A list with b_prime, model, weights, and fit diagnostics.}
\examples{
cfg <- plot_configurations(example_trial("rice"))
fit_smith(cfg)$b_prime
}
