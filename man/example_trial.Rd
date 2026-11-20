\name{example_trial}
\alias{example_trial}
\title{Load one of four example uniformity trials}
\description{Loads rice (20x20), maize (20x20), common bean (10x20), or cowpea (10x20).}
\usage{
example_trial(crop = c("rice", "maize", "common_bean", "cowpea"))
}
\arguments{
\item{crop}{One of rice, maize, common_bean, or cowpea.}
}
\value{A numeric yield matrix.}
\examples{
dim(example_trial("rice"))
}
