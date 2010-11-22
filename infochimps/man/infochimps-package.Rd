\name{infochimps-package}
\alias{infochimps-package}
\docType{package}
\title{
An R wrapper for the infochimps.com API services
}
\description{
This package provides functions to access all of the APIs currently available infochimps.com.
}
\details{
\tabular{ll}{
Package: \tab infochimps\cr
Type: \tab Package\cr
Version: \tab 0.1.1\cr
Date: \tab 2010-11-20\cr
License: \tab BSD\cr
LazyLoad: \tab yes\cr
}

}
\author{
Drew Conway <drew.conway@nyu.edu>
}
\references{
http://api.infochimps.com/.
}

\keyword{ package }
\examples{
library(infochimps)

my.infochimps<-infochimps("your.api.key")
drew<-influence("drewconway",my.infochimps)
}
