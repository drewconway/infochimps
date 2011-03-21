\name{geo.names}
\alias{geo.names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Autocomplete any place name in the world.
}
\description{
Place names request. A prefix to autocomplete. One or both of country and state can be supplied
to constrain the results.
}
\usage{
geo.names(prefix, country="", state="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prefix}{
  A simple prefix, eg. "port"
  }
  \item{country}{
  An (optional) string ISO-3166 2-letter country code, 2 characters
  }
  \item{state}{
  Administrative subdivision one code (eg. TX or AK when country is US)
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a character vector of all completions given search criteria.
}
\references{
http://www.infochimps.com/datasets/global-place-names-autocomplete-geonames
}
\author{
Drew Conway <drew.conway@nyu.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
infochimps("your.api.key")
ny.cities <- geo.names("city", country="US", state="NY")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
