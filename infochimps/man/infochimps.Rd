\name{infochimps}
\alias{infochimps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an infochimps.com API session.
}
\description{
List object to hold a user's API key, as well as all API URLS.  Needed in all functions
}
\usage{
infochimps(api.key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{api.key}{A valid infochimps.com API key}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{api.key}{A valid infochimps.com API key}
}
\references{
To get an API key from infochimps.com see, http://api.infochimps.com/about/features-and-pricing
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

## The function is currently defined as
function(api.key) {
    if(is.character(api.key)){
        .InfochimpsEnv$data$api.key<-api.key   
    }
    else{
        warning("API key must be a string")
    }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{list}

