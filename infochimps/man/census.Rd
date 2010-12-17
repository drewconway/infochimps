\name{census}
\alias{census}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gather U.S. Census data for a given IP address
}
\description{
A function to return combined census data for a given IP address using the inforchimps.com APIs
}
\usage{
census(ip.address)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ip.address}{
Properly formatted IP address as character string
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
list : see reference for listing of all data returned (extensive)
}
\references{
http://api.infochimps.com/describe/web/an/ip_census/combined
}
\author{
Drew Conway, <drew.conway@nyu.edu>
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
nyu<-census("128.122.79.165")

## The function is currently defined as
function(ip.address) {
    census.url<-paste(.InfochimpsEnv$data$ip,"combined.json?ip=",ip.address,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    census.get<-getURL(census.url)
    census.data<-fromJSON(census.get)
    if(is.null(census.data$error)) {
        return(census.data)
    }
    else {
        warning(census.data$message[[1]])
        return(NA)
    }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
