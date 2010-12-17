\name{ip.geo}
\alias{ip.geo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IP address geo-location
}
\description{
A function to return Digitial Elements IP Intelligence geo-loaction data from the infochimps.com API
}
\usage{
ip.geo(ip.address)
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
http://api.infochimps.com/describe/web/an/de/geo
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
nyu<-ip.geo("128.122.79.165")

## The function is currently defined as
function(ip.address) {
    geo.url<-paste(.InfochimpsEnv$data$de,"geo.json?ip=",ip.address,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    geo.get<-getURL(geo.url)
    geo.data<-fromJSON(geo.get)
    if(is.null(geo.data$error)) {
        return(geo.data)
    }
    else {
        warning(geo.data$message[[1]])
        return(NA)
    }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
