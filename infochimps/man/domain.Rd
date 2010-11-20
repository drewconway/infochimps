\name{domain}
\alias{domain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return domain information for a given domian
}
\description{
A function to return Digitial Elements IP domain data from the infochimps.com API
}
\usage{
domain(ip.address, session)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ip.address}{
Properly formatted IP address as character string
}
  \item{session}{
Object containing session API key information, created with infochimps() function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing the following elements:
\item{domain}{Domain name (character)}
\item{company}{Registered company name (character)}
\item{isp}{Internet service provider (character)}
\item{proxy_type}{Proxy type (character)}
\item{naics_code}{NAICS Code (numeric)}
}
\references{
http://api.infochimps.com/describe/web/an/de/domain
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
my.infochimps<-infochimps("your.api.key")
nyu<-domain("128.122.79.165",my.infochimps)

## The function is currently defined as
function(ip.address,session) {
    domain.url<-paste(de.url,"domain.json?ip=",ip.address,"&apikey=",session$api.key,sep="")
    domain.get<-getURL(domain.url)
    domain.data<-fromJSON(domain.get)
    if(is.null(domain.data$error)) {
        return(domain.data)
    }
    else {
        warning(domain.data$message[[1]])
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

