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
domain(ip.address)
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
infochimps("your.api.key")
nyu<-domain("128.122.79.165")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

