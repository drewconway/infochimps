\name{strong.links}
\alias{strong.links}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find all of the Strong Links of a given Twitter user
}
\description{
A function to return infochimps.com Strong Links data 
}
\usage{
strong.links(screen.name, user.id = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{screen.name}{
The name of a Twitter user
}
  \item{user.id}{
a Twitter user ID
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame with the following columns:
\item{user.id}{Twitter user ID (numeric)}
\item{strong.link}{Twitter user ID with Strong Link (numeric)}
\item{link.weight}{Strength of Strong Link (numeric)}

If user.name not found, return NA
}
\references{
http://api.infochimps.com/describe/soc/net/tw/strong_links
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
drew.links<-strong.links("drewconway")
head(drew.links)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

