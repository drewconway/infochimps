\name{influence}
\alias{influence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the level of influence for a given Twitter user
}
\description{
A function to return infochimps.com influence scores for a Twitter user
}
\usage{
influence(screen.name, user.id = NA)
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
list : see reference for listing of all data returned (extensive)
}
\references{
http://api.infochimps.com/describe/soc/net/tw/influence
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
drew<-influence("drewconway")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
