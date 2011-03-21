\name{tw.Autocomplete}
\alias{tw.Autocomplete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matches partial Twitter user names for completion.
}
\description{
Uses the same type of autocomplete that the Twitter iPhone app uses. Given a twitter screen name prefix returns up to 100 possible screen name completions sorted by trstrank.
}
\usage{
tw.Autocomplete(prefix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prefix}{
A three-character or more prefix of a Twitter screen name
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a character vector of all completions given search criteria.
}
\references{
http://www.infochimps.com/datasets/twitter-screen-name-autocomplete
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
tw.Autocomplete("drew")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
