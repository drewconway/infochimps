\name{trstrank}
\alias{trstrank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the trstrank score for a given Twitter user
}
\description{
A function to return infochimps.com trstrank score for a Twitter user
}
\usage{
trstrank(screen.name, user.id = NA)
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
A list with the following elements:
\item{user_id}{A Twitter user ID (numeric)}
\item{screen_name}{Screen name of a Twitter user (character)}
\item{trstrank}{trstrank score (numeric)}
\item{tq}{trstrank quotient (numeric)}
}
\references{
http://api.infochimps.com/describe/soc/net/tw/trstrank
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
trstrank("drewconway")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
