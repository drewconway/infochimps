\name{word.bag}
\alias{word.bag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the words most associated with a given Twitter user
}
\description{
A function to return infochimps.com Word Bag for a Twitter user
}
\usage{
word.bag(screen.name, user.id = NA)
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
\item{user_id}{Twitter used ID (numeric)}
\item{vocab}{Number of distinct tokens ever emitted (numeric)}
\item{total.usages}{Total number of tokens emitted (numeric)}
\item{tok.df}{Data frame with columns: user.id (numeric), rel.freq (numeric), tok user (character), freq.ppb (numeric)}

If user.name not found, return NA
}
\references{
http://api.infochimps.com/describe/soc/net/tw/wordbag
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
hilary<-word.bag("hmason")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
