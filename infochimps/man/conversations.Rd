\name{conversations}
\alias{conversations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create data frame of recent conversations between two Twitter users
}
\description{
A function to return the interactions between two Twitter users with infochimps.com API
}
\usage{
conversations(screen.name.a, screen.name.b, user.id.a = NA, user.id.b = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{screen.name.a}{
The name of a Twitter user
}
  \item{screen.name.b}{
The name of a Twitter user
}
  \item{user.id.a}{
a Twitter user ID
}
  \item{user.id.b}{
a Twitter user ID
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Data frame with the following columns:
\item{user.id.a}{First Twitter user (numeric)}
\item{user.id.b}{Second Twitter user (numeric)}
\item{conversation.id}{Internal Twitter ID for tweet (numeric)}
\item{conversation.type}{Factor describing conversation type (factor). See ref.}
\item{reply.to.id }{If RE type, internal Twitter ID for reply-to tweet (numeric)}
                
If user.name not found, or no data, return NA
}
\references{
http://api.infochimps.com/describe/soc/net/tw/conversation
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
infochimps("my.api.key")
jd.tweets<-conversations("drewconway","CMastication")
head(jd.tweets)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
