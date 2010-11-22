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
trstrank(screen.name, session, user.id = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{screen.name}{
The name of a Twitter user
}
  \item{session}{
Object containing session API key information, created with infochimps() function
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
my.infochimps<-infochimps("your.api.key")
trstrank("drewconway",my.infochimps)

## The function is currently defined as
function(screen.name,session,user.id=NA) {
    if(is.na(user.id)) {
        trstrank.url<-paste(session$base.url,"trstrank.json?screen_name=",screen.name,"&apikey=",session$api.key,sep="")
    }
    else{
        trstrank.url<-paste(session$base.url,"trstrank.json?user_id=",user.id,"&apikey=",session$api.key,sep="")
    }
    trstrank.get<-getURL(trstrank.url)
    trstrank.data<-fromJSON(trstrank.get)
    # Simple error checking
    if(is.null(trstrank.data$error)) {
        return(trstrank.data)
    }
    else {
        warning(trstrank.data$message[[1]])
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
