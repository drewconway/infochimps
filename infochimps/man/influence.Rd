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
influence(screen.name, session, user.id = NA)
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
my.infochimps<-infochimps("your.api.key")
drew<-influence("drewconway",my.infochimps)

## The function is currently defined as
function(screen.name,session,user.id=NA) {
    if(is.na(user.id)) {
        influence.url<-paste(base.url,"influence.json?screen_name=",screen.name,"&apikey=",session$api.key,sep="")
    }
    else{
        influence.url<-paste(base.url,"influence.json?user_id=",user.id,"&apikey=",session$api.key,sep="")
    }
    influence.get<-getURL(influence.url)
    influence.data<-fromJSON(influence.get)
    # Simple error checking
    if(is.null(influence.data$error)){
        return(influence.data)
    }
    else {
        warning(influence.data$message[[1]])
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
