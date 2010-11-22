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
strong.links(screen.name, session, user.id = NA)
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
my.infochimps<-infochimps("your.api.key")
drew.links<-strong.links("drewconway",my.infochimps)
head(drew.links)

## The function is currently defined as
function(screen.name,session,user.id=NA) { 
    if(is.na(user.id)) {
        strong.url<-paste(session$base.url,"strong_links.json?screen_name=",screen.name,"&apikey=",session$api.key,sep="")
    }
    else{
        strong.url<-paste(session$base.url,"strong_links.json?user_id=",user.id,"&apikey=",session$api.key,sep="")
    }
    strong.get<-getURL(strong.url)
    strong.data<-fromJSON(strong.get)
    # Simple error checking
    if(is.null(strong.data$error)){
        strong.edges<-do.call("rbind",strong.data$strong_links)
        strong.edges<-cbind(strong.data$user_id,strong.edges)
        strong.df<-as.data.frame(strong.edges)
        names(strong.df)<-c("user.id","strong.edge","link.weight")
        return(strong.df)
    }
    else{
        warning(strong.data$message[[1]])
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

