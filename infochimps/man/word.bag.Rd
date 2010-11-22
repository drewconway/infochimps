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
word.bag(screen.name, session, user.id = NA)
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
my.infochimps<-infochimps("your.api.key")
hilary<-word.bag("hmason",my.infochimps)

## The function is currently defined as
function(screen.name,session,user.id=NA) {
    if(is.na(user.id)) {
        wordbag.url<-paste(session$base.url,"wordbag.json?screen_name=",screen.name,"&apikey=",session$api.key,sep="")
    }
    else{
        wordbag.url<-paste(session$base.url,"wordbag.json?user_id=",user.id,"&apikey=",session$api.key,sep="")
    }
    wordbag.get<-getURL(wordbag.url)
    wordbag.data<-fromJSON(wordbag.get)
    if(is.null(wordbag.data$error)) {
        # Get wordbag data
        words<-do.call("rbind", wordbag.data$toks)
        words.df<-as.data.frame(cbind(wordbag.data$user_id[[1]],words))
        names(words.df)<-c("user.id","rel.freq","tok","user.freq.ppb")
        words.list<-list(user.id=wordbag.data$user_id[[1]],vocab=wordbag.data$vocab[[1]],total.usages=wordbag.data$total_usages[[1]],tok.df=words.df)
        return(words.list)
    }
    else {
        warning(wordbag.data$message[[1]])
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
