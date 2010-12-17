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

## The function is currently defined as
function(screen.name,user.id=NA) {
    if(is.na(user.id)) {
        wordbag.url<-paste(.InfochimpsEnv$data$base,"wordbag.json?screen_name=",screen.name,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    }
    else{
        wordbag.url<-paste(.InfochimpsEnv$data$base,"wordbag.json?user_id=",user.id,"&apikey=",.InfochimpsEnv$data$api.key,sep="")
    }
    wordbag.get<-getURL(wordbag.url)
    wordbag.data<-fromJSON(wordbag.get)
    if(is.null(wordbag.data$error)) {
        # Get wordbag data
        words<-do.call("rbind", wordbag.data$toks)
        words.df<-as.data.frame(cbind(wordbag.data$user_id[[1]],words), stringsAsFactors=FALSE)
        words.names<-c("user.id","rel.freq","tok","user.freq.ppb")
        names(words.df)<-words.names
        for(c in 1:length(word.names)) {words.df[,c]<-unlist(words.df[,c])}
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
