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
conversations(screen.name.a, screen.name.b, session, user.id.a = NA, user.id.b = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{screen.name.a}{
The name of a Twitter user
}
  \item{screen.name.b}{
The name of a Twitter user
}
  \item{session}{
Object containing session API key information, created with infochimps() function
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
my.infochimps<-infochimps("my.api.key")
jd.tweets<-conversations("drewconway","CMastication",my.infochimps)
head(jd.tweets)

## The function is currently defined as
function(screen.name.a,screen.name.b,session,user.id.a=NA,user.id.b=NA) {
    # Determine the form of the API request
    if(is.na(user.id.a) & is.na(user.id.a)) {
        conversation.url<-paste(session$base.url,"conversation.json?user_a_sn=",screen.name.a,"&user_b_sn=",screen.name.b,"&apikey=",session$api.key,sep="")
    }
    else {
        if(is.na(user.id.a)==FALSE & is.na(user.id.a)==FALSE) {
            conversation.url<-paste(session$base.url,"conversation.json?user_a_id=",user.id.a,"&user_b_id=",user.id.b,"&apikey=",session$api.key,sep="")
        }
        else {
            if(is.na(user.id.na)) {
                conversation.url<-paste(session$base.url,"conversation.json?user_a_sn=",screen.name.a,"&user_b_id=",user.id.b,"&apikey=",session$api.key,sep="")
            }
            else {
                conversation.url<-paste(session$base.url,"conversation.json?user_a_id=",user.id.a,"&user_b_sn=",screen.name.b,"&apikey=",session$api.key,sep="")
            }
        }
    }
    conversation.get<-getURL(conversation.url)
    conversation.data<-fromJSON(conversation.get)
    # Simple error checking
    if(is.null(conversation.data$error)) {
        user.id.a<-conversation.data$user_a_id[[1]]
        user.id.b<-conversation.data$user_b_id[[1]]
        conversations.matrix<-suppressWarnings(do.call("rbind", conversation.data$conversations))
        reply.to<-sapply(1:nrow(conversations.matrix), function(x) ifelse(conversations.matrix[x,2]=="re", conversations.matrix[x,3], NA))
        conversations.df<-cbind(user.id.a, user.id.b, conversations.matrix[,1], conversations.matrix[,2],reply.to)
        conversations.df<-as.data.frame(conversations.df)
        names(conversations.df)<-c("user.id.a","user.id.b","conversation.id","conversation.type","reply.to.id")
        return(conversations.df)
    }
    else {
        warning(conversation.data$message[[1]])
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
