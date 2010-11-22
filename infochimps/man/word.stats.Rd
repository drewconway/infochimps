\name{word.stats}
\alias{word.stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get basic statistics associated with a given word on Twitter
}
\description{
A function to return infochimps.com Word Stats data 
}
\usage{
word.stats(tok, session)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tok}{
The word you are searching (character)
}
  \item{session}{
Object containing session API key information, created with infochimps() function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with the following elements:
\item{global_stdev_ppb}{Standard deviation (numeric)}
\item{range}{Range (numeric)}
\item{tok}{The word (character)}
\item{global_freq_ppb}{Global frequency in parts-per-billion (numeric)}

If tok not found, return NA
}
\references{
http://api.infochimps.com/describe/soc/net/tw/word_stats
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
word.stats("infochimps",my.infochimps)

## The function is currently defined as
function(tok,session) {
    word.url<-paste(session$base.url,"word_stats.json?tok=",tok,"&apikey=",session$api.key,sep="")
    word.get<-getURL(word.url)
    word.data<-fromJSON(word.get)
    # Simple error checking
    if(is.null(word.data$error)) {
        return(word.data)
    }
    else {
        warning(word.data$message[[1]])
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
