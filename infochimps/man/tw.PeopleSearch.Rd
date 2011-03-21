\name{tw.PeopleSearch}
\alias{tw.PeopleSearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Full text search, on any or all fields, of twitter user profiles.
}
\description{
A function to return infochimps.com Twitter people search.  Full text search, on any or all fields, of twitter user profiles.
}
\usage{
tw.PeopleSearch(search.string, from=0, to.df=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{search.string}{
The term to search for. You may use "field:term" to search for that term in only a given field (or even "field.subfield:term" if there are nested fields
}
  \item{from}{
Integer offset to start results at. A multiple of 100.
}
  \item{to.df}{
Option to return search query as a data frame.  Default is TRUE, and if FALSE returned as list keyed by data values.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame with the following data columns (all as characters):
\item{user_id}{A Twitter user ID}
\item{scraped_at}{When the the infochimps flying monkeys scraped the user"s profile from the twitter api (YYYYMMddhhmmss)}
\item{screen_name}{Screen name of a Twitter user}
\item{url}{User"s web site}
\item{location}{User"s hand entered, raw text, location}
\item{description}{[unspecified]}
\item{time_zone}{Time zone as tzinfo compatible string}
\item{utc_offset}{[unspecified]}
\item{lang}{two letter language code, eg. "en"}
\item{geo_enabled}{The string "true" if the user has enabled geo location, "false" otherwise}
\item{verified}{Is this a twitter verified account? "true" or "false"}
\item{contributors_enabled}{If the twitter account is a pro account allowing multiple users to control visit dev.twitter.com for details}
}
\references{
http://www.infochimps.com/datasets/twpeoplesearch
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
search.test <- tw.PeopleSearch("R hacks")
head(search.test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
