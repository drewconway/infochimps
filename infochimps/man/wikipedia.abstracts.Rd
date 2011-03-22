\name{wikipedia.abstratcs}
\alias{wikipedia.abstratcs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search Wikipedia articles by title, by abstract content, or by both.
}
\description{
Provides access to infochimps.com search API of Wikipedia article abstracts.
}
\usage{
wikipedia.abstratcs(search.string, from=0, to.df=TRUE)
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
\item{title}{Wikipedia article title}
\item{url}{Web url of wikipedia article}
\item{abstract}{Text content of wikipedia article abstract}
}
\references{
http://www.infochimps.com/datasets/wikipedia-articles-abstract-search
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
wiki.stats <- wikipedia.abstratcs("statistical computing")
head(wiki.stats)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
