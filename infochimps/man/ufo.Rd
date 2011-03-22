\name{ufo}
\alias{ufo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search accounts of UFO sightings from the National UFO Reporting Center.
}
\description{
A function to return infochimps.com data on international UFO sightings.

Data is from a comprehensive listing of over 60,000 unidentified-flying-object (UFO) sightings in the United States (and some elsewhere).  Fields of particular interest are the shape, which describes the shape of the UFO encountered as well as the description which is free-form descriptive text about the event.
}
\usage{
ufo(search.string, from=0, to.df=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{search.string}{
  The term to search for. You may use 'field:term' to search for that term in only a given field (or even 'field.subfield:term' if there are nested fields
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
\item{sighted_at}{Date ufo sighting occurred (YYYYMMdd)}
\item{reported_at}{Date ufo sighting was reported (YYYYMMdd)}
\item{location}{City and two letter state abbreviation sighting occurred in (City, State)}
\item{shape}{Shape of the ufo. One of [changed, changing, chevron, cigar, circle, cone, crescent, cross, cylinder, delta, diamond,
disk, dome, egg, fireball, flare, flash, formation, hexagon, light, other, pyramid, rectangle, round, sphere, teardrop,
triangle, unknown]}
\item{duration}{Human entered duration, eg. 2 min.}
\item{description}{Full text description of the sighting.}
}
\references{
http://www.infochimps.com/datasets/60000-documented-ufo-sightings-with-text-descriptions-and-metada
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
cow.test <- ufo("cow field")
head(cow.test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
