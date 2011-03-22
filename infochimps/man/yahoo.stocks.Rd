\name{yahoo.stocks}
\alias{yahoo.stocks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search historical stock market data.
}
\description{
This dataset consists of historical stock price data extracted from Yahoo Finance using the ichart (http://finance.yahoo.com/) service.  The selection of stock symbols is gathered from nasdaq.com.
}
\usage{
yahoo.stocks(symbol, begin.date, end.date, from=0, to.df=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbol}{
The stock symbol
}
  \item{from}{
Integer offset to start results at. A multiple of 100.
}
  \item{begin.date}{
The first date in the time-series, formatted as YYYYMMdd
}
  \item{end.date}{
The last date in the time-series, formatted as YYYYMMdd
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
\item{exchange}{Exchange on which this stock is traded}
\item{symbol}{Symbol for this stock}
\item{date}{The date, formatted as YYYYMMdd}
\item{open}{Opening price of this stock}
\item{close}{Closing price of this stock}
\item{adj_close}{Adjusted Closing Price}
\item{low}{Lowest price of this stock sold on date}
\item{high}{Highest price of this stock sold on date}
\item{volume}{Number of shares sold on this day in units of 1000}
}
\references{
http://www.infochimps.com/datasets/yahoo-stock-search
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
appl.stock <- yahoo.stocks("aapl", begin.date="20010801", end.date="20010901")
head(appl.stock)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
