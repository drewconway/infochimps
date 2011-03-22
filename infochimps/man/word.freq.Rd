\name{word.freq}
\alias{word.freq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Word Frequencies From the British National Corpus
}
\description{
The British National Corpus (BNC) is a 100-million-word text corpus of samples of written and spoken English from a wide range of sources. It was compiled as a general corpus (collection of texts) in the field of corpus linguistics. The corpus covers British English the late twentieth century from a wide variety of genres with the intention that it be a representative sample of spoken and written British English of that time.

Of the two parts to the 10-million word spoken corpus, one is a demographic part, containing transcriptions of spontaneous natural conversations made by members of the public and the other a context-governed part, containing transcriptions of recordings made at specific types of meeting and event. All the original recordings transcribed for inclusion in the BNC have been deposited at the British Library Sound Archive.

In this data set, we provide plain text versions of the frequency lists contained in WFWSE. These are raw unedited frequency lists produced by our software and do not contain the many additional notes supplied in the book itself. The lists are tab delimited plain text so can be imported into your preferred spreadsheet format. For the main lists we provide a key to the columns. More details on the process undertaken in the preparation of the lists can be found in the introduction to the book. These lists show dispersion ranging between 0 and 1 rather than 0 and 100 as in the book. We multiplied the value by 100 and rounded to zero decimal places in the book for reasons of space. Log likelihood values are shown here to one decimal place rather than zero as in the book. Please note, all frequencies are per million words.
}
\usage{
word.freq(word)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{word}{
The word being matched against the corpus
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list of characters with the following values:
\item{head_word}{Word type headword, see pp.4-5}
\item{head_word_freq_ppm}{Rounded frequency per million word tokens (down to a minimum of 10 occurrences of a lemma per million), see pp. 5. Where BOTH head word and lemmas appear}
\item{head_word_range}{Range, the number of sectors of the corpus (out of a maximum of 100) in which the word occurs. Where BOTH head word and lemmas appear}
\item{head_word_dispersion}{Dispersion value (Juilland's D) from a minimum of 0.00 to a maximum of 1.00. Where BOTH head word and lemmas appear}
\item{variant_word}{Variant form of headword}
\item{variant_word_freq_ppm}{Rounded frequency per million word tokens (down to a minimum of 10 occurrences of a lemma per million), see pp. 5. Where BOTH head word and lemmas appear}
\item{variant_word_range}{Range, the number of sectors of the corpus (out of a maximum of 100) in which the word occurs. Where BOTH head word and lemmas appear}
\item{variant_word_dispersion}{Dispersion value (Juilland's D) from a minimum of 0.00 to a maximum of 1.00. Where BOTH head word and lemmas appear}

}
\references{
http://www.infochimps.com/datasets/word-frequencies-from-the-british-national-corpus
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
word.freq("statistics")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
