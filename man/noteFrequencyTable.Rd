% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{noteFrequencyTable}
\alias{noteFrequencyTable}
\title{Note-frequency table}
\usage{
noteFrequencyTable(minOctave = 0, maxOctave = 8)
}
\arguments{
\item{minOctave}{integer, smallest (lowest-pitched) octave}

\item{maxOctave}{integer, largest (highest-pitched) octave}
}
\value{
a data frame with 4 columns: note name 1 (written with 'b'),
 note name 2 (written with '#'),index (in semitones with respect to A4)
 and frequency (in Hz)
}
\description{
Builds a dataframe containing notes (in
\href{https://en.wikipedia.org/wiki/Scientific_pitch_notation}{scientific pitch notation})
and corresponding frequencies.
}
\examples{
# example code
noteFrequencyTable()
}
