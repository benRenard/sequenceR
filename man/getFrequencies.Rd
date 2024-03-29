% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{getFrequencies}
\alias{getFrequencies}
\title{Notes-to-frequencies function}
\usage{
getFrequencies(notes, minOctave = 0, maxOctave = 8)
}
\arguments{
\item{notes}{Character vector, note names.}

\item{minOctave}{integer, smallest (lowest-pitched) octave}

\item{maxOctave}{integer, largest (highest-pitched) octave}
}
\value{
a numeric vector of frequencies (in Hz)
}
\description{
Get frequencies from note names (in
\href{https://en.wikipedia.org/wiki/Scientific_pitch_notation}{scientific pitch notation}).
}
\examples{
# example code
getFrequencies(c('A3','A4','A5','C#6','Db6','A9','X0'))
getFrequencies(c('A3','A4','A5','C#6','Db6','A9','X0'),maxOctave=9)
}
