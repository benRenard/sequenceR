% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_soundSample.R
\name{sequence}
\alias{sequence}
\title{Sequence a sound sample}
\usage{
sequence(
  sample,
  time,
  letRing = TRUE,
  volume = rep(1, NROW(time)),
  pan = rep(0, NROW(time)),
  nmax = 10 * 10^6
)
}
\arguments{
\item{sample}{Sound sample object.}

\item{time}{Numeric vector, time (in seconds) at which sample should be repeated}

\item{letRing}{Logical. If TRUE overlapping samples are added;
if FALSE, a new sample stops the previous one (=> beware of the click!))}

\item{volume}{Numeric vector, volume between 0 and 1.}

\item{pan}{Numeric vector, pan between -1 (left) and 1 (right) (0 = centered).}

\item{nmax}{Integer, max number of values for each channel of the resulting Wave.
Default value (10*10^6) roughly corresponds to a 150 Mb stereo wave, ~3 min 45s.}
}
\value{
an S4 Wave object (from package tuneR).
}
\description{
Take a sound sample and repeat it following given timeline,
volume and pan.
}
\examples{
# EXAMPLE 1
# Define a sound sample
sam <- soundSample(sin(2*pi*seq(0,1,,44100)*440)+0.1*rnorm(44100)) # 1-second noisy A
# Sequence it
s <- sequence(sam,time=c(0,0.5,0.75),letRing=FALSE,volume=c(0.4,1,1),pan=c(-1,0,1))
# View the result
plot(s)
# Uncomment to play it
# play(s)

#' EXAMPLE 2 - make it funkyer
# 2-second sequence based on hi-hat sample
s <- sequence(hiHat,time=seq(0,2,,16),volume=rep(c(1,rep(0.5,3)),4))
# View the result
plot(s)
# Uncomment to play it
# play(s)
}
