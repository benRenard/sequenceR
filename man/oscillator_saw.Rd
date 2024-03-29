% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oscillators.R
\name{oscillator_saw}
\alias{oscillator_saw}
\title{Saw oscillator}
\usage{
oscillator_saw(freq = 440, duration = 1, phase = 0, rate = 44100)
}
\arguments{
\item{freq}{Numeric, note frequency in Hz}

\item{duration}{Numeric, note duration in second}

\item{phase}{Numeric, phase in radians (typically between 0 and 2*pi)}

\item{rate}{Numeric, sampling rate in Hz}
}
\value{
An object of class 'soundSample'.
}
\description{
Creates a soundSample using a saw oscillator.
}
\examples{
sam <- oscillator_saw(freq=220,duration=0.1)
plot(sam)
# listen(sam) # uncomment to listen
}
