% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{rescale}
\alias{rescale}
\title{Rescale function}
\usage{
rescale(x, low = 0, high = 1)
}
\arguments{
\item{x}{Numeric vector}

\item{low}{Numeric, lower bound}

\item{high}{Numeric, higher bound}
}
\value{
a rescaled numeric vector
}
\description{
Rescale a series between two bounds
}
\examples{
# example code
rescale(1:10)
rescale(rnorm(10), 100, 101)
}
