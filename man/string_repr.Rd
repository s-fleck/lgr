% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_repr.R
\name{string_repr}
\alias{string_repr}
\alias{string_repr.function}
\alias{string_repr.data.frame}
\alias{string_repr.matrix}
\alias{string_repr.numeric}
\alias{string_repr.default}
\title{Short string representation for R objects}
\usage{
string_repr(x, width = 32, ...)

\method{string_repr}{`function`}(x, width = 32L, ...)

\method{string_repr}{data.frame}(x, width = 32L, ...)

\method{string_repr}{matrix}(x, width = 32L, ...)

\method{string_repr}{numeric}(x, width = 32L, ...)

\method{string_repr}{default}(x, width = 32L, ...)
}
\arguments{
\item{x}{Any \R object.}

\item{width}{a scalar integer}

\item{...}{passed on to methods}
}
\value{
a \code{scalar} character
}
\description{
This is inspired by the python function \code{repr} and produces a short
string representation of any \R object that is suitable for logging and error
messages. It is a generic so you can implement methods for custom S3 objects.
}
\examples{
string_repr(iris)
string_repr(LETTERS)
string_repr(LETTERS, 10)
}
