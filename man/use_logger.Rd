% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_logger.R
\name{use_logger}
\alias{use_logger}
\title{Setup a Simple Logger for a Package}
\usage{
use_logger(
  pkg = desc::desc_get("Package", rprojroot::find_package_root_file("DESCRIPTION"))[[1]]
)
}
\arguments{
\item{pkg}{\code{character} scalar. Name of the package. The default is to try to
get the Package name automatically using the packages \strong{rprojroot} and
\strong{desc}}
}
\value{
a \code{character} scalar containing \R code.
}
\description{
This gives you a minimal logger with no appenders that you can use inside
your package under the name \code{lg} (e.g. lg$fatal("test")). \code{use_logger()}
does not modify any files but only prints code for you to copy and paste.
}
\examples{
use_logger("testpkg")

}
