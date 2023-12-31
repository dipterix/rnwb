% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{to_r}
\alias{to_r}
\title{Try to convert an object to an R object}
\usage{
to_r(x, on_fail = c("ignore", "error", "warning"))
}
\arguments{
\item{x}{input data, can be either an 'R' or 'Python' object}

\item{on_fail}{what to do when the object is still a 'Python' object;
default is to \code{'ignore'}, other choices are \code{'error'}
and \code{'warning'}}
}
\value{
The converted R object
}
\description{
Try to convert an object to an R object
}
