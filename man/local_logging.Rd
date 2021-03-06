% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logging.R
\name{local_logging}
\alias{local_logging}
\title{Set up logging for use on a local machine.}
\usage{
local_logging(level_name = "info")
}
\arguments{
\item{level_name}{One of the strings (trace, debug, info, warn, error, fatal)
    Default value is "info".

This creates a logger that is named after the package
so that all logging messages for this package can be turned on or off
together. This function makes everything go to the console on
standard out.}
}
\description{
Set up logging for use on a local machine.
}
