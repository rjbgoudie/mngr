% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{file_last_line}
\alias{file_last_line}
\title{Get the final line of a file}
\usage{
file_last_line(path)
}
\arguments{
\item{path}{Path to the file}
}
\value{
A character vector of length 1 containing the last line of file
}
\description{
Internally uses system calls to tail and sed
}
