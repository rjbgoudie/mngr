% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{sub_path}
\alias{sub_path}
\title{Manipuate a path}
\usage{
sub_path(pattern, replacement, dir)
}
\arguments{
\item{pattern}{character string containing a regular expression}

\item{replacement}{a replacement for matched pattern}

\item{dir}{a directory to tack on the end of the path}
}
\value{
the path after replacment and concatenation
}
\description{
uses sub for grep replacement
}
