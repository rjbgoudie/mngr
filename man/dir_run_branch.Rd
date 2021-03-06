% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dir.R
\name{dir_run_branch}
\alias{dir_run_branch}
\title{Corresponding (branch-specific) run directory}
\usage{
dir_run_branch(dir = getwd(), check = TRUE)
}
\arguments{
\item{dir}{A directory}

\item{check}{Logical, if FALSE no test for whether dir is inside a git work
tree}
}
\value{
Return the path to the corresponding run directory for the supplied
directory. If the supplied dir is a run directory, then the supplied
directory is returned.
}
\description{
If passed ~/analyses/project/folder/, where ~/analyses/project is the git
toplevel folder, will return
}
\details{
mngr_option_dir_run("$GIT_TOPLEVEL")/$GIT_BRANCH/folder/

If mngr_option_dir_run($GIT_TOPLEVEL)=$GIT_TOPLEVEL, then just returns what
was supplied
}
