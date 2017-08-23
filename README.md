mngr
====

mngr is a system for automating runs of R files efficiently for statistical
analyses.
In principle, it is compatible with any batch system, but currently only using
the Slurm system is implemented.

It is somewhat inspired by
[Make](https://www.gnu.org/software/make/manual/make.html) and
[Rake](https://github.com/ruby/rake), although it is more specialised and
opinionated.
In particular, versions of analyses are handled using `git': different branches
can be run simultaneously and independently, and the output will be stored
separately.

Workflows are defined in a file called `Mngrfile.R` - this file is written in a
special form of R.

After running `rfiles()` all files ending in `.R` in the current directory are
`tasks' that can be run in the command line by running. For example, if you have
a file called `filename.R', then this can be run via

``` sh
mngr filename
```

Often, we want ot run many variants of a particular analysis.
Each variant of an analysis is called an `arm' in mngr.
If all combinations are required, then add the following to the Mngrfile.R

``` r
arms_factorial(initial_value = c(1, 4, 7),
               seed = c(3, 4))
```

So that each arm does not overwrite each other, helper functions are available
to place data files and plots etc in appropriate places: `rds_file` etc

Whenever a task is run, the git HEAD is copied (by cloning or pulling the
git repository) from $HOME/path/to/analysis to $HOME/run/path/to/analysis -
this allows further edits to be made whilst a task is running


Installation
------------

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("rjbgoudie/mngr")
```
