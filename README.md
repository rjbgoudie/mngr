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


File locations and paths
------------------------

There are several file locations relevant to mngr:

1. Where the actual R files are stored, which must be within a git repository.
   We call the top level directory of this git repository $GIT_TOPLEVEL
2. Where the R files will be _run_ from: mngr checks out the git repository to
   a separate location, when running the analysis, so that the original working
   copy of the R files can be edited while the original analysis is running,
   without changing what runs.

   The location of this is set by the option ```mngr_dir_run```.

   This should be a function that converts $GIT_TOPLEVEL to the equivalant run
   directory. By default $HOME/path/to/$GIT_TOPLEVEL/ will be converted to
   $HOME/run/$GIT_TOPLEVEL.

   Note that the function must return the supplied dir if dir is in the run
   directory already.

3. The "output" location: this is where all of the plots, rds files, and logs
   are stored. You may wish for this to on a separate drive to the R source,
   since the output may be large and can be reproduced, so you might not need to
   back the results up so carefully.

   The location of this is set by the option ```mngr_dir_output```.

   This should be a function that converts a path (within the run directory) to
   the corresponding path in the results location. By default, mngr_dir_output
   is set so that the results will be saved within the run directory

These settings should be made in ```~/.mngr/config.R```. So that you can use
```mngr``` interactively, you will also want to source this config file in
your ```.Rprofile```

Slurm
-----

Submit scripts should go in ```~/.mngr/slurm/QUEUE``` where QUEUE is the name
of the queue.

If no queue is specified in an Mngrfile, ```~/.mngr/slurm/default``` will be used

Installation
------------

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("rjbgoudie/mngr")
```
