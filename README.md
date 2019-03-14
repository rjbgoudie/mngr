mngr
====

mngr is a system for automating runs of R files for statistical
analyses.

Workflows are defined in a file called `Mngrfile.R` in the directory of the
analysis.

All R files in the current directory can be made into a 'task' using
```rfiles()```

Dependencies between tasks are set up using the ```%d%``` operator (or
```%depends%```).

Arms
----

Often, we want to run many variants of a particular analysis.
Each variant of an analysis is called an `arm' in mngr.
If all combinations are required, then add the following to the Mngrfile.R

``` r
arms_factorial(initial_value = c(1, 4, 7),
               seed = c(3, 4))
```

This will mean that, for example, in the first arm,
```initial_value = 1``` and ```seed = 3```.
The variables will be set when R starts up.

So that each arm does not overwrite each other, helper functions are available
to place data files and plots etc in appropriate places:

1. ```rds_file("myobject")``` will return the path to 
   ```$OUTPUT/$GITBRANCH/$ARM/rds/myobject.rds```
2. ```pdf_file("myplot")``` will return the path to 
   ```$OUTPUT/$GITBRANCH/$ARM/plots/myplot.pdf```
3. ```tiff_file("myplot")``` will return the path to 
   ```$OUTPUT/$GITBRANCH/$ARM/rds/myplot.tiff```
4. ```png_file("myplot")``` will return the path to 
   ```$OUTPUT/$GITBRANCH/$ARM/rds/myplot.png```

All of these are wrappers to ```file_path```.

Slurm
-----

Submit scripts should go in ```~/.mngr/slurm/QUEUE``` where QUEUE is the name
of the queue (unless you change the value of ```mngr_slurm_submit_path``` -- see
below).

If no queue is specified in an Mngrfile, ```~/.mngr/slurm/default``` will be used

Cores, memory, hours, and throttling constraints can be set for individual
tasks using ```%cores%```, ```%memory%```, ```%hours%``` and ```%throttle%```.

Options
-------

Several settings are controlled by the standard ```options``` mechanism in R.

These settings should be made in ```~/.mngr/config.R```. So that you can use
```mngr``` interactively, you will also want to source this config file in
your ```.Rprofile```

1. Scheduler: ```mngr_scheduler``` should take the value ```"slurm"``` to use
   the Slurm Workload Manger for running the jobs on a cluster, or
   ```"local"``` to run the jobs on locally, using a simple R-based scheduler.

   When the default is to use Slurm, the ```--local``` flag to ```mngr``` will
   override this.

2. Filepaths: ```mngr_dir_output``` and ```mngr_dir_run``` - see 'File locations
   and paths' section below for details.

   Further customisation can be achieved with the ```mngr_dir_slurm_logs```,
   ```mngr_dir_r_logs_latest```, ```mngr_dir_r_logs``` and ```mngr_dir_state```
   optins.

3. Use tempfile for saving RDS files. On some filesystems (e.g NFS network
   shares), it can be slow to write out RDS files using ```saveRDS```. Instead,
   it is much quicker to write the file first to a local filesystem, and then
   move the final file to the network share.

   Setting ```mngr_use_tempfile``` to ```TRUE``` will turn this option on: when
   ```saveRDS``` is used, the R object is first written to ```tempfile()```,
   and then moved to the ultimate final location.

4. Slurm submit directory: ```mngr_slurm_submit_path```. This should be set to
   the path containing Slurm submit scripts. The directory can contain several
   submit scripts, for example for different Slurm queues.

   The default value is ```~/.mngr/slurm/```.

5. Global throttle: ```mngr_global_throttle```. The maximum number of tasks to
   run simulataneously (for local scheduler; default set by
   ```parallel::detectCores()```) or the maximum number of arms of
   a task to run (for slurm; default is 50).

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

Installation
------------

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("rjbgoudie/mngr")
```

Other systems
-------------

```mngr``` is somewhat inspired by
[Make](https://www.gnu.org/software/make/manual/make.html) and
[Rake](https://github.com/ruby/rake), but ```mngr``` is customised to
statistical workflows.
In particular, versions of analyses are handled using `git': different branches
can be run simultaneously and independently, and the output will be stored
separately.
