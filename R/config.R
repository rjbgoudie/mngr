#' Set configuration options for mngr
#'
#' The current options are:
#' - queue: the Slurm queue to submit to
#' - output: where the main output should go
#' - r_logs: where the R logs should be stored
#' - slurm_logs: where the Slurm logs should be stored
#' - throttle: the maximum number of tasks that can run at once
#'
#' @param ... a list of values to replace default config
#' @export
mngr_config <- function(...){
  # this implementation is rubbish
  elements <- list(...)
  new <- modifyList(as.list(task_env$config), elements)
  assign("config", new, envir = task_env)
}

#' Converting paths to runpaths
#'
#' This option determines the function used to set the path where an analysis
#' will be run. mngr checks out the git repository to a separate location, when
#' running the analysis, so that the original working copy of the R files can
#' be edited while the original analysis is running, without changing what runs.
#'
#' This option should be set to a function that converts \code{$GIT_TOPLEVEL}
#' to the equivalant run directory.
#'
#' By default \code{$HOME/path/to/$GIT_TOPLEVEL/} will be converted to
#' \code{$HOME/run/$GIT_TOPLEVEL}.
#'
#' @return The function used for converting filepath to the runpath
mngr_option_dir_run <- function(){
  default <- function(dir){
    home <- Sys.getenv("HOME")
    homerun <- fs::path(home, "run")
    git_toplevel <- dir_git_toplevel(dir = dir, check = FALSE)
    git_toplevel_name <- basename(git_toplevel)
    # if already in a run, just return supplied dir
    if (grepl(homerun, dir)){
      git_toplevel
    } else {
      fs::path(home, "run", git_toplevel_name)
    }
  }
  getOption("mngr_dir_run") %||% default
}

#' Converting run paths to output paths
#'
#' Set the "output" location: this is where all of the plots, rds files, and
#' logs are stored. You may wish for this to on a separate drive to the R
#' source, since the output may be large and can be reproduced, so you might
#' not need to back the results up so carefully. e.g. a 'scratch' disk may
#' suffice.
#'
#' This option should be a set to function that converts a path (within the
#' run directory) to the corresponding path in the results location.
#'
#' By default, ```mngr_dir_output`` is set so that the results will be saved
#' within the run directory.
#'
#' @return The current value of the option
mngr_option_dir_output <- function(){
  default <- function(dir){
    dir
  }
  getOption("mngr_dir_output") %||% default
}

#' Path to keep Slurm logs
#'
#' This should be a function that converts a runpath to a directory wherer you
#' wish to store Slurm logs.
#'
#' By default, Slurm logs will be stored within the ```mngr_dir_output``` in a
#' folder called ```slurm``` within a folder called ```logs```.
#'
#' @return The current value of the option
mngr_option_dir_slurm_logs <- function(){
  default <- function(dir){
    fs::path(mngr_option_dir_output()(dir), "logs", "slurm")
  }
  getOption("mngr_dir_slurm_logs") %||% default
}

#' Path to keep the latest R logs
#'
#' This should be a function that converts a runpath to a directory wherer you
#' wish to store the latest R logs.
#'
#' By default, R logs will be stored within the ```mngr_dir_output``` in a
#' folder called ```r-latest``` within a folder called ```logs```.
#'
#' @return The current value of the option
mngr_option_dir_r_logs_latest <- function(){
  default <- function(dir){
    fs::path(mngr_option_dir_output()(dir), "logs", "r-latest")
  }
  getOption("mngr_dir_r_logs_latest") %||% default
}

#' Path to keep all R logs
#'
#' This should be a function that converts a runpath to a directory wherer you
#' wish to store all R logs.
#'
#' By default, R logs will be stored within the ```mngr_dir_output``` in a
#' folder called ```r``` within a folder called ```logs```.
#'
#' @return The current value of the option
mngr_option_dir_r_logs <- function(){
  default <- function(dir){
    fs::path(mngr_option_dir_output()(dir), "logs", "r")
  }
  getOption("mngr_dir_r_logs") %||% default
}

#' Path to keep the state files
#'
#' This should be a function that converts a runpath to a directory wherer you
#' wish to store the state files.
#'
#' By default, state files will be stored within the ```mngr_dir_output``` in a
#' folder called ```state```.
#'
#' @return The current value of the option
mngr_option_dir_state <- function(){
  default <- function(dir){
    fs::path(mngr_option_dir_output()(dir), "state")
  }
  getOption("mngr_dir_state") %||% default
}

#' Save RDS data to a tempfile initially
#'
#' This option determines whether files saved using saveRDS are first saved to
#' to a tempfile, generated using \code{\link{tempfile}}, and then copied
#' to the final destination. This makes saving much faster on some very slow
#' filesystems.
#'
#' @return The current value of the option
mngr_option_use_tempfile <- function(){
  getOption("mngr_use_tempfile") %||% FALSE
}

#' Path to the directory containing Slurm submit script
#'
#' This option controls which directory will be searched for Slurm submit
#' scripts. The default is \code{~/.mngr/slurm/}
#'
#' @return The current value of the option
mngr_option_slurm_submit_path <- function(){
  default <- "~/.mngr/slurm/"
  getOption("mngr_slurm_submit_path") %||% default
}
