#' Mngr
#' @docType package
#' @name mngr
#' @import dplyr tidyr
NULL

# This environment contains the main variables
# 1. tasklist - this is a list of tasks
# 2. config - this is a list of configuration options
# 3. arms_list
# 4. post_run_list
task_env <- new.env()

# This environment contains the taskarm objects
taskarm_env <- new.env()

# jobids
slurm_env <- new.env()

job_env <- new.env()

# This environment is used by the local queue scheduler
lscheduler_env <- new.env()

# This is the default maximum number of tasks that can run at once
mngr_default_throttle <- 100

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
  assign("config", elements, envir = task_env)
}

#' Find Mngrfile
#'
#' Find the Mngrfile in the supplied directory or the parent directory
#'
#' @param dir a path to a directory
find_mngrfile <- function(dir){
  path <- file.path(dir, "Mngrfile.R")
  if (file.exists(path)){
    path
  } else {
    path <- file.path(dir, "../", "Mngrfile.R")
    if (file.exists(path)){
      path
    } else {
      stop("Can't find Mngrfile")
    }
  }
}

#' Execute a task
#'
#' Actually run a task, and any prerequisites that have changed since the last
#' run.
#'
#' The process is as follows:
#' 1. Ensures the current directory is a git work tree
#' 2. Ensures the current directory is a clean git work directory
#' 3. Loads the Mngrfile
#' 4. Invokes the supplied task
#'
#' @param name a task name
#' @param debug A logical, indicating whether to show debug messages
#' @export
run <- function(name = "default", debug = FALSE){
  is_inside_git_work_tree <- is_inside_git_work_tree()
  if (!is_inside_git_work_tree){
    stop("The directory is not in a git repository")
  }

  suppressWarnings({
    system("git update-index --refresh", intern = TRUE)
    clean <- system("git diff-index --quiet HEAD --", intern = TRUE)
  })
  if (!is.null(attributes(clean)$status) && attributes(clean)$status == 1){
    warning("Not cloning a clean respository:", clean)
  }
  git_clone_or_pull()

  assign("jobids", c(), envir = slurm_env)
  with_dir(run_dir(check = TRUE), {
    mngrfile <- find_mngrfile(getwd())
    source(mngrfile)

    name_expr <- substitute(name)
    if (is.name(name_expr)){
      name <- as.character(name_expr)
    }
    task_obj <- task_get(name)
    lapply(task_env$post_run_list, eval)
    task_obj$invoke(debug = debug)
    run_jobs(debug = debug)

    # this is the wrong place for this
    r_log_fun <- task_env$config$r_logs
    r_log_path <- r_log_fun(normalizePath(".", winslash = "/"))

    invisible(TRUE)
  })
  jobids <- slurm_env$jobids
  jobids_length <- length(jobids)
  if (jobids_length > 0){
    message(jobids_length, " jobs submitted")
    paste(paste(jobids, collapse = ","), r_log_path)
  } else {
    message("No jobs submitted")
  }

  scheduler <- task_env$config$scheduler %||% "slurm"
  if (scheduler == "local"){
    while (!lscheduler_finished()){
      lscheduler_run_next()
      Sys.sleep(2)
    }
  }
}
