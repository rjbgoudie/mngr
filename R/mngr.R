#' Mngr
#' @docType package
#' @name mngr
#' @import tools git2r
NULL

task_env <- new.env()
taskarm_env <- new.env()
slurm_env <- new.env()
job_env <- new.env()

#' Configure mngr
#'
#' @param ... a list of values to replace default config
#' @export
mngr_config <- function(...){
  # this implementation is rubbish
  elements <- list(...)
  assign("config", elements, envir = task_env)
}

#' Find mngrfile
#' @param dir a path
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

#' Execute the task
#' @param name a task name
#' @export
run <- function(name = "default", debug = FALSE){
  is_inside_git_work_tree <- is_inside_git_work_tree()
  if (!is_inside_git_work_tree){
    stop("The directory is not in a git repository")
  }

  suppressWarnings({
    clean <- system("require-clean-work-dir", intern = TRUE)
  })
  if (!is.null(attributes(clean)$status) && attributes(clean)$status == 1){
    stop(clean)
  }
  git_clone_or_pull()

  assign("jobids", c(), envir = slurm_env)
  with_dir(run_dir(check = TRUE), {
    mngrfile <- find_mngrfile(getwd())
    source(mngrfile)

    expr <- substitute(name)
    if (is.name(expr)){
      name <- as.character(expr)
    }
    task_obj <- task_get(name)
    lapply(task_env$post_run_list, eval)
    task_obj$invoke(debug = debug)
    run_jobs(debug = debug)

    # this is the wrong place for this
    r_log_fun <- task_env$config$r_logs
    r_log_path <- r_log_fun(normalizePath("."))

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
}

## #,
## #           rdata = sub_path("^/home", "/scratch", "rdata"),
## #           plots = sub_path("^/home", "/scratch", "plots"))

## rfile("1-a")
## rfile("2-b")
## rfile("3-c")

# Want a single log of the overall progress
# plus files for each r file, with that files' situation
# each job is a different process now, who knows when each starts.

# decouple run directory from git branch nam
# r function that queries sbatch for job status every few seconds

# data to pass into the R process
# mngr_arm

# "fP",  which is rather underused by me at the moment
# git branch which is very heavily used at the moment

# input vs output location

## branch/a_1/a_v/adf_2/
##   folder variant vs naming variant

## mngr_full_factorial(a = c(1, 2, 3), b = c("e", "d"))

## df <- data.frame(a = c(1, 2), b = c("e", "d"))
## mngr_fractional_factorial(df)

## to be able to run from the command line, need rdataFile to know about git
## because otherwise the relative path is not correct.
## ie need to make ~/run not organised by branch?

## can I delete run at the end of the run
## ie run/analyses/mwp/sha/copyofwholegitrepo

## run
## pathtogitbasedir
## br-sha
## gitdir

## should avoid the deleting issue

## but don\'t know the jid until too late? or perhaps the copying should be done
## by the compute node?

## flow number as well as a job number?

## config run_dir

## logdir - basically an easy function of the outdir?
## outdir
## rundir
## indir

## mngr_equiv_path("scratch")

## mngr_config(logs = "logs",
##            rdata = "rdata",
##            plots = "plots")
