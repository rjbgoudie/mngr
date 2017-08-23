#' Mngr
#' @docType package
#' @name mngr
#' @import tools git2r dplyr tidyr
NULL

task_env <- new.env()
taskarm_env <- new.env()
slurm_env <- new.env()
job_env <- new.env()
mngr_default_throttle <- 100

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
