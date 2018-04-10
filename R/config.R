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

mngr_option_run_path <- function(){
  default <- function(dir){
    file.path(Sys.getenv("HOME"), "run")
  }
  getOption("mngr_run_path") %||% default
}

mngr_option_use_tempfile <- function(){
  getOption("mngr_use_tempfile") %||% FALSE
}

mngr_option_cluster_path <- function(){
  getOption("mngr_cluster_path")
}