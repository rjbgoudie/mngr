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

mngr_option_run_path <- function(){
  run_path_fun <- getOption("mngr_run_path")
}

mngr_option_use_tempfile <- function(){
  getOption("mngr_use_tempfile")
}

mngr_option_cluster_path <- function(){
  getOption("mngr_cluster_path")
}
