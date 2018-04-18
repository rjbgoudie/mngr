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

mngr_option_use_tempfile <- function(){
  getOption("mngr_use_tempfile") %||% FALSE
}

mngr_option_cluster_path <- function(){
  getOption("mngr_cluster_path")
}

mngr_option_dir_results <- function(){
  default <- function(dir){
    dir
  }
  getOption("mngr_dir_results") %||% default
}

mngr_option_dir_slurm_logs <- function(){
  default <- function(dir){
    fs::path(mngr_option_dir_results()(dir), "logs", "slurm")
  }
  getOption("mngr_dir_slurm_logs") %||% default
}

mngr_option_dir_r_logs_latest <- function(){
  default <- function(dir){
    fs::path(mngr_option_dir_results()(dir), "logs", "r-latest")
  }
  getOption("mngr_dir_r_logs_latest") %||% default
}

mngr_option_dir_r_logs <- function(){
  default <- function(dir){
    fs::path(mngr_option_dir_results()(dir), "logs", "r")
  }
  getOption("mngr_dir_r_logs") %||% default
}

mngr_option_dir_state <- function(){
  default <- function(dir){
    fs::path(mngr_option_dir_results()(dir), "state")
  }
  getOption("mngr_dir_state") %||% default
}
