#' Add all R files in the current directory as tasks
#'
#' All files ending .R are added as tasks. Each task takes the name of the file,
#' minus the file extension
#' @export
rfiles <- function(){
  pause_loading <- task_env$config$pause_loading %||% FALSE
  if (!pause_loading){
    rfiles <- list.files(path = ".",
                         pattern = "\\.R$")
    rfiles_sans_ext <- sapply(rfiles, tools::file_path_sans_ext)
    sapply(rfiles_sans_ext, rfile)
    nfiles <- length(rfiles_sans_ext)
    message("Added ", nfiles, " R files")
  }
}

#' Create rfile task
#'
#' @param name task name
#' @param action a set of expressions (currently ignored?)
rfile_create <- function(name, action){
  task <- Task(name = name)

  r_filename_fun <- function(taskname){
    paste0(taskname, ".R")
  }

  task$set_filename_function(r_filename_fun)
  task_env$tasklist <- c(list(task), task_env$tasklist)
  names(task_env$tasklist)[1] <- name
}

#' Add rfile as a slurm task
#' @param ... passed to task()
#' @export
rfile <- function(name){
  expr <- substitute(name)

  scheduler <- mngr_option_scheduler()
  if (scheduler == "slurm"){
    action <- slurm_r_job
  } else if (scheduler == "local"){
    action <- lscheduler_job
  }

  if (is.name(expr)){
    name <- as.character(expr)
  }
  if (task_exists(name)){
    message("already exists")
  } else {
    rfile_create(name, {})
  }
  task_obj <- task_get(name, exists = TRUE)
  task_obj$enhance(actions_new = action)
  invisible(TRUE)
}
