#' Add all R files in the current directory as tasks
#' @export
rfiles <- function(){
  rfiles <- list.files(path = ".",
                       pattern = "\\.R$")
  rfiles_sans_ext <- sapply(rfiles, file_path_sans_ext)
  sapply(rfiles_sans_ext, rfile)
  nfiles <- length(rfiles_sans_ext)
  message("Added ", nfiles, " R files")
}

#' Create rfile task
#'
#' @param name task name
#' @param action a set of expressions
rfile_create <- function(name, action){
  task <- RTask(name = name)
  task_env$tasklist <- c(list(task), task_env$tasklist)
  names(task_env$tasklist)[1] <- name
}

#' Add rfile as a slurm task
#' @param ... passed to task()
#' @export
rfile <- function(name){
  expr <- substitute(name)
  action <- slurm_r_job

  if (is.name(expr)){
    name <- as.character(expr)
  }
  if (task_exists(name)){
    message("already exists")
  } else {
    rfile_create(name, {})
  }
  id <- task_find_id(name, exists = TRUE)
  task_env$tasklist[[id]]$enhance(action_new = action)
  invisible(TRUE)
}
