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

  r_file_date <- function(name){
    command <- paste0("git log -1 --format=%cD ", name, ".R")
    r_file_date <- system(command, intern = TRUE)
    as.POSIXct(strptime(r_file_date, format = "%a,  %d %b %Y %T %z"))
  }
  task$set_custom_last_edited_time(r_file_date)
  task_env$tasklist <- c(list(task), task_env$tasklist)
  names(task_env$tasklist)[1] <- name
}

#' Add rfile as a slurm task
#' @param ... passed to task()
#' @export
rfile <- function(name){
  expr <- substitute(name)

  scheduler <- task_env$config$scheduler %||% "slurm"
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
