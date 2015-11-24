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
rfile <- function(name, action = {
  id <- task_find_id(name, exists = TRUE)
  jp <- task_env$tasklist[[id]]$jobid_prereqs()
  jp <- if (!is.null(jp) && length(jp) > 0){
    paste0("--dependency=afterok:", paste(jp, collapse = ","), " ")
  } else {
    ""
  }
  path <- normalizePath(paste0(name, ".R"))

  r_log_fun <- task_env$config$r_logs
  r_log_dir <- r_log_fun(normalizePath("."))
  r_log_latest_dir <- paste0(r_log_dir, "-latest/")
  ensure_exists(r_log_dir)
  ensure_exists(r_log_latest_dir)

  r_log_latest_file <- paste0("\\${SLURM_ARRAY_TASK_ID}-", name, ".Rout")
  r_log_specific_file <- paste0("\\${SLURM_ARRAY_JOB_ID}.", r_log_latest_file)

  r_log_specific_path <- file.path(r_log_dir, r_log_specific_file)
  r_log_latest_path <- file.path(r_log_latest_dir, r_log_latest_file)

  r_log_path <<- r_log_specific_path

  slurm_log_file <- paste0("%A.%a-", name, ".txt")
  slurm_log_fun <- task_env$config$slurm_logs
  slurm_log_path <- slurm_log_fun(normalizePath("."))
  ensure_exists(slurm_log_path)
  slurm_log_path <- file.path(slurm_log_path, slurm_log_file)

  array <- task_env$config$array

  incant <-
    paste0("MNGR_RFILE=", path,
           " MNGR_RLOGFILE=", r_log_specific_path,
           " MNGR_RLOGLATESTFILE=", r_log_latest_path,
           " sbatch -J ", name,
           " --array=1-", array,
           " --parsable ", jp,
           " --output=", slurm_log_path,
           " ", getOption("mngr_cluster_path"), "/mngr_slurm_submit.darwin",
           "\n")
  jobid <- system(incant, intern = TRUE)
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", name, " (", jobid, ")")
  ## cat(incant)
  jobid <<- jobid
}){
  expr <- substitute(name)
  action <- substitute(action)
  if (is.name(expr)){
    name <- as.character(expr)
  }
  if (task_exists(name)){
    message("already exists")
  } else {
    rfile_create(name, {})
  }
  id <- task_find_id(name, exists = TRUE)
  task_env$tasklist[[id]]$add_action(action)
  invisible(TRUE)
}
