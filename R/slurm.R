slurm_sand_r_job <- substitute({
  id <- task_find_id(name, exists = TRUE)
  dependency <- task_env$tasklist[[id]]$jobid_prereqs()
  dependency <- if (!is.null(dependency) && length(dependency) > 0){
    paste0("--dependency=afterok:", paste(dependency, collapse = ","), " ")
  } else {
    ""
  }
  path <- normalizePath(paste0(name, ".R"))

  r_log_specific_path <- .self$r_log_specific_file(ensure_dir = TRUE)
  r_log_latest_path <- .self$r_log_latest_file(ensure_dir = TRUE)
  r_log_path <<- r_log_specific_path

  slurm_log_path <- .self$slurm_file(ensure_dir = TRUE)

  array <- task_env$config$array

  incant <-
    paste0("MNGR_RFILE=", path,
           " MNGR_RLOGFILE=", r_log_specific_path,
           " MNGR_RLOGLATESTFILE=", r_log_latest_path,
           " sbatch -J ", name,
           " --array=1-", array,
           " --parsable ",
           dependency,
           " --output=", slurm_log_path,
           " ", getOption("mngr_cluster_path"), "/mngr_slurm_submit.sand",
           "\n")
  jobid <- system(incant, intern = TRUE)
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", name, " (", jobid, ")")
  ## cat(incant)
  jobid <<- jobid
})

slurm_tesla_r_job <- substitute({
  id <- task_find_id(name, exists = TRUE)
  dependency <- task_env$tasklist[[id]]$jobid_prereqs()
  dependency <- if (!is.null(dependency) && length(dependency) > 0){
    paste0("--dependency=afterok:", paste(dependency, collapse = ","), " ")
  } else {
    ""
  }
  path <- normalizePath(paste0(name, ".R"))

  r_log_specific_path <- .self$r_log_specific_file(ensure_dir = TRUE)
  r_log_latest_path <- .self$r_log_latest_file(ensure_dir = TRUE)
  r_log_path <<- r_log_specific_path

  slurm_log_path <- .self$slurm_file(ensure_dir = TRUE)

  array <- task_env$config$array

  incant <-
    paste0("MNGR_RFILE=", path,
           " MNGR_RLOGFILE=", r_log_specific_path,
           " MNGR_RLOGLATESTFILE=", r_log_latest_path,
           " sbatch -J ", name,
           " --array=1-", array,
           " --parsable ",
           dependency,
           " --output=", slurm_log_path,
           " ", getOption("mngr_cluster_path"), "/mngr_slurm_submit.tesla",
           "\n")
  jobid <- system(incant, intern = TRUE)
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", name, " (", jobid, ")")
  ## cat(incant)
  jobid <<- jobid
})
