slurm_r_job <- function(task){
  id <- task_find_id(task$name, exists = TRUE)
  dependency <- task_env$tasklist[[id]]$jobid_prereqs()
  dependency <- if (!is.null(dependency) && length(dependency) > 0){
    paste0("--dependency=afterok:", paste(dependency, collapse = ","), " ")
  } else {
    ""
  }
  path <- normalizePath(paste0(task$name, ".R"))

  r_log_specific_path <- task$r_log_specific_file(ensure_dir = TRUE)
  r_log_latest_path <- task$r_log_latest_file(ensure_dir = TRUE)
  r_log_path <<- r_log_specific_path

  slurm_log_path <- task$slurm_file(ensure_dir = TRUE)

  array <- task_env$config$array
  queue <- task_env$config$queue %||% "sand"

  incant <-
    paste0("MNGR_RFILE=", path,
           " MNGR_RLOGFILE=", r_log_specific_path,
           " MNGR_RLOGLATESTFILE=", r_log_latest_path,
           " sbatch -J ", task$name,
           " --array=1-", array,
           " --parsable ",
           dependency,
           " --time=24:00:00",
           " --output=", slurm_log_path,
           " ", getOption("mngr_cluster_path"), "/mngr_slurm_submit.", queue,
           "\n")
  jobid <- system(incant, intern = TRUE)
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", task$name, " (", jobid, ")")
  ## cat(incant)
  task$set_jobid(jobid)
  jobid
}
