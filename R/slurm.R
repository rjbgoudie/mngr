slurm_r_job <- function(task){
  id <- task_find_id(task$name, exists = TRUE)
  path <- normalizePath(paste0(task$name, ".R"))

  slurm_log_path <- task$slurm_file(ensure_dir = TRUE)

  queue <- task_env$config$queue %||% "sand"

  array <- task_env$config$array
  jobids <- c()
  for (index in seq_len(array)){
    name <- paste(task$name, index, sep = "_")

    dependency <- task_env$tasklist[[id]]$jobid_prereqs(index)
    dependency <- if (!is.null(dependency) && length(dependency) > 0){
      paste0("--dependency=afterok:", paste(dependency, collapse = ","), " ")
    } else {
      ""
    }

    r_log_specific_path <- task$r_log_specific_file(ensure_dir = TRUE, index)
    r_log_latest_path <- task$r_log_latest_file(ensure_dir = TRUE, index)
    r_log_path <<- r_log_specific_path

    array <- 1
    incant <-
      paste0("MNGR_RFILE=", path,
             " MNGR_RLOGFILE=", r_log_specific_path,
             " MNGR_RLOGLATESTFILE=", r_log_latest_path,
             " MNGR_ARM=", index,
             " sbatch -J ", name,
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
    jobids <- c(jobids, jobid)
  }
  task$set_jobid(jobids)
  jobids
}
