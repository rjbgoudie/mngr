slurm_r_job <- function(task){
  id <- task_find_id(task$name, exists = TRUE)
  task_obj <- task_env$tasklist[[id]]
  path <- normalizePath(paste0(task$name, ".R"))

  slurm_log_path <- task$slurm_file(ensure_dir = TRUE)

  queue <- task_env$config$queue %||% "sand"

  arms <- arms_all(task$name, include_shared = FALSE)
  arms_count <- length(arms)

  jobids <- c()
  for (arm_index in seq_len(arms_count)){
    name_with_array <- paste(task$name, arm_index, sep = "_")

    # if any shared arms, then depend on all prereqs, not just the matching
    # index
    if (task_obj$any_shared()){
      dependency <- task_obj$jobid_prereqs(TRUE)
    } else {
      dependency <- task_obj$jobid_prereqs(arm_index)
    }

    memory <- task_obj$get_memory()
    cores <- task_obj$get_cores()

    dependency <- if (!is.null(dependency) && length(dependency) > 0){
      paste0("--dependency=afterok:", paste(dependency, collapse = ","), " ")
    } else {
      ""
    }

    r_log_specific_path <- task$r_log_specific_file(ensure_dir = TRUE, arm_index)
    r_log_latest_path <- task$r_log_latest_file(ensure_dir = TRUE, arm_index)
    r_log_path <<- r_log_specific_path

    incant <-
      paste0("MNGR_RFILE=", path,
             " MNGR_RLOGFILE=", r_log_specific_path,
             " MNGR_RLOGLATESTFILE=", r_log_latest_path,
             " MNGR_TASKNAME=", task$name,
             " MNGR_ARM=", arm_index,
             " sbatch -J ", name_with_array,
             " --parsable ",
             dependency,
             " --mem=", memory,
             " --ntasks=", cores,
             " --nodes=1",
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
