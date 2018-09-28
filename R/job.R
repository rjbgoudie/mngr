#' Check for job existence
#'
#' @param name name of job
job_exists <- function(name){
  job_exists <- FALSE
  joblist_non_null <- length(job_env$joblist) > 0
  if (joblist_non_null){
    job_exists <- name %in% names(job_env$joblist)
  }
  job_exists
}

#' Find job id
#'
#' @param name job name
#' @param exists does the job exist?
job_find_id <- function(name, exists = job_exists(name)){
  if (exists){
    which(name == names(job_env$joblist))
  } else {
    NULL
  }
}

#' Create job
#'
#' @param name job name
#' @param action a set of expressions
job_create <- function(jobs_needed, actions, properties){
  scheduler <- mngr_option_scheduler()

  if (scheduler == "slurm"){
    scheduler_fun <- SlurmJob
  } else if (scheduler == "local"){
    scheduler_fun <- LSchedulerJob
  }

  group_size <- 1
  ngroups <- ceiling(nrow(jobs_needed)/group_size)
  jobs_needed$group <- rep(seq_len(ngroups), each = group_size, len = nrow(jobs_needed))

  for (group in seq_len(ngroups)){
    rows <- which(jobs_needed$group == group)

    # all task_name should be the same, so just use the first
    basename <- jobs_needed[rows[1], "task_name"]
    job <- scheduler_fun(basename = basename,
                         arm_index = jobs_needed[rows, "arm_index"],
                         actions = actions,
                         properties = properties)
    job$set_name(names = jobs_needed[rows, "job_ids"])
    job$set_prereqs(jobs_needed[rows, "prereq_job_ids_with_throttle"])

    for (row in rows){
      state_update_last_invoked_time(jobs_needed[row, "job_ids"])
      job_env$joblist <- c(job_env$joblist, list(job))
      names(job_env$joblist)[length(job_env$joblist)] <- jobs_needed[rows, "job_ids"]
    }
  }
}

#' Run jobs
#'
#' @param debug A logical, indicating whether debug messages should be shown
run_jobs <- function(debug = FALSE){
  joblist <- job_env$joblist
  lapply(joblist, function(job){
    job$execute(debug = debug)
  })
}
