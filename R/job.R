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
  jobs_needed$group <- rep(seq_len(ngroups), each = 1, len = nrow(jobs_needed))

  for (group in seq_len(ngroups)){
    rows <- which(jobs_needed$group == group)
    state_update_last_invoked_time(jobs_needed[rows, "job_ids"])
    job <- scheduler_fun(name = jobs_needed[rows, "job_ids"],
                         basename = jobs_needed[rows, "task_name"],
                         arm_index = jobs_needed[rows, "arm_index"],
                         actions = actions,
                         prereqs = as.character(unlist(jobs_needed[rows, "prereq_job_ids_with_throttle"])),
                         properties = properties)
    job_env$joblist <- c(job_env$joblist, list(job))
    names(job_env$joblist)[length(job_env$joblist)] <- jobs_needed[rows, "job_ids"]
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
