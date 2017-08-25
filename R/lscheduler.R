lscheduler_job <- function(task){
  id <- job_find_id(task$name, exists = TRUE)
  task_obj <- job_env$joblist[[id]]
  path <- normalizePath(paste0(task$basename, ".R"), winslash = "/")

  # if any shared arms, then depend on all prereqs, not just the matching
  # index
  dependency <- task_obj$jobid_prereqs()

  r_log_specific_path <- task$r_log_specific_file(ensure_dir = TRUE)
  r_log_latest_path <- task$r_log_latest_file(ensure_dir = TRUE)
  r_log_path <<- r_log_specific_path

  incant <- paste("R CMD BATCH --no-save --no-restore",
                  "\"--args -1",
                  task$basename,
                  task$arm_index,
                  "\"", path,
                  r_log_latest_path)
  # need to mirror logs to r_log_specific_path too

  jobid <- lscheduler_add(incant = incant,
                          dependency = task_obj$jobid_prereqs())
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", task$name, " (", jobid, ")")

  task$set_jobid(jobid)
  slurm_add_jobids(jobid)
}

#' Add to lscheduler
#'
#' @param name job name
#' @param action a set of expressions
lscheduler_add <- function(incant, dependency){
  lscheduler_env$incant <- c(lscheduler_env$incant, list(incant))
  lscheduler_env$started <- c(lscheduler_env$started, FALSE)
  lscheduler_env$finished <- c(lscheduler_env$finished, FALSE)
  lscheduler_env$dependency <- c(lscheduler_env$dependency, list(dependency))
  lscheduler_env$process <- c(lscheduler_env$process, list(NA))

  jobid <- length(lscheduler_env$incant)
  as.character(jobid)
}

lscheduler_run_next <- function(){
  start_next <- match(FALSE, lscheduler_env$started, nomatch = 1)
  depend <- as.integer(lscheduler_env$dependency[[start_next]])

  running <- lscheduler_env$started & !lscheduler_env$finished

  for (i in which(running)){
    if (!lscheduler_env$process[[i]]$is_alive()){
      lscheduler_env$finished[i] <- TRUE
    }
  }

  running <- lscheduler_env$started & !lscheduler_env$finished
  dependencies_for_next_are_done <- all(lscheduler_env$finished[depend])

  if (dependencies_for_next_are_done & sum(running) < 2){
    cat("RUNNING", lscheduler_env$incant[[start_next]], "\n")
    lscheduler_env$started[start_next] <- TRUE
    p <- processx::process$new(commandline = lscheduler_env$incant[[start_next]])
    lscheduler_env$process[[start_next]] <- p
  }
}

lscheduler_finished <- function(){
  all(lscheduler_env$finished)
}
