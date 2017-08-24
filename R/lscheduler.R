lscheduler_job <- function(task){
  id <- job_find_id(task$name, exists = TRUE)
  task_obj <- job_env$joblist[[id]]
  path <- normalizePath(paste0(task$basename, ".R"))

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

  jobid <- lscheduler_add(list(incant = incant,
                           dependency = task_obj$jobid_prereqs(),
                           started = FALSE,
                           finished = FALSE))
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", task$name, " (", jobid, ")")

  task$set_jobid(jobid)
  slurm_add_jobids(jobid)
}

#' Add to lscheduler
#'
#' @param name job name
#' @param action a set of expressions
lscheduler_add <- function(job){
  lscheduler_env$q <- c(lscheduler_env$q, list(job))
  jobid <- length(lscheduler_env$q)
  as.character(jobid)
}

lscheduler_run_next <- function(){
  q <- lscheduler_env$q
  started <- sapply(lscheduler_env$q, "[[", "started")
  start_next <- match(FALSE, started, nomatch = 1)
  depend <- as.integer(lscheduler_env$q[[start_next]]$dependency)
  finished <- sapply(lscheduler_env$q, "[[", "finished")

  running <- q[started & !finished]
  for (i in seq_along(running)){
    if (!running[[i]]$process$is_alive()){
      lscheduler_env$q[started & !finished][[i]]$finished <- TRUE
      finished[started & !finished][i] <- TRUE
    }
  }

  if (all(finished[depend]) & sum(started & !finished) < 2){
    cat("RUNNING", lscheduler_env$q[[start_next]]$incant, "\n")
    lscheduler_env$q[[start_next]]$started <- TRUE
    p <- processx::process$new(commandline = lscheduler_env$q[[start_next]]$incant)
    lscheduler_env$q[[start_next]][["process"]] <- p
  }
}

lscheduler_finished <- function(){
  finished <- sapply(lscheduler_env$q, "[[", "finished")
  all(finished)
}
