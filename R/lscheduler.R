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

  incant <- paste("R CMD BATCH --no-save --no-restore --no-timing",
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

lscheduler_jobid <- function(){
  substring(tempfile(pattern = "", tmpdir = ""), first = 2)
}

#' Add to lscheduler
#'
#' @param name job name
#' @param action a set of expressions
lscheduler_add <- function(incant, dependency){
  jobid <- lscheduler_jobid()
  lscheduler_env$incant <- c(lscheduler_env$incant, list(incant))
  lscheduler_env$started <- c(lscheduler_env$started, FALSE)
  lscheduler_env$finished <- c(lscheduler_env$finished, FALSE)
  lscheduler_env$dependency <- c(lscheduler_env$dependency, list(dependency))
  lscheduler_env$process <- c(lscheduler_env$process, list(NA))
  n <- length(lscheduler_env$finished)
  names(lscheduler_env$finished)[n] <- jobid
  as.character(jobid)
}

# returns numeric(0) if nothing to start
lscheduler_start_next <- function(){
  dependency <- lscheduler_env$dependency
  not_started <- which(!lscheduler_env$started)
  finished <- lscheduler_env$finished

  startable <- sapply(not_started, function(i){
    depend <- dependency[[i]]
    all(finished[depend])
  })
  if (any(is.na(startable))){
    browser()
  }

  start_next <- match(TRUE, startable, nomatch = FALSE)
  cat("Startable: ", startable, "\n")
  cat("Start next:", not_started[start_next], "\n")
  not_started[start_next]
}

lscheduler_number_running <- function(){
  running <- lscheduler_env$started & !lscheduler_env$finished

  for (i in which(running)){
    if (!lscheduler_env$process[[i]]$is_alive()){
      lscheduler_env$finished[i] <- TRUE
    }
  }

  running <- lscheduler_env$started & !lscheduler_env$finished
  sum(running)
}

lscheduler_run_next <- function(){
  number_running <- lscheduler_number_running()
  everything_started <- all(lscheduler_env$started)

  start_next <- lscheduler_start_next()
  something_startable <- length(start_next) != 0

  if (!everything_started && something_startable && number_running < 5){
    cat("RUNNING", lscheduler_env$incant[[start_next]], "\n")
    lscheduler_env$started[start_next] <- TRUE
    incant <- lscheduler_env$incant[[start_next]]
    p <- processx::process$new(commandline = incant,
                               windows_verbatim_args = TRUE)
    lscheduler_env$process[[start_next]] <- p
  }
}

lscheduler_finished <- function(){
  all(lscheduler_env$finished)
}


lscheduler_started <- function(){
  sum(lscheduler_env$started)
}

lscheduler_total <- function(){
  length(lscheduler_env$started)
}

lscheduler_kill_all <- function(){
  lapply(lscheduler_env$process, function(p){
    p$kill()
  })
}
