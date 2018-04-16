lscheduler_job <- function(task){
  id <- job_find_id(task$name, exists = TRUE)
  task_obj <- job_env$joblist[[id]]
  path <- normalizePath(paste0(task$basename, ".R"), winslash = "/")

  # if any shared arms, then depend on all prereqs, not just the matching
  # index
  dependency <- task_obj$jobid_prereqs()

  jobid <- lscheduler_jobid()
  task$set_jobid(jobid)

  r_log_specific_path <- task$r_log_specific_file()
  r_log_latest_path <- task$r_log_latest_file()
  r_log_path <<- r_log_specific_path

  if (have_tee()){
    incant <- paste("cat",
                    path,
                    "| R --no-save --no-restore",
                    "\"--args -1",
                    task$basename,
                    task$arm_index,
                    "\"",
                    "2>&1 | tee",
                    r_log_specific_path,
                    r_log_latest_path)
  } else {
    incant <- paste("R CMD BATCH --no-save --no-restore --no-timing",
                    "\"--args -1",
                    task$basename,
                    task$arm_index,
                    "\"", path,
                    r_log_latest_path)
    # need to mirror logs to r_log_specific_path too
  }

  jobid <- lscheduler_add(incant = incant,
                          dependency = task_obj$jobid_prereqs(),
                          jobid = jobid)
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", task$name, " (", jobid, ")")

  slurm_add_jobids(jobid)
}

lscheduler_jobid <- function(){
  substring(tempfile(pattern = "", tmpdir = ""), first = 2)
}

#' Add to lscheduler
#'
#' @param name job name
#' @param action a set of expressions
lscheduler_add <- function(incant, dependency, jobid){
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

LSchedulerJob <- setRefClass(
  "LSchedulerJob",
  fields = list(
    name = "character",
    basename = "character",
    arm_index = "integer",
    actions = "list",
    prereqs = "character",
    jobid = "character",
    prereq_jobids = "character",
    properties = "list"
  ),
  methods = list(
    execute = function(debug = FALSE){
      action_class <- sapply(actions, class)
      lapply(actions[action_class == "{"], eval.parent)
      lapply(actions[action_class == "call"], function(action){
        eval(action)(.self)
      })
      lapply(actions[action_class == "function"], function(action){
        action(.self)
      })
    },
    set_jobid = function(x){
      jobid <<- x
    },
    jobid_prereqs = function(){
      out <- c()
      if (length(prereqs) > 0){
        out <- unlist(sapply(prereqs, function(name){
          id <- job_find_id(name)
          if (!is.null(id)){
            parent <- job_env$joblist[[id]]$jobid
          } else {
            c()
          }
        }))
      }
      out
    },
    last_run_time = function(){
      path <- r_log_latest_file()
      if (file.exists(path)){
        run_time(path)
      } else {
        NULL
      }
    },
    r_log_latest_file = function(){
      r_log_latest_dir <- mngr_option_dir_r_logs_latest()(fs::path_tidy(getwd()))
      fs::dir_create(r_log_latest_dir)
      r_log_latest_file <- paste0(name, ".Rout")
      file.path(r_log_latest_dir, r_log_latest_file)
    },
    r_log_specific_file = function(){
      r_log_dir <- mngr_option_dir_r_logs()(fs::path_tidy(getwd()))
      fs::dir_create(r_log_dir)

      r_log_latest_file <- paste0(name, ".Rout")
      r_log_specific_file <- paste0(jobid, "__", r_log_latest_file)
      file.path(r_log_dir, r_log_specific_file)
    },
    jobname = function(){
      paste(name, git_short_sha(), sep = "__")
    }
  )
)
