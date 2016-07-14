slurm_r_job <- function(task){
  id <- job_find_id(task$name, exists = TRUE)
  task_obj <- job_env$joblist[[id]]
  path <- normalizePath(paste0(task$basename, ".R"))

  slurm_log_path <- task$slurm_file(ensure_dir = TRUE)

  queue <- task_env$config$queue %||% "sand"

  # if any shared arms, then depend on all prereqs, not just the matching
  # index
  dependency <- task_obj$jobid_prereqs()

  memory <- task_obj$get_memory()
  cores <- task_obj$get_cores()

  dependency <- if (!is.null(dependency) && length(dependency) > 0){
    paste0("--dependency=afterok:", paste(dependency, collapse = ","), " ")
  } else {
    ""
  }

  run_time <- task_obj$predict_run_time()

  r_log_specific_path <- task$r_log_specific_file(ensure_dir = TRUE)
  r_log_latest_path <- task$r_log_latest_file(ensure_dir = TRUE)
  r_log_path <<- r_log_specific_path

  incant <-
    paste0("MNGR_RFILE=", path,
           " MNGR_RLOGFILE=", r_log_specific_path,
           " MNGR_RLOGLATESTFILE=", r_log_latest_path,
           " MNGR_TASKNAME=", task$basename,
           " MNGR_ARM=", task$arm_index,
           " sbatch -J ", task$jobname(),
           " --parsable ",
           dependency,
           " --mem=", memory,
           " --ntasks=", cores,
           " --nodes=1",
           " --time=", run_time,
           " --output=", slurm_log_path,
           " ", getOption("mngr_cluster_path"), "/mngr_slurm_submit.", queue,
           "\n")
  jobid <- system(incant, intern = TRUE)
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", task$name, " (", jobid, ")",
          " Run time prediction ", paste(run_time, collapse = ":"))
## cat(incant)

task$set_jobid(jobid)
slurm_add_jobids(jobid)
}

slurm_add_jobids <- function(new){
  jobids <- c(slurm_env$jobids, new)
  assign("jobids", jobids, envir = slurm_env)
}

run_jobs <- function(debug = FALSE){
  joblist <- job_env$joblist
  lapply(joblist, function(job){
    job$execute(debug = debug)
  })
}

SlurmJob <- setRefClass(
  "SlurmJob",
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
    slurm_file = function(ensure_dir = TRUE){
    slurm_log_fun <- task_env$config$slurm_logs
    slurm_log_dir <- slurm_log_fun(normalizePath("."))

    if (ensure_dir){
      ensure_exists(slurm_log_dir)
    }

    slurm_log_file <- paste0("%A.%a-", name, ".txt")
    file.path(slurm_log_dir, slurm_log_file)
  },
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
  get_memory = function(){
    if (length(properties$memory) > 0){
      properties$memory
    } else {
      3840 #3993
    }
  },
  get_cores = function(){
    if (length(properties$cores) > 0){
      properties$cores
    } else {
      1
    }
  },
  last_run_time = function(){
    path <- r_log_latest_file(ensure_dir = FALSE)
    if (file.exists(path)){
      run_time(path)
    } else {
      NULL
    }
  },
  predict_run_time = function(){
    last <- last_run_time()
    if (!is.null(last)){
      multiply <- 2
      extra <- 10
      overflow3 <- (multiply * last[3]) %/% 60
      last[3] <- (multiply * last[3]) %% 60
      overflow2 <- (multiply * last[2] + overflow3 + extra) %/% 60
      last[2] <- (multiply * last[2] + overflow3 + extra) %% 60
      overflow1 <- (multiply * last[1] + overflow2) %/% 60
      last[1] <- (multiply * last[1] + overflow2) %% 60
      args <- c("%d:%02d:%02d", as.list(last))
      do.call("sprintf", args)
    } else {
      "48:00:00"
    }
  },
  r_log_latest_file = function(ensure_dir = TRUE){
    r_log_fun <- task_env$config$r_logs
    r_log_dir <- r_log_fun(normalizePath("."))
    r_log_latest_dir <- paste0(r_log_dir, "-latest/")

    if (ensure_dir){
      ensure_exists(r_log_latest_dir)
    }
    r_log_latest_file <- paste0(name, ".Rout")
    file.path(r_log_latest_dir, r_log_latest_file)
  },
  r_log_specific_file = function(ensure_dir = TRUE){
    r_log_fun <- task_env$config$r_logs
    r_log_dir <- r_log_fun(normalizePath("."))

    if (ensure_dir){
      ensure_exists(r_log_dir)
    }
    r_log_latest_file <- paste0(name, ".Rout")
    r_log_specific_file <- paste0("\\${SLURM_JOB_ID}_", r_log_latest_file)
    file.path(r_log_dir, r_log_specific_file)
  },
  jobname = function(){
    paste(name, git_short_sha(), sep = "--")
  }
  )
)

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
job_create <- function(name, ...){
  job <- SlurmJob(name = name, ...)
  job_env$joblist <- c(job_env$joblist, list(job))
  names(job_env$joblist)[length(job_env$joblist)] <- name
}
