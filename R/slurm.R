slurm_r_job <- function(task){
  id <- job_find_id(task$name, exists = TRUE)
  task_obj <- job_env$joblist[[id]]
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
    cat(incant)
    jobids <- c(jobids, jobid)
  }
  task$set_jobid(jobids)
  slurm_add_jobids(jobids)
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
    actions = "list",
    prereqs = "character",
    jobid = "character",
    prereq_jobids = "character",
    shared = "list",
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
  jobid_prereqs = function(index){
    if (length(prereqs) > 0){
      unlist(sapply(prereqs, function(name){
        id <- job_find_id(name)
        if (!is.null(id)){
          parent <- job_env$joblist[[id]]$jobid

          if (length(parent) == 0){
            parent <- job_env$joblist[[id]]$jobid_prereqs(index)
          }
          if (length(parent) > 1){
            parent <- parent[index]
          }
          parent
        } else {
          c()
        }
      }))
      } else {
        c()
      }
    },
    get_memory = function(){
      if (length(properties$memory) > 0){
        properties$memory
      } else {
        3993
      }
    },
    get_cores = function(){
      if (length(properties$cores) > 0){
        properties$cores
      } else {
        1
      }
    },
    r_log_latest_file = function(ensure_dir = TRUE, index){
      r_log_fun <- task_env$config$r_logs
      r_log_dir <- r_log_fun(normalizePath("."))
      r_log_latest_dir <- paste0(r_log_dir, "-latest/")

      if (ensure_dir){
        ensure_exists(r_log_latest_dir)
      }
      r_log_latest_file <- paste0(name, "_", index, ".Rout")
      file.path(r_log_latest_dir, r_log_latest_file)
    },
    r_log_specific_file = function(ensure_dir = TRUE, index){
      r_log_fun <- task_env$config$r_logs
      r_log_dir <- r_log_fun(normalizePath("."))

      if (ensure_dir){
        ensure_exists(r_log_dir)
      }
      r_log_latest_file <- paste0(name, "_", index, ".Rout")
      r_log_specific_file <- paste0("\\${SLURM_JOB_ID}_", r_log_latest_file)
      file.path(r_log_dir, r_log_specific_file)
    },
    any_shared = function(){
      length(shared) > 0
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
