slurm_r_job <- function(job){
  path <- normalizePath(paste0(job$basename, ".R"), winslash = "/")
  slurm_log_path <- job$slurm_file()

  r_log_specific_path <- job$r_log_specific_file()
  r_log_latest_path <- job$r_log_latest_file()
  r_log_path <<- r_log_specific_path

  # no dependency across arms is allowed, so just take union of prereqs
  #
  # if any shared arms, then depend on all prereqs, not just the matching
  # index
  dependency <- job$jobid_prereqs()
  dependency <- if (!is.null(dependency) && length(dependency) > 0){
    paste0("--dependency=afterok:", paste(dependency, collapse = ","), " ")
  } else {
    ""
  }

  memory <- job$get_memory()
  cores <- job$get_cores()
  run_time <- job$predict_run_time()

  queue <- task_env$config$queue %||% "default"
  dry_run <- task_env$config$dry_run %||% FALSE

  arr_sep_fun <- function(x){
    arr_sep <- ";"
    paste0(x, collapse = arr_sep)
  }
  incant <-
    paste0("MNGR_RFILE=", path,
           " MNGR_RLOGFILE=\"", arr_sep_fun(r_log_specific_path), "\"",
           " MNGR_RLOGLATESTFILE=\"", arr_sep_fun(r_log_latest_path), "\"",
           " MNGR_TASKNAME=", job$basename,
           " MNGR_ARM=\"", arr_sep_fun(job$arm_index), "\"",
           " sbatch -J ", job$jobname(),
           " --parsable ",
           dependency,
           " --mem=", memory * length(job$arm_index),
           " --ntasks=", cores * length(job$arm_index),
           " --nodes=1",
           " --time=", run_time,
           " --output=", slurm_log_path,
           " ", mngr_option_slurm_submit_path(), "/", queue,
           "\n")

  jobid <- if (!dry_run){
    system(incant, intern = TRUE)
  } else {
    cat(incant)
    "0"
  }
  time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
  message(time, " Submitted ", job$name, " (", jobid, ")",
          " Run time prediction ", paste(run_time, collapse = ":"))
  ## cat(incant)

  job$set_jobid(jobid)
  slurm_add_jobids(jobid)
}

#' Add Job ID to list of Job IDs
#'
#' @param new A new Job ID
slurm_add_jobids <- function(new){
  jobids <- c(slurm_env$jobids, new)
  assign("jobids", jobids, envir = slurm_env)
}

SlurmJob <- setRefClass(
  "SlurmJob",
  fields = list(
    name = "character",
    names = "character",
    basename = "character",
    arm_index = "integer",
    actions = "list",
    prereqs = "character",
    jobid = "character",
    prereq_jobids = "character",
    properties = "list"
  ),
  methods = list(
    set_name = function(names){
      "Input is a character vector of names of the task(s) that this job
       corresponds to"
      # just use the first name for now for simplicity
      name <<- names[1]
      names <<- names
    },

    set_prereqs = function(prereqs){
      "Input is a list of lists, each component is a list of the prereqs of
       that Task. We just use the union of prereqs"
      prereqs <<- unique(as.character(unlist(prereqs)))
    },

    slurm_file = function(){
      slurm_log_dir <- mngr_option_dir_slurm_logs()(fs::path_tidy(getwd()))

      fs::dir_create(slurm_log_dir)
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

    last_run_time_seconds = function(){
      "Return the last run time of this job in seconds. If the job consists
       of multiple tasks, the maximum is returned"

      paths <- r_log_latest_file()
      run_times_seconds <- lapply(paths, function(path){
        if (file.exists(path)){
          run_time_seconds(path)
        } else {
          NULL
        }
      })
      out <- run_times_seconds[!sapply(run_times_seconds, is.null)]
      if (length(out) > 0){
        max(unlist(out))
      } else {
        NULL
      }
    },

    predict_run_time = function(){
      "Returns a character of format h:mm:ss with a prediction for the run time
       needed for this job. It will either be the specified hours(), or
       the last run time (+ 10 mins, with a floor of 14 mins) or 59 mins"

      if (length(properties$hours) > 0){
        sprintf("%d:00:00", properties$hours)
      } else {
        last_seconds <- last_run_time_seconds()
        if (!is.null(last_seconds)){
          ten_mins <- 10 * 60
          fourteen_mins <- 14 * 60
          # add on 10 mins to good luck
          # don't use less than 30 mins
          seconds <- max(last_seconds + ten_mins, fourteen_mins)
          hms(seconds)
        } else {
          "00:59:00"
        }
      }
    },

    r_log_latest_file = function(){
      out <- character(length = length(arm_index))
      for (index in seq_along(arm_index)){
        r_log_latest_dir <- mngr_option_dir_r_logs_latest()(fs::path_tidy(getwd()))
        fs::dir_create(r_log_latest_dir)

        r_log_latest_file <- paste0(names[index], ".Rout")
        out[index] <- file.path(r_log_latest_dir, r_log_latest_file)
      }
      out
    },

    r_log_specific_file = function(){
      out <- character(length = length(arm_index))
      for (index in seq_along(arm_index)){
        r_log_dir <- mngr_option_dir_r_logs()(fs::path_tidy(getwd()))
        fs::dir_create(r_log_dir)
        r_log_latest_file <- paste0(names[index], ".Rout")
        r_log_specific_file <- paste0("\\${SLURM_JOB_ID}__", r_log_latest_file)
        out[index] <- file.path(r_log_dir, r_log_specific_file)
      }
      out
    },

    jobname = function(){
      paste(name, git_short_sha(), sep = "__")
    }
  )
)

#' Convert seconds to formatted h:mm:ss string
#'
#' @param seconds A numeric number of seconds
#' @return A character vector of the time formatted as "h:mm:ss"
hms <- function(seconds){
  h <- seconds %/% 3600
  m <- seconds %/% 60 %% 60
  s <- seconds %% 60
  sprintf("%d:%02d:%02d",  h,  m,  s)
}
