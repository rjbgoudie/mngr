slurm_r_job <- function(job){
  path <- normalizePath(paste0(job$basename, ".R"), winslash = "/")
  slurm_log_path <- job$slurm_file()

  r_log_specific_path <- job$r_log_specific_file()
  r_log_latest_path <- job$r_log_latest_file()
  r_log_path <<- r_log_specific_path

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

  incant <-
    paste0("MNGR_RFILE=", path,
           " MNGR_RLOGFILE=", r_log_specific_path,
           " MNGR_RLOGLATESTFILE=", r_log_latest_path,
           " MNGR_TASKNAME=", job$basename,
           " MNGR_ARM=", job$arm_index,
           " sbatch -J ", job$jobname(),
           " --parsable ",
           dependency,
           " --mem=", memory,
           " --ntasks=", cores,
           " --nodes=1",
           " --time=", run_time,
           " --output=", slurm_log_path,
           " ", mngr_option_slurm_submit_path(), "/", queue,
           "\n")

  jobid <- if (!dry_run){
    system(incant, intern = TRUE)
  } else {
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

    last_run_time = function(){
      path <- r_log_latest_file()
      if (file.exists(path)){
        run_time(path)
      } else {
        NULL
      }
    },

    predict_run_time = function(){
      if (length(properties$hours) > 0){
        sprintf("%d:00:00", properties$hours)
      } else {
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
          "01:00:00"
        }
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
      r_log_specific_file <- paste0("\\${SLURM_JOB_ID}__", r_log_latest_file)
      file.path(r_log_dir, r_log_specific_file)
    },

    jobname = function(){
      paste(name, git_short_sha(), sep = "__")
    }
  )
)
