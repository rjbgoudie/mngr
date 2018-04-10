TaskArm <- setRefClass(
  "TaskArm",
  fields = list(
    task_name = "character",
    taskarm_name = "character",
    arm_index = "integer",
    prereqs = "character", # a list of task names
    actions = "list",
    properties = "list",
    custom_timestamp = "list"
  ),
  methods = list(
    invoke = function(debug = FALSE){
      if (needed(debug = debug)){
        debug_msg(debug, "Invoking ", taskarm_name)

        state_file(ensure_dir = TRUE, create = TRUE)
        job_create(name = taskarm_name,
                   basename = task_name,
                   arm_index = arm_index,
                   actions = actions,
                   prereqs = prereqs,
                   properties = properties)
      } else {
        debug_msg(debug, taskarm_name, " not needed")
      }
    },
    needed = function(debug = FALSE){
      state_file <- state_file(ensure_dir = FALSE, create = FALSE)
      never_run <- !file.exists(state_file)

      timestamp_newer_than_last_run <- FALSE
      if (!never_run){
        last_run_date <- file.info(state_file)$mtime
        ts <- timestamp()
        debug_msg(debug, "lastrundate:", last_run_date, ". timestamp: ", ts)
        timestamp_newer_than_last_run <- ts > last_run_date
      }

      prerequisite_run_more_recently <- FALSE
      most_recent_prereq <- FALSE
      if (length(prereqs) > 0){
        prereqs_timestamp <- lapply(prereqs, function(taskarm_name){
          taskarm_get(taskarm_name, exists = TRUE)$timestamp()
        })
        most_recent_prereq <- do.call("max", prereqs_timestamp)
      }
      prerequisite_run_more_recently <- most_recent_prereq > timestamp()

      # flesh this out
      build_all <- FALSE

      out <- never_run ||
        timestamp_newer_than_last_run ||
        prerequisite_run_more_recently ||
        build_all

      debug_msg(debug,
                "Needed status for ", taskarm_name, " is ", out, ". ",
                "never_run: ", never_run, ". ",
                "prerequistite_run_more_recently: ", prerequisite_run_more_recently,
                ". ",
                "build_all: ", build_all, ".")
      out
    },
    timestamp = function(){
      state_file <- state_file(ensure_dir = FALSE, create = FALSE)

      if (file.exists(state_file)){
        out <- file.info(state_file)$mtime
      } else {
        out <- Sys.time()
      }
      if (length(custom_timestamp) > 0){
        custom <- custom_timestamp[[1]](name = task_name)
        if (custom > out){
          out <- custom
        }
      }
      as.POSIXct(out)
    },
    state_file = function(ensure_dir = TRUE, create = FALSE){
      state_fun <- task_env$config$state
      state_dir <- state_fun(normalizePath(".", winslash = "/"))

      if (ensure_dir){
        ensure_exists(state_dir)
      }
      state_file <- paste0(state_dir, "/", taskarm_name)

      if (create){
        file.create(state_file)
        Sys.setFileTime(state_file, Sys.time())
      } else {
        state_file
      }
    }
  )
)

#' Check for taskarm existence
#'
#' @param name name of taskarm
taskarm_exists <- function(name){
  taskarm_exists <- FALSE
  taskarmlist_non_null <- length(taskarm_env$taskarmlist) > 0
  if (taskarmlist_non_null){
    taskarm_exists <- name %in% names(taskarm_env$taskarmlist)
  }
  taskarm_exists
}

#' Get a taskarm
#'
#' @param name taskarm name
#' @param exists does the taskarm exist?
taskarm_get <- function(name, exists = taskarm_exists(name)){
  if (exists){
    id <- which(name == names(taskarm_env$taskarmlist))
    taskarm_env$taskarmlist[[id]]
  } else {
    NULL
  }
}

#' Create taskarm
#'
#' @param name taskarm name
#' @param action a set of expressions
taskarm_create <- function(taskarm_name, ...){
  taskarm <- TaskArm(taskarm_name = taskarm_name, ...)
  taskarm_env$taskarmlist <- c(list(taskarm), taskarm_env$taskarmlist)
  names(taskarm_env$taskarmlist)[1] <- taskarm_name
}
