TaskArm <- setRefClass(
  "TaskArm",
  fields = list(
    task_name = "character",
    taskarm_name = "character",
    arm_index = "integer",
    prereqs = "character", # a list of task names
    actions = "list",
    properties = "list",
    custom_last_edited_time = "list"
  ),
  methods = list(
    invoke = function(debug = FALSE){
      if (needed(debug = debug)){
        debug_msg(debug, "Invoking ", taskarm_name)

        update_last_invoked_time()
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
      !ever_invoked() ||
        edited_since_last_invoked() ||
        prerequisite_invoked_more_recently()
    },

    last_edited_time = function(){
      "Returns POSIXct with custom_last_edited_time (or Unix epoch)"
      have_custom <- length(custom_last_edited_time) > 0
      ifelse(have_custom,
             custom_last_edited_time[[1]](name = task_name),
             MNGR_UNIX_EPOCH)
    },

    state_path = function(){
      "Get path to the state file for this taskarm"
      state_dir <- mngr_option_dir_state()(fs::path_tidy(getwd()))
      fs::dir_create(state_dir)
      fs::path(state_dir, taskarm_name)
    },

    update_last_invoked_time = function(){
      "Mark this taskarm as just done. ie touch the state_path"
      state_file <- state_path()
      fs::file_create(state_file)
      Sys.setFileTime(state_file, Sys.time())
    },

    last_invoked_time = function(){
      "Returns POSIXct with the last_invoked_time (or Unix epoch if never
       invoked)"
      if (ever_invoked()){
        file.info(state_path())$mtime
      } else{
        MNGR_UNIX_EPOCH
      }
    },

    ever_invoked = function(){
      "Returns TRUE if this taskarm has ever been invoked"
      file.exists(state_path())
    },

    edited_since_last_invoked = function(){
      "Returns TRUE if last_edited_time is after last_invoked_time"
      last_edited_time() > last_invoked_time()
    },

    prerequisite_invoked_more_recently = function(){
      "Returns TRUE if any prerequisite has been invoked since this taskarm
       was last invoked"
      if (length(prereqs) > 0){
        prereqs_last_invoked_time <- lapply(prereqs, function(taskarm_name){
          taskarm_get(taskarm_name, exists = TRUE)$last_invoked_time()
        })
        do.call("max", prereqs_last_invoked_time) > last_invoked_time()
      } else {
        FALSE
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
