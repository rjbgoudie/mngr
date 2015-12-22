#' Build factorial experiment
#' @param ... passed to expand.grid
#' @export
arms_factorial <- function(...){
  arms_list <- list(...)
  assign("arms_list", arms_list, envir = task_env)
}

arms_all <- function(task, include_shared = TRUE){
  id <- task_find_id(task, exists = TRUE)
  task_obj <- task_env$tasklist[[id]]

  arms_list <- task_env$arms_list
  share <- task_obj$getShared()
  which_arms_shared <- names(arms_list) %in% share
  arms_unshared <- do.call("expand.grid", arms_list[!which_arms_shared])
  arms_unshared <- lapply(seq_len(nrow(arms_unshared)), function(i){
    as.list(arms_unshared[i, ])
  })
  if (include_shared){
    if (sum(which_arms_shared) > 0){
      arms_shared <- arms_list[which_arms_shared]
      values <- lapply(seq_along(arms_unshared), function(x) arms_shared)
      arms_unshared <- mapply(append, arms_unshared, values, SIMPLIFY = FALSE)
    }
    arms_unshared
  } else {
    arms_unshared
  }
}

#' Unique name for arm
arm_name <- function(){
  arms <- arms_all(.task, include_shared = TRUE)

  arm <- arms[[.arm]]
  o <- order(names(arm))
  arm <- arm[o]
  arm_values <- sapply(arm, paste, collapse = "-")
  arm_names <- names(arm)
  paste(arm_names, arm_values, sep = "--",  collapse = "__")
}

arm_name_shared <- function(){
  arms <- arms_all(.task, include_shared = TRUE)

  arm <- arms[[.arm]]
  o <- order(names(arm))
  arm <- arm[o]
  grid <- expand.grid(arm)
  apply(grid, 1, function(arm){
    arm_values <- sapply(arm, paste, collapse = ",")
    arm_names <- names(arm)
    paste(arm_names, arm_values, sep = "--",  collapse = "__")
  })
}

#' @export
`%share%` <- function(taskname, share){
  stopifnot(inherits(taskname, "character"))
  task_id <- task_find_id(taskname, exists = TRUE)
  task_env$tasklist[[task_id]]$add_shared(share)
  invisible(taskname)
}

#' @export
read_rds_shared <- function(...){
  all_arms <- arm_name_shared()
  all_paths <- sapply(all_arms, function(x){
    rds_file(..., arm = x)
  })
  lapply(all_paths, readRDS)
}

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
      timestamp_newer_than_last_run <- timestamp() > last_run_date
    }

    prerequisite_run_more_recently <- FALSE
    most_recent_prereq <- FALSE
    if (length(prereqs) > 0){
      prereqs_timestamp <- lapply(prereqs, function(taskarm_name){
        id <- taskarm_find_id(taskarm_name, exists = TRUE)
        taskarm_env$taskarmlist[[id]]$timestamp()
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
    out
  },
  state_file = function(ensure_dir = TRUE, create = FALSE){
    state_fun <- task_env$config$state
    state_dir <- state_fun(normalizePath("."))

    if (ensure_dir){
      ensure_exists(state_dir)
    }
    state_file <- paste0(state_dir, "/", taskarm_name)

    if (create){
      file.create(state_file)
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

#' Find taskarm id
#'
#' @param name taskarm name
#' @param exists does the taskarm exist?
taskarm_find_id <- function(name, exists = taskarm_exists(name)){
  if (exists){
    which(name == names(taskarm_env$taskarmlist))
  } else {
    message("Taskarm ", name, " does not exist")
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
