#' Check for task existence
#'
#' @param name name of task
task_exists <- function(name){
  task_exists <- FALSE
  tasklist_non_null <- length(task_env$tasklist) > 0
  if (tasklist_non_null){
    task_exists <- name %in% names(task_env$tasklist)
  }
  task_exists
}

#' Get a task
#'
#' @param name task name
#' @param exists does the task exist?
task_get <- function(name, exists = task_exists(name)){
  if (exists){
    id <- which(name == names(task_env$tasklist))
    task_env$tasklist[[id]]
  } else {
    NULL
  }
}

#' Create task
#'
#' @param name task name
#' @param action a set of expressions
task_create <- function(name, action){
  task <- Task(name = name)
  task_env$tasklist <- c(list(task), task_env$tasklist)
  names(task_env$tasklist)[1] <- name
}

#' Create a new task
#'
#' @param name task name
#' @param action a set of expressions
#' @author RJB Goudie
#' @export
`%task%` <- task <- function(name, action){
  expr <- substitute(name)
  action <- substitute(action)
  if (is.name(expr)){
    name <- as.character(expr)
  }
  if (task_exists(name)){
    message("already exists")
  } else {
    task_create(name, action)
  }
  # this is inefficient for newly created
  task_obj <- task_get(name, exists = TRUE)
  task_obj$enhance(actions_new = action)
  invisible(a)
}

#' Set up task dependencies
#'
#' @param a task name
#' @param b task name
#' @author RJB Goudie
#' @export
`%d%` <- `%depends%` <- depends <- function(a, b){
  a <- substitute(a)
  b <- substitute(b)
  if (is.name(a)){
    a <- as.character(a)
  }
  if (is.name(b)){
    b <- as.character(b)
  }
  if (!task_exists(a)){
    task_create(a, {})
  }
  if (!task_exists(b)){
    task_create(b, {})
  }
  task_a <- task_get(a, exists = TRUE)
  task_a$enhance(prereqs_new = b)
  invisible(a)
}

#' @export Task
Task <- setRefClass(
  "Task",
  fields = list(
    name = "character",
    prereqs = "character", # a list of task names
    actions = "list",
    already_invoked = "logical",
    shared = "list",
    properties = "list",
    custom_timestamp = "list"
  ),
  methods = list(
    initialize = function(...){
    already_invoked <<- FALSE
    custom_timestamp <<- list()
    actions <<- list()
    prereqs <<- character(0)
    initFields(...)
  },
  is_dummy = function(){
    length(actions) == 0
  },
  taskarm_name = function(arm_index){
    paste(name, arm_index, sep = "_")
  },
  enhance = function(prereqs_new = NULL, actions_new = NULL){
    if (!is.null(prereqs_new)){
      prereqs <<- c(prereqs_new, prereqs)
    }
    if (!is.null(actions_new)){
      actions <<- c(list(actions_new), actions)
    }
  },
  prereq_taskarm_names = function(arm_index){
    prereq_tasks <- sapply(prereqs, task_get)
    prereq_tasks <- Filter(not.null, prereq_tasks)

    out <- lapply(prereq_tasks, function(prereq_task){
      if (prereq_task$is_dummy()){
        prereq_task$prereq_taskarm_names(arm_index)
      } else {
        prereq_task$taskarm_name(arm_index)
      }
    })
    as.character(unlist(out))
  },
  invoke = function(debug = FALSE) {
    if (!already_invoked){
      already_invoked <<- TRUE

      debug_msg(debug, "Invoking prerequisties for ", name)
      invoke_prereqs(debug = debug)

      arms_count <- 1
      if (length(actions) > 0){
        arms <- arms_all(name, include_shared = FALSE)
        arms_count <- length(arms)
      }

      debug_msg(debug, "Building ", arms_count, " arms for ", name)
      for (arm_index in seq_len(arms_count)){
        taskarm_create(task_name = name,
                       taskarm_name = taskarm_name(arm_index),
                       arm_index = arm_index,
                       prereqs = prereq_taskarm_names(arm_index),
                       actions = actions,
                       properties = properties,
                       custom_timestamp = custom_timestamp)
        id <- taskarm_find_id(name_with_array, exists = TRUE)
        taskarm_env$taskarmlist[[id]]$invoke(debug = debug)
      }
    }
  },
  invoke_prereqs = function(debug = FALSE){
    if (length(prereqs) > 0){
      debug_msg(debug, "Running prereqs for ", name)

      sapply(prereqs, function(name){
        task_obj <- task_get(name, exists = TRUE)
        task_obj$invoke(debug = debug)
      })
    }
  },
  add_shared = function(new_shared){
    shared <<- c(shared, new_shared)
  },
  getShared = function(){
    shared
  },
  set_properties = function(...){
    new <- list(...)
    to_replace <- match(names(new), names(properties))
    properties[to_replace] <<- new
    to_add <- !(names(new) %in% names(properties))
    properties <<- c(properties, new[to_add])
  },
  set_custom_timestamp = function(f){
    custom_timestamp[[1]] <<- f
  }
  )
)
