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

#' Find task id
#'
#' @param name task name
#' @param exists does the task exist?
task_find_id <- function(name, exists = task_exists(name)){
  if (exists){
    which(name == names(task_env$tasklist))
  } else {
    message("does not exist")
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
  id <- task_find_id(name, exists = TRUE)
  task_env$tasklist[[id]]$add_action(action)
  invisible(TRUE)
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
  id_a <- task_find_id(a, exists = TRUE)
  task_env$tasklist[[id_a]]$add_prereqs(b)
  invisible(TRUE)
}

#' @export Task
Task <- setRefClass(
  "Task",
  fields = list(
    name = "character",
    prereqs = "character", # a list of task names
    actions = "list",
    already_invoked = "logical",
    needed = "logical",
    jobid = "character",
    prereq_jobids = "character",
    r_log_path = "character"
  ),
  methods = list(
    initialize = function(name = name){
    name <<- name
    already_invoked <<- FALSE
  },
  set_name = function(x) {
    name <<- x
  },
  add_prereqs = function(x) {
    prereqs <<- c(x, prereqs)
  },
  add_action = function(x) {
    actions <<- c(list(x), actions)
  },
  invoke = function() {
    if (!already_invoked){
      already_invoked <<- TRUE
      invoke_prereqs()
      lapply(actions, eval.parent)
    }
    unique(c(prereq_jobids, jobid))
  },
  invoke_prereqs = function(){
    if (length(prereqs) > 0){
      prereq_jobids <<- unlist(sapply(prereqs, function(name){
        id <- task_find_id(name, exists = TRUE)
        task_env$tasklist[[id]]$invoke()
      }))
    }
  },
  jobid_prereqs = function(){
    if (length(prereqs) > 0){
      sapply(prereqs, function(name){
        id <- task_find_id(name, exists = TRUE)
        task_env$tasklist[[id]]$jobid
      })
    } else {
      c()
    }
  }
  )
)
