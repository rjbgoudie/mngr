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
    jobid = "character",
    prereq_jobids = "character",
    r_log_path = "character"
  ),
  methods = list(
    initialize = function(...){
    already_invoked <<- FALSE
    initFields(...)
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
    if (!isTRUE(already_invoked)){
      already_invoked <<- TRUE
      invoke_prereqs()
      if (isTRUE(.self$needed())){
        state_fun <- task_env$config$state
        state_dir <- state_fun(normalizePath("."))
        state_file <- paste0(state_dir, "/", name)
        dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
        file.create(state_file)

        lapply(actions, eval.parent)
      }
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
      unlist(sapply(prereqs, function(name){
        id <- task_find_id(name, exists = TRUE)
        task_env$tasklist[[id]]$jobid
      }))
    } else {
      c()
    }
  },
  needed = function(){
    TRUE
  },
  timestamp = function(){
    Sys.time()
  }
  )
)

RTask <- setRefClass(
  "RTask",
  contains = "Task",
  methods = list(
    needed = function(){
    state_fun <- task_env$config$state
    state_dir <- state_fun(normalizePath("."))
    state_file <- paste0(state_dir, "/", name)
    never_run <- !file.exists(state_file)

    command <- paste0("git log -1 --format=%cD ", name, ".R")
    r_file_date <- system(command, intern = TRUE)
    last_edited_date <- strptime(r_file_date, format = "%a,  %d %b %Y %T %z")

    edited_since_last_run <- FALSE
    if (!never_run){
      last_run_date <- file.info(state_file)$mtime
      edited_since_last_run <- last_edited_date > last_run_date
    }
    prerequisite_run_more_recently <- FALSE
    most_recent_prereq <- FALSE
    if (length(prereqs) > 0){
      prereqs_timestamp <- lapply(prereqs, function(name){
        id <- task_find_id(name, exists = TRUE)
        task_env$tasklist[[id]]$timestamp()
      })
      most_recent_prereq <- do.call("max", prereqs_timestamp)
    }
    prerequisite_run_more_recently <- most_recent_prereq > timestamp()

    # flesh this out
    build_all <- FALSE

    never_run ||
      edited_since_last_run ||
      prerequisite_run_more_recently ||
      build_all
  },
  timestamp = function(){
    command <- paste0("git log -1 --format=%cD ", name, ".R")
    r_file_date <- system(command, intern = TRUE)
    r_file_date <- strptime(r_file_date, format = "%a,  %d %b %Y %T %z")

    state_fun <- task_env$config$state
    state_dir <- state_fun(normalizePath("."))
    state_file <- paste0(state_dir, "/", name)

    timestamp <- r_file_date
    if (file.exists(state_file)){
      state_file_date <- file.info(state_file)$mtime
      if (state_file_date > r_file_date){
        timestamp <- state_file_date
      }
    }
    timestamp
  }
  )
)
