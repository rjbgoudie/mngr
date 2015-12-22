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
    message("Task ", name, " does not exist")
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
  task_env$tasklist[[id]]$enhance(actions_new = action)
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
  id_a <- task_find_id(a, exists = TRUE)
  task_env$tasklist[[id_a]]$enhance(prereqs_new = b)
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
    properties = "list"
  ),
  methods = list(
    initialize = function(...){
    already_invoked <<- FALSE
    initFields(...)
  },
  enhance = function(prereqs_new = NULL, actions_new = NULL){
    prereqs <<- c(prereqs_new, prereqs)
    actions <<- c(list(actions_new), actions)
  },
  invoke = function(debug = FALSE) {
    if (!already_invoked){
      already_invoked <<- TRUE
      debug_msg(debug, "Invoking prerequisties for ", name)

      invoke_prereqs(debug = debug)
      if (.self$needed(debug = debug)){

        debug_msg(debug, "Invoking ", name)

        state_file(ensure_dir = TRUE, create = TRUE)
        job_create(name = name,
                   actions = actions,
                   prereqs = prereqs,
                   shared = shared,
                   properties = properties)
      } else {
        debug_msg(debug, name, " not needed")
      }
    }
  },
  invoke_prereqs = function(debug = FALSE){
    if (length(prereqs) > 0){
      debug_msg(debug, "Running prereqs for ", name)

      sapply(prereqs, function(name){
        id <- task_find_id(name, exists = TRUE)
        task_env$tasklist[[id]]$invoke(debug = debug)
      })
    }
  },
  needed = function(debug = FALSE){
    state_file <- state_file(ensure_dir = FALSE, create = FALSE)
    never_run <- !file.exists(state_file)

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

    out <- never_run ||
      prerequisite_run_more_recently ||
      build_all

    debug_msg(debug,
              "Needed status for ", name, " is ", out, ". ",
              "never_run: ", never_run, ". ",
              "prerequistite_run_more_recently: ", prerequisite_run_more_recently,
              ". ",
              "build_all: ", build_all, ".")
    out
  },
  timestamp = function(){
    state_file <- state_file(ensure_dir = FALSE, create = FALSE)

    if (file.exists(state_file)){
      file.info(state_file)$mtime
    } else {
      Sys.time()
    }
  },
  state_file = function(ensure_dir = TRUE, create = FALSE){
    state_fun <- task_env$config$state
    state_dir <- state_fun(normalizePath("."))

    if (ensure_dir){
      ensure_exists(state_dir)
    }
    state_file <- paste0(state_dir, "/", name)

    if (create){
      file.create(state_file)
    } else {
      state_file
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
  }
  )
)

RTask <- setRefClass(
  "RTask",
  contains = "Task",
  methods = list(
    needed = function(debug = FALSE){
    state_file <- state_file(ensure_dir = FALSE, create = FALSE)
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
    out <- never_run ||
      edited_since_last_run ||
      prerequisite_run_more_recently ||
      build_all

    debug_msg(debug,
              "Needed status for ", name, " is ", out, ". ",
              "never_run: ", never_run, ". ",
              "edited_since_last_run: ", edited_since_last_run, ". ",
              "prerequistite_run_more_recently: ", prerequisite_run_more_recently,
              ". ",
              "build_all: ", build_all, ".")
    out
  },
  timestamp = function(){
    command <- paste0("git log -1 --format=%cD ", name, ".R")
    r_file_date <- system(command, intern = TRUE)
    r_file_date <- strptime(r_file_date, format = "%a,  %d %b %Y %T %z")

    state_file <- state_file(ensure_dir = FALSE, create = FALSE)

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
