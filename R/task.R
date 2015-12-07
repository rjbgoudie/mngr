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
  task_env$tasklist[[id]]$add_action(action)
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
  task_env$tasklist[[id_a]]$add_prereqs(b)
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
    jobid = "character",
    prereq_jobids = "character",
    r_log_path = "character",
    shared = "list",
    memory = "numeric",
    cores = "numeric"
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
  set_jobid = function(x){
    jobid <<- x
  },
  invoke = function(debug = FALSE) {
    if (!isTRUE(already_invoked)){
      already_invoked <<- TRUE
      if (debug){
        message("Invoking prerequisties for ", name)
      }
      invoke_prereqs(debug = debug)
      if (isTRUE(.self$needed(debug = debug))){

        if (debug){
          message("Invoking ", name)
        }

        state_file(ensure_dir = TRUE, create = TRUE)
        action_class <- sapply(actions, class)
        lapply(actions[action_class == "{"], eval.parent)
        lapply(actions[action_class == "call"], function(action){
          eval(action)(.self)
        })
        lapply(actions[action_class == "function"], function(action){
          action(.self)
        })
      } else {
        if (debug){
          message(name, " not needed")
        }
      }
    }
    unique(c(prereq_jobids, jobid))
  },
  invoke_prereqs = function(debug = FALSE){
    if (length(prereqs) > 0){
      if (debug){
        message("Running prereqs for ", name)
      }
      prereq_jobids <<- unlist(lapply(prereqs, function(name){
        id <- task_find_id(name, exists = TRUE)
        task_env$tasklist[[id]]$invoke(debug = debug)
      }))
    }
  },
  jobid_prereqs = function(index){
    if (length(prereqs) > 0){
      unlist(sapply(prereqs, function(name){
        id <- task_find_id(name, exists = TRUE)
        parent <- task_env$tasklist[[id]]$jobid

        if (length(parent) == 0){
          parent <- task_env$tasklist[[id]]$jobid_prereqs(index)
        }
        if (length(parent) > 1){
          parent <- parent[index]
        }
        parent
      }))
    } else {
      c()
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

    if (debug){
      message("Needed status for ", name, " is ", out, ". ",
              "never_run: ", never_run, ". ",
              "prerequistite_run_more_recently: ", prerequisite_run_more_recently,
              ". ",
              "build_all: ", build_all, ".")
    }
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

    if (isTRUE(ensure_dir)){
      ensure_exists(state_dir)
    }
    state_file <- paste0(state_dir, "/", name)

    if (isTRUE(create)){
      file.create(state_file)
    } else {
      state_file
    }
  },
  slurm_file = function(ensure_dir = TRUE){
    slurm_log_fun <- task_env$config$slurm_logs
    slurm_log_dir <- slurm_log_fun(normalizePath("."))

    if (isTRUE(ensure_dir)){
      ensure_exists(slurm_log_dir)
    }

    slurm_log_file <- paste0("%A.%a-", name, ".txt")
    file.path(slurm_log_dir, slurm_log_file)
  },
  add_shared = function(new_shared){
    shared <<- c(shared, new_shared)
  },
  getShared = function(){
    shared
  },
  any_shared = function(){
    length(shared) > 0
  },
  set_memory = function(new_memory){
    memory <<- new_memory
  },
  get_memory = function(){
    if (length(memory) > 0){
      memory
    } else {
      3993
    }
  },
  set_cores = function(new_cores){
    cores <<- new_cores
  },
  get_cores = function(){
    if (length(cores) > 0){
      cores
    } else {
      1
    }
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

    if (debug){
      message("Needed status for ", name, " is ", out, ". ",
              "never_run: ", never_run, ". ",
              "edited_since_last_run: ", edited_since_last_run, ". ",
              "prerequistite_run_more_recently: ", prerequisite_run_more_recently,
              ". ",
              "build_all: ", build_all, ".")
    }
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
  },
  r_log_latest_file = function(ensure_dir = TRUE, index){
    r_log_fun <- task_env$config$r_logs
    r_log_dir <- r_log_fun(normalizePath("."))
    r_log_latest_dir <- paste0(r_log_dir, "-latest/")

    if (isTRUE(ensure_dir)){
      ensure_exists(r_log_latest_dir)
    }
    r_log_latest_file <- paste0(name, "_", index, ".Rout")
    file.path(r_log_latest_dir, r_log_latest_file)
  },
  r_log_specific_file = function(ensure_dir = TRUE, index){
    r_log_fun <- task_env$config$r_logs
    r_log_dir <- r_log_fun(normalizePath("."))

    if (isTRUE(ensure_dir)){
      ensure_exists(r_log_dir)
    }
    r_log_latest_file <- paste0(name, "_", index, ".Rout")
    r_log_specific_file <- paste0("\\${SLURM_JOB_ID}_", r_log_latest_file)
    file.path(r_log_dir, r_log_specific_file)
  }
  )
)
