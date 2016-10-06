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
    if (is_dummy()){
      name
    } else {
      arm <- arms(include_shared = TRUE)[[arm_index]]
      o <- order(names(arm))
      arm <- arm[o]
      arm_values <- sapply(arm, paste, collapse = "-")
      arm_names <- names(arm)
      arm_str <- paste(arm_names, arm_values, sep = "--",  collapse = "__")
      paste(name, arm_str, sep = "__")
    }
  },
  enhance = function(prereqs_new = NULL, actions_new = NULL){
    if (!is.null(prereqs_new)){
      prereqs <<- c(prereqs_new, prereqs)
    }
    if (!is.null(actions_new)){
      actions <<- c(list(actions_new), actions)
    }
  },
  arms = function(include_shared = TRUE){
    arms_list <- task_env$arms_list
    share <- getShared()
    which_arms_shared <- names(arms_list) %in% share
    arms_unshared <- do.call("expand.grid", arms_list[!which_arms_shared])
    arms_unshared <- lapply(seq_len(nrow(arms_unshared)), function(i){
      as.list(arms_unshared[i,, drop = FALSE])
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
  },
  arm = function(arm_index, include_shared = TRUE){
    arms(include_shared = include_shared)[[arm_index]]
  },
  which_arms = function(to_match){
    arm_indicators <- sapply(arms(), function(arm){
      comparable_arm_names <- names(arm)[names(arm) %in% names(to_match)]
      are_equal <- sapply(comparable_arm_names, function(x){
        length(intersect(arm[[x]], to_match[[x]])) > 0
      })
      all(are_equal)
    })
    which(arm_indicators)
  },
  prereq_taskarm_names = function(arm_index,
                                  arm_values = arm(arm_index = arm_index,
                                                   include_shared = TRUE),
                                  debug = FALSE){
    tasks <- prereq_tasks()
    out <- lapply(tasks, function(task){
      if (task$is_dummy()){
        task$prereq_taskarm_names(arm_index, arm_values = arm_values)
      } else {
        prereq_arms <- task$which_arms(arm_values)
        lapply(prereq_arms, function(arm_index){
          task$taskarm_name(arm_index)
        })
      }
    })
    out <- as.character(unlist(out))
    debug_msg(debug,
              "Prereqs_taskarms for ", name,
              " arm_index ", arm_index,
              " are: ", paste(out, collapse = ","))
    out
  },
  invoke = function(debug = FALSE) {
    if (!already_invoked){
      already_invoked <<- TRUE

      debug_msg(debug, "Invoking prerequisties for ", name)
      invoke_prereqs(debug = debug)

      arms_count <- 1
      if (length(actions) > 0){
        arms <- arms(include_shared = FALSE)
        arms_count <- length(arms)
      }

      debug_msg(debug, "Building ", arms_count, " arms for ", name)
      for (arm_index in seq_len(arms_count)){
        this_taskarm_name <- taskarm_name(arm_index)
        taskarm_create(task_name = name,
                       taskarm_name = this_taskarm_name,
                       arm_index = arm_index,
                       prereqs = prereq_taskarm_names(arm_index, debug = debug),
                       actions = actions,
                       properties = properties,
                       custom_timestamp = custom_timestamp)
        taskarm_get(this_taskarm_name, exists = TRUE)$invoke(debug = debug)
      }
    }
  },
  prereq_tasks = function(){
    prereq_tasks <- sapply(prereqs, task_get)
    Filter(not.null, prereq_tasks)
  },
  invoke_prereqs = function(debug = FALSE){
    debug_msg(debug, "Running prereqs for ", name)
    tasks <- prereq_tasks()
    lapply(tasks, function(task) task$invoke(debug = debug))
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
