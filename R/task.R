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
    merge = "list",
    split = "list",
    properties = "list",
    custom_timestamp = "list",
    arms_cache = "list",
    arms_cached = "logical"
  ),
  methods = list(
    initialize = function(...){
      already_invoked <<- FALSE
      custom_timestamp <<- list()
      actions <<- list()
      prereqs <<- character(0)
      arms_cached <<- c(FALSE, FALSE)
      initFields(...)
    },
    is_dummy = function(){
      length(actions) == 0
    },
    taskarm_name = function(arm_index){
      if (is_dummy()){
        name
      } else {
        arm <- arms()[[arm_index]]
        o <- order(names(arm))
        arm <- arm[o]

        arm_values <- sapply(arm, paste, collapse = "-")
        arm_values_long <- nchar(arm_values) > 10
        arm_values[arm_values_long] <- sapply(arm_values[arm_values_long],
                                              digest, algo = "xxhash32")
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
    arms = function(expand_split = TRUE){
      if (!arms_cached[expand_split + 1]){
        arms_list <- task_env$arms_list

        merge <- getMerge()
        split <- getSplit()

        # 1. merged arms are never expanded via expand.grid
        # 2. if expand_split = TRUE, then split arms are treated as standard arms
        # 3. if expand_split = FALSE, then split arms are repeated nrow(expand.grid)
        #    times

        if (expand_split){
          dont_expand <- merge
        } else {
          dont_expand <- c(merge, split)
        }

        which_arms_expand <- !(names(arms_list) %in% dont_expand)
        which_arms_merge <- names(arms_list) %in% merge

        arms_base <- do.call("expand.grid", arms_list[which_arms_expand])
        out <- lapply(seq_len(nrow(arms_base)), function(i){
          as.list(arms_base[i,, drop = FALSE])
        })

        if (!expand_split){
          which_arms_split <- names(arms_list) %in% split

          if (sum(which_arms_split) > 0){
            arms_split <- arms_list[which_arms_split]
            arms_split_expanded <- do.call("expand.grid", arms_split)
            arms_split_expanded_count <- nrow(arms_split_expanded)

            if (length(out) > 0){
              out <- lapply(seq_along(out), function(i){
                lapply(seq_len(arms_split_expanded_count), function(j){
                  append(out[[i]], arms_split)
                })
              })
              out <- unlist(out, recursive = FALSE)
            } else {
              out <- lapply(seq_len(arms_split_expanded_count), function(i){
                arms_split
              })
            }
          }
        }

        if (sum(which_arms_merge) > 0){
          arms_merge <- arms_list[which_arms_merge]

          if (length(out) > 0){
            values <- lapply(seq_along(out), function(x) arms_merge)
            out <- mapply(append, out, values, SIMPLIFY = FALSE)
          } else {
            out <- list(arms_merge)
          }
        }
        arms_cached[expand_split + 1] <<- TRUE
        arms_cache[[expand_split + 1]] <<- out
        out
      } else {
        arms_cache[[expand_split + 1]]
      }
    },
    arm = function(arm_index){
      arms()[[arm_index]]
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
                                    arm_values = arm(arm_index = arm_index),
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
          arms <- arms()
          arms_count <- length(arms)
        }

        throttle <- get_throttle()

        debug_msg(debug, "Building ", arms_count, " arms for ", name)
        for (arm_index in seq_len(arms_count)){

          arm_prereqs <- prereq_taskarm_names(arm_index, debug = debug)

          throttle_arm_dependency <- throttle_arm_dependency(arm_index, throttle)
          if (!is.null(throttle_arm_dependency)){
            arm_prereqs <- c(arm_prereqs, taskarm_name(throttle_arm_dependency))
          }

          this_taskarm_name <- taskarm_name(arm_index)
          taskarm_create(task_name = name,
                         taskarm_name = this_taskarm_name,
                         arm_index = arm_index,
                         prereqs = arm_prereqs,
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
    add_merge = function(new_merge){
      merge <<- c(merge, new_merge)
    },
    getMerge = function(){
      merge
    },
    add_split = function(new_split){
      split <<- c(split, new_split)
    },
    getSplit = function(){
      split
    },
    get_throttle = function(){
      if (length(properties$throttle) > 0){
        properties$throttle
      } else {
        task_env$config$throttle %||% mngr_default_throttle
      }
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

throttle_arm_dependency <- function(arm_index,  throttle){
  index <- ((arm_index %/% throttle) - 1) * throttle + arm_index %% throttle
  if (index >= 1){
    index
  } else {
    NULL
  }
}
