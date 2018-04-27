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
  pause_loading <- task_env$config$pause_loading %||% FALSE
  if (!pause_loading){
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
    split = "character",
    properties = "list",
    filename_function = "list",
    arms_cache = "list",
    arms_cached = "logical",
    job_ids_cache = "list",
    job_ids_cached = "logical"
  ),
  methods = list(
    initialize = function(...){
      already_invoked <<- FALSE
      filename_function <<- list(function(name){""})
      actions <<- list()
      prereqs <<- character(0)
      arms_cached <<- FALSE
      job_ids_cached <<- FALSE
      initFields(...)
    },

    is_dummy = function(){
      "Returns true if the Task has no actions"
      length(actions) == 0
    },

    enhance = function(prereqs_new = NULL, actions_new = NULL){
      "Add a new action or prerequisite to the Task"
      if (!is.null(prereqs_new)){
        prereqs <<- c(prereqs_new, prereqs)
      }
      if (!is.null(actions_new)){
        actions <<- c(list(actions_new), actions)
      }
    },

    arms_list_with_default = function(){
      "Returns the arms list, with MNGR_DEFAULT_ARM added.
       This dummy is a placeholder to make many calculations easier, and to
       allow there to be no arms at all"
      c(list(MNGR_DEFAULT_ARM = 0), task_env$arms_list)
    },

    arms_to_invoke = function(){
      "Returns tibble with arms that should be invoked as rows. Each arm
       variable is a column, and the corresponding values are recorded. Columns
       (arms) are sorted alphabetically"

      if (arms_cached){
        arms_cache[[1]]
      } else {
        arms_list <- arms_list_with_default()

        # merged arms are never invoked
        arm_names_to_invoke <- setdiff(names(arms_list), merge)
        arms <- do.call("expand_grid", arms_list[arm_names_to_invoke])

        # Each invoked arm has ALL values of ALL merged arms
        if (length(merge) > 0){
          arms_merge <- arms_list[names(arms_list) %in% merge]

          # list with component for each merge arm
          # Each component contains all values of that arm repeated for each for
          # of arms
          arms_merge_list <- purrr::map(arms_merge, ~rep(list(.), nrow(arms)))

          arms <- bind_cols(arms, list(arms_merge_list))
        }

        # sort arms
        o <- order(colnames(arms))
        arms_cached <<- TRUE
        (arms_cache[[1]] <<- arms[, o])
      }
    },

    arms_when_splitting = function(){
      "When loading data from a merged task, we must merge all arm names that
       are being split over. i.e. we replace the split arm values with ALL arm
       values for that arm"

      arms_local <- arms_to_invoke()
      for (s in split){
        arms_local[, s] <- list(arms_list_with_default()[s])
      }
      arms_local
    },

    arm_ids = function(splitting = FALSE){
      "Return a character vector of arm id strings. These take the form
       ARM1NAME--ARM1VALUE__ARM2NAME--ARM2VALUE etc, although if a
       armvalue is long, it is converted to a hash"

      if (splitting){
        arms <- arms_when_splitting()
      } else {
        arms <- arms_to_invoke()
      }

      # Build strings of the form:
      # ARM1NAME--ARM1VALUE__ARM2NAME--ARM2VALUE etc
      arms %>%
        select(-MNGR_DEFAULT_ARM) %>%
        rowwise %>%
        summarize_all(paste_or_hash) %>%
        rowwise %>%
        do(tibble(arm_id_string =
                    paste(names(.), ., sep = "--", collapse = "__"))) %>%
        pull(arm_id_string)
    },

    job_ids = function(){
      "Return a character vector of job id strings. These take the form
       TASKNAME__ARM1NAME--ARM1VALUE__ARM2NAME--ARM2VALUE etc, although if a
       armvalue is long, it is converted to a hash"
      if (job_ids_cached){
        job_ids_cache[[1]]
      } else {
        job_ids_cached <<- TRUE
        (job_ids_cache[[1]] <<- paste(name,
                                      arm_ids(splitting = FALSE),
                                      sep = "__"))
      }
    },

    throttle_job_ids = function(){
      "Get the job_id of the (throttle)th previous arm value, so that it can be
       added as a prerequisite to allow for throttling"
      throttle <- get_throttle()
      arms_local <- arms_to_invoke()
      arm_seq <- seq_len(nrow(arms_local))

      indicies <- ((arm_seq %/% throttle) - 1) * throttle + arm_seq %%
        throttle
      indicies <- ifelse(indicies >= 1, indicies, NA)
      job_ids()[indicies]
    },

    which_jobs_involve = function(match){
      "For each row of a given data_frame arm values (match), identify which
       arms in THIS task involve the arm values in that match row. "

      this <- arms_to_invoke()
      common_cols <- intersect(colnames(this), colnames(match))
      this <- this[, common_cols]
      result <- array(dim = c(nrow(match), nrow(this), ncol(this)))

      for (i in 1:nrow(match)){
        for (r in 1:nrow(this)){
          for (c in 1:ncol(this)){
            mic <- unlist(match[i, c])
            trc <- unlist(this[r, c])
            result[i, r, c] <- length(intersect(mic, trc)) > 0
          }
        }
      }

      # all arms (common_cols) must have at least something in common
      involved <- rowSums(result, dims = 2) == length(common_cols)

      # For each row of the match data_frame, which rows in this are involved
      indicies_list <- apply(involved, 1, which)

      # Convert these indicies to job_ids
      lapply(indicies_list, function(index){
        as.list(job_ids()[index])
      })
    },

    build_jobs = function(){
      arms_local <- arms_to_invoke()
      jobs_df <- bind_cols(arms_local,
                           tibble(prereq_job_ids = prereq_job_ids()))

      jobs_df <- jobs_df %>%
        mutate(task_name = name,
               task_filename = get_filename(),
               last_edited = task_last_edited_all(task_filename),
               arm_index = row_number(),
               job_ids = job_ids(),
               ever_invoked = state_ever_invoked(job_ids),
               last_invoked = state_last_invoked_all(job_ids))

      jobs_df %>%
        mutate(edited_since_last_invoked = last_edited > last_invoked,
               most_recently_invoke_prereq =
                 do.call(c, purrr::map(prereq_job_ids, ~max(state_last_invoked_all(.)))),
               prereq_invoked_after_last_invoked = most_recently_invoke_prereq > last_invoked,
               needed = case_when(!ever_invoked ~ TRUE,
                                  edited_since_last_invoked ~ TRUE,
                                  prereq_invoked_after_last_invoked ~ TRUE,
                                  TRUE ~ FALSE))
    },

    invoke = function(debug = FALSE){
      "Invoke this task and its prerequisities, unless it has already been
       invoked"
      if (!already_invoked){
        already_invoked <<- TRUE

        debug_msg(debug, "Invoking prerequisties for ", name)
        invoke_prereqs(debug = debug)

        jobs_needed <- build_jobs() %>%
          filter(needed)

        jobs_needed %>%
          rowwise %>%
          do(null = state_update_last_invoked_time(.$job_ids),
             null = job_create(name = .$job_ids,
                               basename = .$task_name,
                               arm_index = .$arm_index,
                               actions = actions,
                               prereqs = as.character(unlist(.$prereq_job_ids)),
                               properties = properties))

        message(nrow(jobs_needed), " arms of ", name, " needed")
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

    prereq_job_ids = function(){
      "Return a list, each component corresponds to an arm of this task"

      arms_local <- arms_to_invoke()
      arm_seq <- seq_len(nrow(arms_local))

      prerequisities <- prereq_tasks()
      names(prerequisities) <- rep("prereq_taskarms",
                                   times = length(prerequisities))

      if (length(prerequisities) == 0){
        result <- purrr::map(arm_seq, function(i) list())
      } else {
        # A list each component of which corresponds to a task.
        # Each component contains a component corresponding to each arm of this
        # task
        prereq_jobids_by_prereq <- lapply(prerequisities, function(task){
          if (task$is_dummy()){
            task$prereq_job_ids()
          } else {
            task$which_jobs_involve(arms_local)
          }
        })
        # Flip so that prerequisities tasks are nested within arms
        prereq_jobids_by_arm <- purrr::transpose(prereq_jobids_by_prereq)

        # Then for each arm, unlist to remove task level
        result <- lapply(prereq_jobids_by_arm, unlist, recursive = FALSE)
      }

      # Add (throttle)th previous arm, so as to throttle
      purrr::map2(result, throttle_job_ids(), append_unless_na)
    },

    add_merge = function(new_merge){
      merge <<- c(merge, new_merge)
    },

    add_split = function(new_split){
      split <<- c(split, new_split)
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

    set_filename_function = function(f){
      filename_function[[1]] <<- f
    },

    get_filename = function(){
      filename_function[[1]](name)
    }
  )
)

# concatenate arm values (separated by "-"), unless the value is
# very long, in which case we hash the concatenated string. This is
# required because filesystems do not allow for very long file or
# directory names
paste_or_hash <- function(...){
  p <- paste(unlist(...), collapse = "-")
  is_long <- nchar(p) > 10
  p[is_long] <- sapply(p[is_long], digest::digest, algo = "xxhash32")
  unlist(p)
}
