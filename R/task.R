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

    which_arms_involve = function(match, id){
      "A list, each component of which corresponds to a row of a given
       data_frame of arm values (match). Each component contains the IDs
       (job_id or arm_id) of the arms in THIS task involve the arm values in
       that corresponding row of match."

      this <- arms_to_invoke()
      common_cols <- intersect(colnames(this), colnames(match))
      this <- this[, common_cols]
      result <- array(dim = c(nrow(match), nrow(this), ncol(this)))

      for (i in 1:nrow(match)){ # rows of match
        for (r in 1:nrow(this)){ # different arm values of this task
          for (c in 1:ncol(this)){ # different arm variables of this task
            mic <- unlist(match[i, c])
            trc <- unlist(this[r, c])
            result[i, r, c] <- length(intersect(mic, trc)) > 0
          }
        }
      }

      # Calculate the rowSum over dim 3 of result
      # So involved is a logical matrix, with rows corresponding to rows of
      # match and columns corresponding to arm values of THIS task.
      # involved[i, j] == TRUE iff arm j of THIS task involves the
      # arm values in match[i, ]
      involved <- rowSums(result, dims = 2) == length(common_cols)

      # A list, each component of which corresponds to a row of match
      # Each component is a vector of arm indicies of THIS task that involve
      # arm values in the row of match
      #
      # The following is like apply(involved, 1, which) but always returns a
      # list
      indicies_list <- tapply(involved, row(involved), which, simplify = FALSE)

      id_fun <- if (id == "job"){
        .self$job_ids
      } else if (id == "arm") {
        .self$arm_ids
      }

      # A list, each component of which corresponds to a row of match
      # Each component is a list of the id_fun of the arms of THIS task that
      # involve the arm values in that row of match
      lapply(indicies_list, function(index){
        as.list(id_fun()[index])
      })
    },

    build_jobs = function(debug = FALSE){
      arms_local <- arms_to_invoke()
      jobs_df <- bind_cols(arms_local,
                           tibble(prereq_job_ids = prereq_ids(id = "job")))

      jobs_df <- jobs_df %>%
        mutate(task_name = name,
               task_filename = get_filename(),
               last_edited = task_last_edited_all(task_filename),
               arm_index = row_number(),
               job_ids = job_ids(),
               ever_invoked = state_ever_invoked(job_ids),
               last_invoked = state_last_invoked_all(job_ids),
               prereq_job_ids_with_throttle = purrr::map2(prereq_job_ids,
                                                          throttle_job_ids(),
                                                          append_unless_na))

      jobs_df <- jobs_df %>%
        mutate(edited_since_last_invoked = last_edited > last_invoked,
               most_recently_invoke_prereq =
                 do.call(c, purrr::map(prereq_job_ids, ~max(state_last_invoked_all(.)))),
               prereq_invoked_after_last_invoked = most_recently_invoke_prereq > last_invoked,
               needed = case_when(!ever_invoked ~ TRUE,
                                  edited_since_last_invoked ~ TRUE,
                                  prereq_invoked_after_last_invoked ~ TRUE,
                                  TRUE ~ FALSE))

      if (debug){
        cat(write.table(jobs_df[, sapply(jobs_df, class) != "list"]),
            file = stderr())
      }
      jobs_df
    },

    invoke = function(debug = FALSE){
      "Invoke this task and its prerequisites, unless it has already been
       invoked"
      if (!already_invoked){
        already_invoked <<- TRUE

        debug_msg(debug, "Invoking prerequisties for ", name)
        invoke_prereqs(debug = debug)

        jobs_needed <- build_jobs(debug = debug) %>%
          filter(needed)

        if (!is_dummy() & nrow(jobs_needed) > 0){
          job_create(jobs_needed, actions, properties)

          message(nrow(jobs_needed), " arms of ", name, " needed")
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

    prereq_ids_by_prereq = function(id, arms_local){
      "Return a list, each component of which corresponds to a prereq of this
       of this task. Each of these components contains a component corresponding
       to each arm of THIS task, containing a list of IDs of arms of that prereq
       that involve the corresponding arm of THIS task"
      prerequisites <- prereq_tasks()
      names(prerequisites) <- rep("prereq_taskarms",
                                  times = length(prerequisites))

      if (length(prerequisites) == 0){
        arm_seq <- seq_len(nrow(arms_local))
        list(purrr::map(arm_seq, function(i) list()))
      } else {
        if (length(prerequisites) == 1 & prerequisites[[1]]$is_dummy()){
          # With just one dummy prerequisite, just jump up a level
          prerequisites[[1]]$prereq_ids_by_prereq(id = id,
                                                  arms_local = arms_local)
        } else {
          lapply(prerequisites, function(task){
            if (task$is_dummy()){
              warning("Depending on more than one dummy not handled yet")
            } else {
              task$which_arms_involve(arms_local, id = id)
            }
          })
        }
      }
    },

    prereq_ids = function(id){
      "Return a list, each component corresponds to an arm of THIS task.
       Each of component contains the arm IDs of the prereq that involve the
       corresponding arm of THIS task"
      arms_local <- arms_to_invoke()

      prereq_ids_by_prereq_local <-
        .self$prereq_ids_by_prereq(id = id, arms_local = arms_local)

      # purrr::transpose reverses the nesting of the OUTER two lists
      # so this results in a list, each component of which corresponds to an
      # arm of this task. Each of these components contains a component
      # corresponding to each prerequisite of this task.
      prereq_ids_by_arm <- purrr::transpose(prereq_ids_by_prereq_local)

      # Then for each arm, unlist to remove the level corresponding to each
      # prerequisite task
      lapply(prereq_ids_by_arm, unlist, recursive = FALSE)
    },

    add_merge = function(new_merge){
      merge <<- c(merge, new_merge)
    },

    add_split = function(new_split){
      split <<- c(split, new_split)
    },

    get_throttle = function(){
      local <- if (length(properties$throttle) > 0){
        properties$throttle
      } else {
        Inf
      }
      global <- mngr_option_global_throttle()
      min(local, global)
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
