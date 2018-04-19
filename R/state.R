state_load_all <- function(){
  state_dir <- mngr_option_dir_state()(fs::path_tidy(getwd()))
  fs::dir_create(state_dir)

  info <- fs::dir_info(state_dir)
  info$file <- as.character(fs::path_file(info$path))

  state_env$info <- info[, c("file", "modification_time")]
  state_env$state_dir <- state_dir
}

state_ever_invoked <- function(taskarm_name){
  taskarm_name %in% state_env$info$file
}

state_last_invoked <- function(taskarm_name){
  if (state_ever_invoked(taskarm_name)){
    state_env$info %>%
      filter(file == taskarm_name) %>%
      pull(modification_time)
  } else{
    MNGR_UNIX_EPOCH
  }
}

state_last_invoked_all <- function(taskarm_names){
  if (length(taskarm_names) == 0){
    MNGR_UNIX_EPOCH
  } else {
    do.call(c, lapply(taskarm_names, state_last_invoked))
  }
}

state_update_last_invoked_time <- function(taskarm_name){
  state_file <- fs::path(state_env$state_dir, taskarm_name)
  fs::file_create(state_file)
  Sys.setFileTime(unclass(state_file), Sys.time())

  if (state_ever_invoked(taskarm_name)){
    state_env$info[state_env$info$file == taskarm_name, "modification_time"] <-
      Sys.time()
  } else {
    new_row <- tibble::data_frame(file = taskarm_name,
                                  modification_time = Sys.time())
    state_env$info <- dplyr::bind_rows(state_env$info, new_row)
  }
}
