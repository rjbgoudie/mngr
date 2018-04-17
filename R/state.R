state_load_all <- function(){
  state_dir <- mngr_option_dir_state()(fs::path_tidy(getwd()))
  fs::dir_create(state_dir)

  info <- fs::dir_info(state_dir)
  info$file <- path_file(info$path)

  state_env$info <- info[, c("file", "modification_time")]
  state_env$state_dir <- state_dir
}

state_ever_invoked <- function(taskarm_name){
  taskarm_name %in% state_env$info$file
}

state_last_invoked <- function(taskarm_name){
  if (state_ever_invoked(taskarm_name)){
    state_env$info %>%
      filter(state_env$info$file == taskarm_name) %>%
      pull(modification_time)
  } else{
    MNGR_UNIX_EPOCH
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
    state_env$info <- rbind(state_env$info,
                            list(file = taskarm_name,
                                 modification_time = Sys.time()))
  }
}
