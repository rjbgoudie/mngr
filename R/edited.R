task_last_edited_load_all <- function(){
  files_command <- "git ls-files"
  filenames <- system(files_command, intern = TRUE)
  last_edited <- tibble(filename = filenames)

  git_last_edited <- function(filename){
    command <- paste("git --no-pager log -1 --format=\"%ci\"", filename)
    system(command, intern = TRUE)
  }

  last_edited <- last_edited %>%
    rowwise %>%
    mutate(last_edited = git_last_edited(filename))
  last_edited$last_edited <- as.POSIXct(strptime(last_edited$last_edited,
                                                 format = "%Y-%m-%d %H:%M:%S %z"))
  state_env$last_edited <- last_edited
}

task_last_edited <- function(task_name){
  if (task_name != ""){
    state_env$last_edited %>%
      filter(filename == task_name) %>%
      pull(last_edited)
  } else {
    MNGR_UNIX_EPOCH
  }
}

task_last_edited_all <- function(task_names){
  do.call(c, lapply(task_names, task_last_edited))
}
