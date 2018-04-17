task_last_edited_load_all <- function(){
  command <- paste("git ls-files -z",
                   "|",
                   "xargs -0 -n1 -I{} --",
                   "git --no-pager log -1 --format=\"%cD\t{}\" {}")
  last_edited <- system(command, intern = TRUE)
  last_edited <- paste(last_edited, collapse = "\n")
  last_edited <- read.table(text = last_edited,
                            sep = "\t",
                            col.names = c("last_edited", "filename")) %>%
    as_tibble
  last_edited$last_edited <- as.POSIXct(strptime(last_edited$last_edited,
                                                 format = "%a,  %d %b %Y %T %z"))
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
