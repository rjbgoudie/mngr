all_jids <- function(){
  paste0(sapply(task_env$tasklist, "[[", "jobid"), collapse = ",")
}

my_jobs <- system(paste("squeue --jobs", all_jids()), intern = TRUE)
read.table(text = my_jobs, header = TRUE)
