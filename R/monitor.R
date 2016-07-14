#' Monitor Slurm jobs
#'
#' This function is designed to work from the command line. It accepts two
#' arguments. The first is a comma-separated list of Slurm Job IDs. The second
#' is a path to R log directory.
#'
#' @export
monitor <- function(){
  set_terminal_width()

  suppressWarnings({
    suppressMessages({
      args <- commandArgs(TRUE)
      no_jobs <- args[[1]] == "No"
      args_split <- strsplit(args[[1]], " ")[[1]]

      jids <- strsplit(args_split[1], ",")[[1]]
      logs_dir <- args_split[2]
      logs_paths <- Sys.glob(paste0(logs_dir, "/", jids, "*.Rout"))

      squeue_status <- squeue(jobs = jids)
      rout_df <- parse_rout_files(logs_paths)
    })
  })

  have_squeue_status <-
    !is.null(squeue_status) &&
     is.data.frame(squeue_status) &&
     nrow(squeue_status) > 0
  have_rout <- !is.null(rout_df) && nrow(rout_df) > 0

  if (!no_jobs){
    if (have_squeue_status){
      cat("Queue status:\n")
      print(squeue_status)
      cat("\n")
    }

    if (have_rout){
      cat("Rout status:\n")
      cat_df(rout_df)
    }
  } else {
    message("No jobs submitted")
  }
}

#' Wrapper around Slurm's squeue function
#'
#' @param jobs Vector of Slurm Job IDs
#' @param format A format string for Slurm's squeue function
squeue <- function(jobs,
                   format =
                     "%.8i %.15P %.30j %.7u %.2t %.10M %.6D %.20R %.10L %.10p"){
  jobs <- paste0(jobs, collapse = ",")
  format <- paste0("--format=\"", format, "\"")
  squeue_output <- system(paste("squeue", format, " --jobs", jobs),
                          intern = TRUE,
                          ignore.stderr = TRUE)
  if (length(attributes(squeue_output)$status) == 0){
    read.table(text = squeue_output, header = TRUE)
  } else {
    "Job does not exist"
  }
}

#' @export
latest_logs <- function(){
  with_dir(run_dir(check = TRUE), {
    mngrfile <- find_mngrfile(getwd())
    source(mngrfile)
    r_log_fun <- task_env$config$r_logs
    r_log_dir <- r_log_fun(normalizePath("."))
    r_log_latest_dir <- paste0(r_log_dir, "-latest/")

    r_log_latest_file <- "*.Rout"
    logs_paths <- Sys.glob(file.path(r_log_latest_dir, r_log_latest_file))
    rout_df <- parse_rout_files(logs_paths)
    cat_df(rout_df)
  })
}
