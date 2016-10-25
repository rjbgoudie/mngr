#' Monitor Slurm jobs
#'
#' This function is designed to work from the command line. It accepts two
#' arguments. The first is a comma-separated list of Slurm Job IDs. The second
#' is a path to R log directory.
#'
#' @export
monitor <- function(jobs = NULL, logs = NULL){
  set_terminal_width()

  suppressWarnings({
    suppressMessages({
      if (is.null(jobs)){
        args <- commandArgs(TRUE)
        no_jobs <- args[[1]] == "No"
        args_split <- strsplit(args[[1]], " ")[[1]]

        jids <- strsplit(args_split[1], ",")[[1]]
        logs_dir <- args_split[2]
      } else {
        jids <- jobs
        logs_dir <- logs
        no_jobs <- length(jids) == 0
      }
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

  if (!have_rout & !have_squeue_status){
    message("No jobs submitted")
  } else if (!have_rout & have_squeue_status){
    pretty_print_squeue(squeue_status)
  } else if (have_rout & !have_squeue_status){
    pretty_print_rout(rout_df)
  } else {
    merged <- merge_tables(squeue_status, rout_df)
    pretty_print_merged(merged, squeue_status)
  }
}

#' Wrapper around Slurm's squeue function
#'
#' @param jobs Vector of Slurm Job IDs. NULL returns all jobs
#' @param user Vector of usernames. NULL returns all jobs
#' @return
#' A data.frame with the following columns:
#'
#' jid, partition, jobname, shortsha, user, slurm_status, t_used, nodes,
#' nodelist_reason, t_left, priority
#'
#' plus
#'
#' arm___xyz columns for each arm value
squeue <- function(jobs = NULL,
                   user = NULL){
  format <- "%.8i\t%.50P\t%.100j\t%.7u\t%.2t\t%.10M\t%.6D\t%.20R\t%.10L\t%.10p"
  if (!is.null(jobs)){
    jobs <- paste0(" --jobs=", paste0(jobs, collapse = ","))
  } else {
    jobs <- ""
  }

  if (!is.null(user)){
    user <- paste0(" --user=", paste0(user, collapse = ","))
  } else {
    user <- NULL
  }

  format <- paste0("--format=\"", format, "\"")

  squeue_output <- system(paste("squeue", format, jobs, user),
                          intern = TRUE,
                          ignore.stderr = TRUE)
  if (length(attributes(squeue_output)$status) == 0){
    x <- read.table(text = squeue_output,
                    header = TRUE,
                    sep = "\t",
                    strip.white = TRUE)
    if (nrow(x) > 0){
      colnames(x) <- tolower(colnames(x))
      x <- x %>%
        rename(jid = jobid,
               t_used = time,
               t_left = time_left,
               slurm_status = st,
               nodelist_reason = nodelist.reason.) %>%
        do(parse_name(., type = "squeue"))
    }
    x
  } else {
    message("Job does not exist")
    NULL
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

get_jobids <- function(pattern = NULL){
  queue <- squeue()
  rows <- grepl(pattern, x = queue$NAME)
  queue[rows, "JOBID"]
}

qdel <- function(jobids){
  command <- paste("scancel", paste(jobids, collapse = " "))
  system(command)
}

kill_pattern <- function(shortsha = NULL, user = NULL){
  sq <- squeue(user = user)
  jobids <- sq[sq$shortsha == shortsha, "jid"]
  qdel(jobids)
}

sinfo <- function(partition = "mrc-bsu-sand,mrc-bsu-tesla"){
  format <- "%P\t%F"
  format <- paste0("--format=\"", format, "\"")

  if (!is.null(partition)){
    partition <- paste0("--partition=\"", partition, "\"")
  } else {
    partition <- ""
  }

  sinfo_output <- system(paste("sinfo", format, partition),
                         intern = TRUE,
                         ignore.stderr = TRUE)
  if (length(attributes(sinfo_output)$status) == 0){
    x <- read.table(text = sinfo_output,
                    header = TRUE,
                    sep = "\t",
                    strip.white = TRUE)
    if (nrow(x) > 0){
      colnames(x) <- tolower(colnames(x))
      x <- x %>%
        rename(status = `nodes.a.i.o.t.`) %>%
        separate(status, c("allocated", "idle", "other", "total"))
    }
    x
  } else {
    message("Job does not exist")
    NULL
  }
}

slurm_summary <- function(squeue_status){
  # summary
  sinfo <- sinfo(partition = "mrc-bsu-sand,mrc-bsu-tesla")
  paste0("\033[36mSHA:\033[39m ", unique(squeue_status$shortsha),
         " \033[36mPartition:\033[39m ", unique(squeue_status$partition),
         " \033[36msand idle:\033[39m ", sinfo[1, "idle"], "/", sinfo[1, "total"],
         " \033[36mtesla idle:\033[39m ", sinfo[2, "idle"], "/", sinfo[2, "total"])
}
