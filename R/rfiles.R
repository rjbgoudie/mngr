#' @export
rfiles <- function(){
  rfiles <- list.files(path = ".",
                       pattern = "\\.R$")
  rfiles_sans_ext <- sapply(rfiles, file_path_sans_ext)
  sapply(rfiles_sans_ext, rfile)
  nfiles <- length(rfiles_sans_ext)
  message("Added ", nfiles, " R files")
}

#' @export
rfile <- function(...){
  task(..., {
    id <- task_find_id(name, exists = TRUE)
    jp <- task_env$tasklist[[id]]$jobid_prereqs()
    jp <- if (!is.null(jp)){
      paste0("--dependency=afterok:", paste(jp, collapse = ","), " ")
    } else {
      ""
    }
    path <- normalizePath(paste0(name, ".R"))

    r_log_file <-
      paste0("\\${SLURM_ARRAY_JOB_ID}.\\${SLURM_ARRAY_TASK_ID}-", name, ".Rout")
    r_log_fun <- task_env$config$r_logs
    r_log_path <- r_log_fun(normalizePath("."))
    ensure_exists(r_log_path)
    r_log_path <<- r_log_path
    r_log_path <- file.path(r_log_path, r_log_file)

    slurm_log_file <- paste0("%A-%a-", name, ".txt")
    slurm_log_fun <- task_env$config$slurm_logs
    slurm_log_path <- slurm_log_fun(normalizePath("."))
    ensure_exists(slurm_log_path)
    slurm_log_path <- file.path(slurm_log_path, slurm_log_file)

    array <- task_env$config$array

    incant <-
      paste0("MNGR_RFILE=", path,
             " MNGR_RLOGFILE=", r_log_path,
             " sbatch -J ", name,
             " --array=1-", array,
             " --parsable ", jp,
             " --output=", slurm_log_path,
             " ", getOption("mngr_cluster_path"), "/mngr_slurm_submit.darwin",
             "\n")
    jobid <- system(incant, intern = TRUE)
    time <- strftime(Sys.time(),  format = "%a %d %b %H:%M:%S")
    message(time, " Submitted ", name, " (", jobid, ")")
    #cat(incant)
    #    jobid <- "1"
    jobid <<- jobid
  })
}
