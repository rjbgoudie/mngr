#' Ductr
#' @docType package
#' @name ductr
#' @import tools git2r
NULL

task_env <- new.env()

ductr_postrun <- function(action){
}

log_filepath <- function(file){
}

#' @export
ductr_config <- function(...){
  # this implementation is rubbish
  elements <- list(...)
  assign("config", elements, envir = task_env)
}

save <- function(..., file){

  save(..., file = file)
}

load <- function(file, ...){

  load(file = file, ...)
}

ductr_copy_to_run_dir <- function(){
  system("require-clean-work-dir")
}

find_ductfile <- function(dir){
  path <- file.path(dir, "Ductfile.R")
  if (file.exists(path)){
    path
  } else {
    stop("Can't find Ductfile")
  }
}

#' @export
run <- function(name){
  clean <- system("require-clean-work-dir", intern = TRUE)
  if (length(clean) > 0 && attr(clean, "status") == 1){
    stop(clean)
  }
  git_clone_or_pull()

  jobids <- c()
  with_dir(ductr_run_path(), {
    ductfile <- find_ductfile(getwd())
    source(ductfile)

    expr <- substitute(name)
    if (is.name(expr)){
      name <- as.character(expr)
    }
    id <- task_find_id(name, exists = TRUE)
    lapply(task_env$post_run_list, eval)
    new_jobid <- task_env$tasklist[[id]]$invoke()
    jobids <- c(jobids, new_jobid)

    # this is the wrong place for this
    r_log_fun <- task_env$config$r_logs
    r_log_path <- r_log_fun(normalizePath("."))

    invisible(TRUE)
  })
  paste(paste(jobids, collapse = ","), r_log_path)
}


shell <- function(){

  cmd <- "ductr_watchlast () {
    local up=$(tput cuu1)$(tput el) IFS=$'\n\' lines
  local start=${3:-0} end
  lines=($(Rscript --no-init-file ~/shovel/rjb_status.R))
  end=${4:-${#lines[@]}}
  echo \"${lines[*]:$start:$end}\"
  while true
  do
    lines=($(Rscript -e 'Sys.time()'))
    end=${4:-${#lines[@]}}
    printf \"%.0s$up\" \"${lines[@]:$start:$end}\"
    echo \"${lines[*]:$start:$end}\"
    sleep ${2:-2}
    # go up and clear each line
  done
};
ductr_watchlast
"


}

## #,
## #           rdata = sub_path("^/home", "/scratch", "rdata"),
## #           plots = sub_path("^/home", "/scratch", "plots"))

## rfile("1-a")
## rfile("2-b")
## rfile("3-c")

# Want a single log of the overall progress
# plus files for each r file, with that files' situation
# each job is a different process now, who knows when each starts.

# decouple run directory from git branch nam
# r function that queries sbatch for job status every few seconds

# data to pass into the R process
# ductr_arm

# "fP",  which is rather underused by me at the moment
# git branch which is very heavily used at the moment

# input vs output location

## branch/a_1/a_v/adf_2/
##   folder variant vs naming variant

## ductr_full_factorial(a = c(1, 2, 3), b = c("e", "d"))

## df <- data.frame(a = c(1, 2), b = c("e", "d"))
## ductr_fractional_factorial(df)

## to be able to run from the command line, need rdataFile to know about git
## because otherwise the relative path is not correct.
## ie need to make ~/run not organised by branch?

## can I delete run at the end of the run
## ie run/analyses/mwp/sha/copyofwholegitrepo

## run
## pathtogitbasedir
## br-sha
## gitdir

## should avoid the deleting issue

## but don\'t know the jid until too late? or perhaps the copying should be done
## by the compute node?

## flow number as well as a job number?

## config run_dir

## logdir - basically an easy function of the outdir?
## outdir
## rundir
## indir

## ductr_equiv_path("scratch")

## ductr_config(logs = "logs",
##            rdata = "rdata",
##            plots = "plots")
