#' Clone or pull to the run dir
#'
#' @param dir directory to clone or pull to the corresponding rundir
git_clone_or_pull <- function(dir = getwd()){
  repo <- git2r::repository(path = dir)
  git_head_name <- head(repo)@name

  run_git_toplevel_dir <- run_git_toplevel_dir(dir = dir)
  dir.create(run_git_toplevel_dir, recursive = TRUE, showWarnings = FALSE)

  is_git_dir <- is_git_dir(run_git_toplevel_dir)
  if (is_git_dir){
    run_repo <- git2r::repository(path = run_git_toplevel_dir)
    git2r::pull(repo = run_repo)
    git2r::checkout(run_repo, branch = git_head_name)
    message("Pull complete")
  } else {
    git2r::clone(url = git_toplevel_dir(), local_path = run_git_toplevel_dir)
    run_repo <- git2r::repository(path = run_git_toplevel_dir)
    git2r::checkout(run_repo, branch = git_head_name)
    message("Clone complete")
  }
}

git_abbrev_ref <- function(dir = getwd()){
  command <- "git rev-parse --abbrev-ref HEAD"
  system_in_dir(command, dir = dir, intern = TRUE)
}
