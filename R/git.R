#' Clone or pull to the run dir
#'
#' Clone or pull to the directory to the corresponding run directory
#'
#' 1. Ensure dir is a git worktree
#' 2. Get the git HEAD name
#' 3. Create the run directory if it does not exist
#' 4. Clone/pull as required
#'
#' @param dir directory to clone or pull to the corresponding rundir
git_clone_or_pull <- function(dir = getwd()){
  dir_is_inside_git_work_tree <- is_inside_git_work_tree(dir)
  if (!dir_is_inside_git_work_tree){
    stop("Directory is not a git directory")
  }
  dir_git_toplevel <- dir_git_toplevel(dir)
  repo <- git2r::repository(path = dir_git_toplevel)

  if (packageVersion("git2r") >= "0.22.1"){
    git_head_name <- git2r::repository_head(repo)$name
  } else {
    git_head_name <- git2r::head(repo)@name
  }

  dir_run_branch_toplevel <- dir_run_branch_toplevel(dir = dir, check = TRUE)
  dir.create(dir_run_branch_toplevel, recursive = TRUE, showWarnings = FALSE)

  run_is_inside_git_work_tree <- is_inside_git_work_tree(dir_run_branch_toplevel)
  if (run_is_inside_git_work_tree){
    run_repo <- git2r::repository(path = dir_run_branch_toplevel)
    git2r::pull(repo = run_repo)
    git2r::checkout(run_repo, branch = git_head_name)
    message("Pull complete")
  } else {
    git2r::clone(url = dir_git_toplevel(check = FALSE),
                 local_path = dir_run_branch_toplevel)
    run_repo <- git2r::repository(path = dir_run_branch_toplevel)
    git2r::checkout(run_repo, branch = git_head_name)
    message("Clone complete")
  }
}

#' Abbreviated name for HEAD commit in git repository
#'
#' Uses git rev-parse --abbrev-ref to get the branch name when appropriate
#'
#' @param dir A directory
#' @param base_only Logical, should only the first part of a foo/bar branch
#' name be returned
#' @return A character with the abbrev-ref
git_abbrev_ref <- function(dir = getwd(), base_only = TRUE){
  command <- "git rev-parse --abbrev-ref HEAD"
  abbrev_ref <- system_in_dir(command, dir = dir, intern = TRUE)
  if (base_only){
    strsplit(abbrev_ref, "/", fixed = T)[[1]][1]
  } else {
    abbrev_ref
  }
}

#' Short SHA for HEAD commit in git repository
#'
#' Uses git rev-parse
#'
#' @param dir A directory
#' @param length Number of characters
#' @return A character vector of length 1 containing the short SHA for the HEAD
#' commit
git_short_sha <- function(dir = getwd(), length = 4){
  command <- paste0("git rev-parse --short=", length, " HEAD")
  system_in_dir(command, dir = dir, intern = TRUE)
}
