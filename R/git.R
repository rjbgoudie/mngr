git_clone_or_pull <- function(){
  repo <- git2r::repository(path = getwd())
  git_head_name <- head(repo)@name

  run_git_path <- mngr_run_git_path()
  dir.create(run_git_path, recursive = TRUE, showWarnings = FALSE)

  is_git_dir <- is_git_dir(run_git_path)
  if (is_git_dir){
    run_repo <- git2r::repository(path = run_git_path)
    git2r::pull(repo = run_repo)
    git2r::checkout(run_repo, branch = git_head_name)
    message("Pull complete")
  } else {
    git2r::clone(url = git_home(), local_path = run_git_path)
    run_repo <- git2r::repository(path = run_git_path)
    git2r::checkout(run_repo, branch = git_head_name)
    message("Clone complete")
  }
}

is_git_dir <- function(dir){
  incant <- paste("(cd ", dir, "; git rev-parse --is-inside-work-tree)")
  out <- NA
  out <- suppressWarnings(try(system(incant, intern = TRUE, ignore.stderr = TRUE), silent = TRUE))
  if (length(out) > 0 && !is.na(out) && out == "true"){
    TRUE
  } else {
    FALSE
  }
}

git_home <- function(){
  system("git rev-parse --show-toplevel", intern = TRUE)
}

git_sha_short <- function(){
  system("git rev-parse --short HEAD", intern = TRUE)
}

git_sha <- function(){
  system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
}
