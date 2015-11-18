git_home <- function(){
  system("git rev-parse --show-toplevel", intern = TRUE)
}

git_sha_short <- function(){
  system("git rev-parse --short HEAD", intern = TRUE)
}

git_sha <- function(){
  system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
}

ductr_run_git_path <- function(){
  git_home <- git_home()
  git_sha <- git_sha()
  home <- Sys.getenv("HOME")
  pattern <- paste0("^", home)
  git_home_from_home <- rel_path(dir = git_home, start = home)
  paste0(home, "/run/", git_home_from_home, "/", git_sha)
}

ductr_run_path <- function(){
  curr_wd <- getwd()
  git_home <- git_home()
  curr_from_git_home <- rel_path(dir = curr_wd, start = git_home)
  run_git_path <- ductr_run_git_path()
  paste0(run_git_path, "/", curr_from_git_home)
}

ductr_out_path <- function(){
  path_fun <- sub_path("^/home", "/scratch", "")
  path_fun(ductr_run_path())
}

is_a_run_dir <- function(dir){
  home <- Sys.getenv("HOME")
  pattern <- paste0("^", home, "/run")
  grepl(pattern, dir)
}

# uses sub for grep replacement
#' @export
sub_path <- function(pattern, replacement, dir){
  function(path){
    path <- normalizePath(path, mustWork = FALSE)
    out <- sub(pattern = pattern,
               replacement = replacement,
               x = path)
    file.path(out, dir)
  }
}
