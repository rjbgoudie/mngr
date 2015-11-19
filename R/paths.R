mngr_run_git_path <- function(){
  git_home <- git_home()
  git_sha <- git_sha()
  home <- Sys.getenv("HOME")
  pattern <- paste0("^", home)
  git_home_from_home <- rel_path(dir = git_home, start = home)
  paste0(home, "/run/", git_home_from_home, "/", git_sha)
}

mngr_run_path <- function(){
  curr_wd <- getwd()
  git_home <- git_home()
  curr_from_git_home <- rel_path(dir = curr_wd, start = git_home)
  run_git_path <- mngr_run_git_path()
  paste0(run_git_path, "/", curr_from_git_home)
}

mngr_out_path <- function(){
  path_fun <- sub_path("^/home", "/scratch", "")
  path_fun(mngr_run_path())
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
