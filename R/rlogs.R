count_rout_warnings <- function(log){
  cmd <- paste('grep -c -i "^Warning"', log)
  system(cmd, intern = TRUE)
}

count_rout_errors <- function(log){
  cmd <- paste('grep -c -i "^Error"', log)
  system(cmd, intern = TRUE)
}
