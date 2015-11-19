#' @export
file_path <- function(file,
                      arm = T,
                      directory = "etc",
                      extension = ""){
  output_fun <- task_env$config$output
  np <- normalizePath(".")
  pre <- output_fun(mngr_run_path())

  directory <- ifelse(directory == "", "", paste0(directory, "/"))
  if (isTRUE(arm)){
    arm <- paste0(arm_name(), "/")
  }
  extension <- ifelse(extension == "", "", paste0(".", extension))

  path_directory <- paste0(pre, directory, arm)
  ensure_exists(path_directory)
  paste0(path_directory, file, extension)
}

#' @export
rds_file <- function(...){
  file_path(..., directory = "rds", extension = "rds")
}

#' @export
pdf_file <- function(...){
  file_path(..., directory = "plots", extension = "pdf")
}

#' @export
save <- function(..., file){
  warning("Please use saveRDS instead")
  base:::saveRDS(..., file = file)
}

#' @export
saveRDS <- function(..., file){
  unlink(file)
  base:::save(..., file = file)
}

#' @export
pdf <- function(file, ...){
  unlink(file)
  grDevices:::pdf(file = file, ...)
}

#' @export
tiff <- function(file, ...){
  unlink(file)
  grDevices:::tiff(file = file, ...)
}

#' @export
png <- function(file, ...){
  unlink(file)
  grDevices:::png(file = file, ...)
}
