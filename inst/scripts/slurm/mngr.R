source("~/.mngr/config.R")
library(mngr, quietly = TRUE, warn.conflicts = FALSE)
mngr:::run_cmd(args = commandArgs(TRUE))
