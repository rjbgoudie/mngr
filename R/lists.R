#' Parse job names
#'
#' @param x A job name
#' @param type Either "rout", "rlatest" or "squeue"
parse_name <- function(x, type = "rout"){
  probe_text <- head(x$name, 1)
  matches <- gregexpr("__", probe_text)[[1]]
  nmatches <- length(matches)
  min_matches <- switch(type,
                        `rout` = 2,
                        `rlatest` = 1,
                        `squeue` = 2)
  # a valid job name has at least 2 __
  if (nmatches >= min_matches){
    npieces <- nmatches + 1
    piece_names <- paste0("piece", seq_len(npieces))

    x <- x %>%
      separate(name, piece_names, "__")

    if (type == "rout"){
      x <- x %>%
        rename(jid = piece1, jobname = piece2)

      arm_pieces <- piece_names[-(1:2)]
    } else if (type == "rlatest"){
      x <- x %>%
        rename(jobname = piece1)
      arm_pieces <- piece_names[-1]

      # Other functions that assume a jid is present, so add dummy
      x$jid <- 1:nrow(x)
    } else if (type == "squeue"){
      x <- x %>%
        rename(jobname = piece1) %>%
        rename_("shortsha" = tail(piece_names, 1))
      arm_pieces <- piece_names[2:(npieces - 1)]
    }

    # spread_ needs a unique identifier for each row, so add a dummy
    x$row_id <- 1:nrow(x)

    x <- x %>% mutate_at(vars(one_of(arm_pieces)), funs(paste0("arm___", .)))

    for (i in seq_along(arm_pieces)){
      arm_piece_name <- paste0("name", arm_pieces[i])
      new_piece_names <- c(arm_piece_name, arm_pieces[i])
      x <- x %>%
        separate_(arm_pieces[i], new_piece_names, "--") %>%
        spread_(new_piece_names[1], new_piece_names[2])
    }
    x <- x %>% select(-row_id)
  } else {
    warning("Invalid job name found")
  }
  x
}

#' Pretty print squeue output
pretty_print_squeue <- function(x){
  out <- x %>%
    arrange(jobname) %>%
    select(jobname,
           starts_with("arm___"),
           jid,
           slurm_status,
           nodes,
           t_used,
           t_left,
           nodelist_reason) %>%
    rename(n = nodes, s = slurm_status)

  arm_cols <- substring(colnames(out), 1, 6) == "arm___"
  colnames(out)[arm_cols] <- paste0("\033[36m",
                                    substring(colnames(out), 7)[arm_cols],
                                    "\033[39m")

  out <- out %>%
    mutate_all(funs(as.character)) %>%
    mutate_all(funs(coalesce(., ""))) %>%
    as.data.frame

  cat("\n", slurm_summary(squeue_status = x), "\n")
  cat_df(out)
}


pretty_print_rout <- function(x){
  if (nrow(x) == 0){
    cat("No R out files to print yet")
  } else {
    x <- x %>%
      mutate(finished = TRUE) %>%
      prep_finished_success

    finished_success <- finished_success(x)
    more_complex <- x %>%
      filter(!all_finished_success) %>%
      mutate(mean_time = internal_run_seconds,
             jid = paste(jid))

    out <- bind_rows(finished_success, more_complex) %>%
      arrange(jobname) %>%
      mutate(ok = pretty_success_status(TRUE, success),
             t = pretty_print_time(mean_time)) %>%
      rename(e = errors,
             w = warnings) %>%
      select(jobname,
             jid,
             starts_with("arm___"),
             t,
             ok,
             e,
             w,
             final)

    out <- link_to_arm_indicies(out)
    arm_cols <- substring(colnames(out), 1, 6) == "arm___"
    colnames(out)[arm_cols] <- paste0("\033[36m",
                                      substring(colnames(out), 7)[arm_cols],
                                      "\033[39m")

    out <- out %>%
      mutate_all(funs(as.character)) %>%
      mutate_all(funs(coalesce(., ""))) %>%
      as.data.frame

    cat_df(out)
  }
}

#' Merge rout status table with arm data
#'
#' Merges rout status table with arm data, so that even when mngr-latest-logs
#' is run we still know which arm corresponds to which values etc
#'
#' @param x A data.frame with "arm___xxx" columns
#'
link_to_arm_indicies <- function(x){
  link_to_arm_indicies_single_jobname <- function(x){
    name <- x$jobname[1]
    task_obj <- task_get(name)
    arms <- task_obj$arms_to_invoke() %>%
      select(-MNGR_DEFAULT_ARM) %>%
      rowwise %>%
      summarize_all(paste_or_hash) %>%
      mutate(MNGR_ARM_ID = row_number())
    arm_cols <- substring(colnames(x), 1, 6) == "arm___"
    colnames(x)[arm_cols] <- substring(colnames(x)[arm_cols], 7)
    left_join(x, arms, by = colnames(x)[arm_cols]) %>%
      rename(arm_id = MNGR_ARM_ID) %>%
      arrange(arm_id) %>%
      select("jobname", "arm_id", everything())
  }

  x %>%
    group_by(jobname) %>%
    do(link_to_arm_indicies_single_jobname(.)) %>%
    ungroup
}

pretty_print_merged <- function(full_status, slurm_status){
  # calculations for subdividing
  full_status <- full_status %>%
    prep_finished_success %>%
    group_by(jobname) %>%
    mutate(all_awaiting_dependency = all(awaiting_dependency),
           all_same = all(partition == partition[1]))

  finished_success <- finished_success(full_status)

  awaiting_dependency <- full_status %>%
    filter(all_awaiting_dependency) %>%
    summarise(mean_time = NA,
              success = NA,
              jid = NA,
              nodes = max(nodes),
              slurm_status = slurm_status[1],
              t_left = t_left[1],
              nodelist_reason = NA)

  more_complex <- full_status %>%
    ungroup() %>%
    filter(!(all_finished_success == TRUE | all_awaiting_dependency == TRUE)) %>%
    mutate(mean_time = internal_run_seconds,
           jid = paste(jid),
           slurm_status = if_else(slurm_status == "R",
                                  paste0("\033[32m",
                                         slurm_status,
                                         "\033[39m"),
                                  paste(slurm_status)),
           nodelist_reason = nodelist_reason)

  out <- bind_rows(finished_success, awaiting_dependency, more_complex) %>%
    mutate(ok = pretty_success_status(finished, success),
           t = pretty_print_time(mean_time)) %>%
    arrange(jobname) %>%
    select(jobname,
           starts_with("arm___"),
           jid,
           slurm_status,
           nodes,
           t_used,
           t_left,
           t,
           ok,
           nodelist_reason,
           final) %>%
    rename(n = nodes,
           s = slurm_status,
           node_reason = nodelist_reason)

  arm_cols <- substring(colnames(out), 1, 6) == "arm___"
  colnames(out)[arm_cols] <- paste0("\033[36m",
                                    substring(colnames(out), 7)[arm_cols],
                                    "\033[39m")

  out <- out %>%
    mutate_all(funs(as.character)) %>%
    mutate_all(funs(coalesce(., ""))) %>%
    as.data.frame

  cat("\n", slurm_summary(slurm_status), "\n")
  cat_df(out)
  # full_status %>% as.data.frame
}

# must have success, finished, internal_run_seconds columns
finished_success <- function(x){
  x %>%
    group_by(jobname) %>%
    filter(all_finished_success) %>%
    summarise(mean_time = mean(internal_run_seconds),
              success = TRUE,
              finished = TRUE,
              jid = paste(head(jid, 1), tail(jid, 1), sep = ":"),
              nodelist_reason = NA)
}

prep_finished_success <- function(x){
  x %>%
    group_by(jobname) %>%
    mutate(all_finished_success = all(success) & all(finished)) %>%
    ungroup()
}

#' Merge squeue and rout status tables
#'
#' @param squeue_status A squeue table
#' @param rout_df A data.frame of rout
#' @return A merged table
merge_tables <- function(squeue_status, rout_df){
  # join tables
  arm_cols <- substring(colnames(squeue_status), 1, 6) == "arm___"
  join_cols <- c("jid", "jobname", colnames(squeue_status)[arm_cols])

  full_status <- full_join(squeue_status,
                           rout_df,
                           by = join_cols) %>%
    group_by(jid)

  full_status %>%
    summarise_all(funs(.[!is.na(.)][1])) %>%
    ungroup() %>%
    mutate(finished = is.na(nodelist_reason),
           awaiting_dependency = nodelist_reason == "(Dependency)")
}

#' Pretty print the time
#'
#' Print times as "XXh XXm XXs"
#'
#' @param t A vector of times
#' @return A character vector of pretty print times
pretty_print_time <- function(t){
  sapply(t, function(time){
    if (!is.na(time)){
      paste0(unname(time %/% 3600),
             "h ",
             unname((time %% 3600) %/% 60),
             "m ",
             round(unname(time %% 60)),
             "s")
    } else {
      time
    }
  })
}

#' Pretty print ticks and crosses
#'
#' Returns a vector with ticks for components that have finished successfully,
#' and crosses for components that finished unsuccessfully
#'
#' @param finished A logical vector of finished indicators
#' @param success A logiscal vector of success indicators
#' @return A character vector of ticks, crosses and NAs
pretty_success_status <- function(finished, success){
  out <- NA
  out <- ifelse(finished & success, "\033[32m✔︎\033[39m", NA)
  out <- ifelse(finished & !success, "\033[31m✘\033[39m", out)
  out
}
