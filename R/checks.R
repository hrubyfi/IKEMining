#' Initiate The Join Check function
#'
#' Place this function before joining two or more dataframes
#'
#' @param data A dataframe
#'
#' @export
check_join_before <- function(data) {
  force(data);
  function() data
  assign('join_check', data, envir = .GlobalEnv)

  return(data)
}


#' Display Status Of The Join  Check Function
#'
#' Place this function after joining two or more dataframes
#'
#' @param data A dataframe. Required.
#' @param ID Column name of the join by atribute. Required.
#' @param removed_rows Logical value. When `TRUE` displays removed rows. Default `FALSE`.
#' @param group_size Logical value. When `TRUE` displays table of group sizes by ID. Default `FALSE`.
#' @param check_occurance Integer. Show rows from group (group_size group) of specific size. Default `NA`.
#' @param show_removed Logical value. When `TRUE` displays removed rows from before the join. Default `FALSE`.
#'
#' @importFrom plyr empty
#' @importFrom knitr kable
#'
#' @export
check_join_after <- function(data, ID, removed_rows = FALSE, group_size = FALSE, check_occurance = NA, show_removed = FALSE) {

  force(data);
  function() data

  old_data <- get('join_check', data, envir = .GlobalEnv)

  cat("---NUMBER OF ROWS---")
  cat("\nBEFORE JOIN: ", nrow(old_data), "\n AFTER JOIN: ", nrow(data), "\n")


  cat("\n---ROW OPERATIONS---\n")
  r_rows <- nrow(old_data) - nrow(data)
  a_rows <- nrow(data) - nrow(old_data)
  if (r_rows <= 0) {
    r_rows = 0
  }
  if (a_rows <= 0) {
    a_rows = 0
  }

  cat("Removed rows:", r_rows)
  cat("\nAdded rows:", a_rows, "\n")

  if (removed_rows) {
    cat("\n---REMOVED ROWS LIST---\n")
    removed_rows <- dplyr::setdiff(old_data, data %>% select(colnames(old_data)))
    print_length <- nrow(removed_rows)
    print(removed_rows, n = print_length)
  }

  if (group_size) {
    cat("\n---GROUP SIZE BY ID---\n")

    tab <- knitr::kable(data %>%
                          count(Patient, name="Group size")
                        %>% count(`Group size`, name = "Number of occurances"))

    cat(tab, sep="\n")
  }

  if (!is.na(check_occurance)) {
    cat("\n---ID OF ROWS WITH", check_occurance ,"OCCURANCES---\n")
    id_of_rows <- data %>% group_by({{ID}}) %>% filter(n() == check_occurance) %>% ungroup() %>% pull({{ID}})
    id_rows_with <- knitr::kable(data %>% filter({{ID}} %in% id_of_rows) %>% arrange({{ID}}))
    cat(id_rows_with, sep="\n")
  }

  cat("\n---REMOVED IDs---\n")
  old_id <- old_data %>% pull({{ID}})
  new_id <- data %>% pull({{ID}})
  cat(length(setdiff(old_id, new_id)))

  if (show_removed) {
    cat("\n---REMOVED ROWS---\n")
    table_removed_rows <- old_data %>% filter({{ID}} %in% setdiff(old_id, new_id)) %>% arrange({{ID}})
    if (!plyr::empty(table_removed_rows)){
      cat(knitr::kable(table_removed_rows), sep="\n")
    } else {
      cat("No rows were removed")
    }

  }

  cat("\n---DUPLICATE ROWS BY ID---")
  #cat(old_data %>% count({{ID}}, name="nrows") %>% count(nrows) %>% pull(n))

  cat("\nBEFORE JOIN: ",
      old_data %>% group_by({{ID}}) %>% filter(row_number() > 1) %>% ungroup() %>% count() %>% pull(n))
  cat("\nAFTER JOIN: ",
      data %>% group_by({{ID}}) %>% filter(row_number() > 1) %>% ungroup() %>% count() %>% pull(n))
  remove('join_check', envir = .GlobalEnv)
  invisible(data)
}

initiate_join_check <- function() {
  assign('join_check', 0, envir = .GlobalEnv)
}
