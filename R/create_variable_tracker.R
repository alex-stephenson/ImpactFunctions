#' Create a Variable Tracker to Compare Raw and Cleaned Datasets
#'
#' This function compares the column names of a raw dataset and a cleaned dataset
#' and generates a tracker showing which variables have been added or removed.
#' It should be included as a tab in the data submission to HQ.
#'
#' @param raw_data A data frame representing the original/raw dataset before cleaning.
#' @param clean_data A data frame representing the cleaned dataset.
#'
#' @return A data frame with three columns:
#' \describe{
#'   \item{Variable}{The name of the variable that was added or removed.}
#'   \item{Action}{Indicates whether the variable was "added" (present in `clean_data` but not in `raw_data`)
#'                 or "remove" (present in `raw_data` but not in `clean_data`).}
#'   \item{Rationale}{A blank field where the rationale for each action can be added manually.}
#' }
#'
#' @import dplyr
#'
#' @examples
#' library(dplyr)
#' raw_data <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#' clean_data <- data.frame(a = 1:3, d = 10:12)
#' create_variable_tracker(raw_data, clean_data)
#'
#' @export


create_variable_tracker <- function(raw_data, clean_data) {

  removed_cols <- data.frame(Variable = colnames(raw_data)) %>%
    filter(! Variable %in% colnames(clean_data)) %>%
    mutate(Action = "remove",
           Rationale = "")


  new_cols <- data.frame(Variable = colnames(clean_data)) %>%
    filter(! Variable %in% colnames(raw_data)) %>%
    mutate(Action = "added",
           Rationale = "")
  variable_tracker <- bind_rows(removed_cols, new_cols)
  return(variable_tracker)
}
