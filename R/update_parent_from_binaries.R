

#' Update parent columns based on child binaries
#' @description A function that updates the parent column of select multiple questions in the clogs if there are any child binaries.
#'
#' This issue can arise if there are binaries that have been changed from 0 to 1 but the FO did not update the concatenated values in the new value column.
#'
#' Please note that the separator used for select multiple questions should be "/".
#' @param data a cleaning log dataframe
#' @param tool_path the path to the Kobo tool file
#' @param survey_sheet the sheet name, defaults to "survey"
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import readxl
#' @export
#' @examples
#' update_parent_values(data = combined_clogs,
#' tool_path =  kobo_path)

update_parent_values <- function(data, tool_path, survey_sheet = "survey") {

  ## lets first make sure we're only using select multiple questions

  kobo_survey <- tryCatch(read_excel(tool_path, sheet = survey_sheet, .name_repair = "unique_quiet"),
                          error = function(e) stop("Error reading survey sheet: ", e))

  kobo_survey_sm <- kobo_survey %>%
    filter(str_detect(type, "multiple")) %>%
    pull(name)

  data<- data %>%
    filter(question %in% kobo_survey_sm,
           change_type == "change_response") %>%
    mutate(question_stem = str_extract(question, "^[^/]+"))


  # Identify the parent rows (those without '/') and child rows (those with '/')
  data <- data %>%
    mutate(
      is_parent = !str_detect(question, "/"),
      child_option = ifelse(!is_parent, str_remove(question, ".*/"), NA)
    )

  # Get the parent rows that have missing child options
  missing_children <- data %>%
    filter(!is_parent, new_value == "1") %>%
    select(uuid, question_stem, child_option) %>%
    distinct()

  # Ensure the 'child_option' is retained after the join
  data_updated <- data %>%
    filter(is_parent) %>%
    left_join(missing_children, by = c("uuid", "question_stem")) %>%
    group_by(uuid, question_stem) %>%
    mutate(
      # Combine the parent value and missing child options
      updated_new_value = paste(
        unique(c(str_split(new_value, "\\s+")[[1]], child_option.x, child_option.y)),
        collapse = " "
      )
    ) %>%
    ungroup()

  # Check if the 'updated_new_value' exists and fix the issue by using proper column names
  if("updated_new_value" %in% colnames(data_updated)) {
    data_final <- data_updated %>%
      bind_rows(data %>% filter(!is_parent)) %>%
      distinct() %>%
      filter(!is.na(uuid) & !is.na(question)) %>%
      arrange(uuid, question)
  } else {
    stop("Error: 'updated_new_value' column is not present in the data.")
  }

  resolved_data <- data_final %>%
    select(question, check_binding, select_multiple_value = updated_new_value) %>%
    filter(!str_detect(question, "/"))

  parent_q_correct <- change_response %>%
    left_join(resolved_data, by = join_by(question == question, check_binding == check_binding))

  combined_new_answers <- parent_q_correct %>%
    mutate(new_value = ifelse(is.na(select_multiple_value), new_value, select_multiple_value),
           new_value = str_replace_all(new_value, "NA", ""),
           new_value = str_squish(new_value)) %>%
    distinct()

  return(combined_new_answers)

}
