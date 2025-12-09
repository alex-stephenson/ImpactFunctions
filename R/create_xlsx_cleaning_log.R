#' Create a data frame linking 'other' text questions to their parent select questions
#'
#' This helper function identifies text questions in the survey tool that correspond to
#' "other (specify)" options in select questions. It then matches each "other" text
#' question with the list of answer choices from its parent select question.
#'
#' @param tool A data frame representing the "survey" sheet, containing at least
#'   the columns `type`, `name`, and `relevant`.
#' @param choices A data frame representing the "choices" sheet, containing at least
#'   the columns `list_name` and `name`.
#'
#' @return A data frame with one row per "other" text question, and two columns:
#'   \describe{
#'     \item{other_question}{The name of the "other" text question.}
#'     \item{choices}{A semicolon-separated string of the answer choices associated with its parent question.}
#'   }
#'
#' @details
#' The function looks for text-type questions whose `relevant` expression ends with an `'other')`
#' condition, then extracts their parent question name. It matches these to the corresponding
#' select questions and compiles their answer choices from the choices sheet.
#'
#' @examples
#' \dontrun{
#' tool <- data.frame(
#'   type = c("select_one fruit_list", "text"),
#'   name = c("favorite_fruit", "favorite_fruit_other"),
#'   relevant = c(NA, "${favorite_fruit} = 'other')")
#' )
#' choices <- data.frame(
#'   list_name = "fruit_list",
#'   name = c("apple", "banana", "other")
#' )
#'
#' create_other_df(tool, choices)
#' }
#'
#' @export
create_other_df <- function(tool, choices) {

  # Step 1: Identify relevant 'other' text questions and their parent question names
  other_text_questions <- tool %>%
    dplyr::filter(type == "text", stringr::str_detect(relevant, "(?i)'other'")) %>%
    dplyr::mutate(
      parent_question = stringr::str_extract(relevant, "\\{([A-Za-z0-9_]+)\\}") %>%
        stringr::str_remove_all("\\{|\\}")
    )

  # Step 2: Get list names for the parent questions
  parent_questions <- tool %>%
    dplyr::filter(stringr::str_detect(type, "select")) %>%
    dplyr::mutate(list_name = stringr::str_split_i(type, " ", 2)) %>%
    dplyr::filter(name %in% other_text_questions$parent_question)

  # Step 3: Filter choices to only relevant list_names
  relevant_choices <- choices %>%
    dplyr::filter(list_name %in% parent_questions$list_name)

  # Step 4: Combine everything into wide format: other question ~ its 'other' answer options
  questions_and_answers <- parent_questions %>%
    # parent_questions has: name (parent) and list_name
    dplyr::left_join(
      other_text_questions %>%
        dplyr::select(other_question = name, parent_question),
      by = c("name" = "parent_question")
    ) %>%
    dplyr::left_join(
      choices %>% dplyr::rename(answer_name = name),
      by = "list_name",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(! answer_name %in%  c('other', 'Other')) %>%
    dplyr::group_by(other_question) %>%
    dplyr::summarise(
      choices = paste(answer_name, collapse = ";\n"),
      .groups = "drop"
    )

  if (nrow(questions_and_answers) == 0) {
    stop("No 'other' questions with matching choices found.")
  }

  return(questions_and_answers)
}

#' Create a Validation List for Data Entry
#'
#' This function generates a validation list to be used for data entry validation.
#' It combines predefined validation lists with dynamically formatted choices derived from
#' the `choices` and `tool` parameters. The resulting dataframe is intended to provide
#' structured and valid choices for various question types.
#'
#' @param choices A dataframe representing the Kobo choices sheet.
#'        Expected to have at least the columns `list_name` and `name`.
#' @param tool A dataframe representing the Kobo survey sheet.
#'        Expected to have at least the columns `type` and `name`.
#' @param others Whether to include other questions, normally going to be inherited.
#'
#' @return A dataframe where each column corresponds to a choice list for a specific question.
#'         Each row contains a valid choice for the question.
#'
#' @examples
#' # Assume choices_df and tool_df are sample dataframes that fit the expected structure.
#' # validation_list <- create_validation_list(choices, tool)
#'
#' @export
create_validation_list <- function(choices, tool, others = F) {
  new_lists <- list(
    c("change_type_validation", "change_response;\nblank_response;\nremove_survey;\nno_action"),
    c("binaries_sm_options_lgl", "FALSE;\nTRUE"),
    c("binaries_sm_options_num", "0;\n1")
    # c("_duplicates_","-- keep the survey --;\n-- delete the survey --"),
    # c("_action_","-- confirm --;\n-- update --;\n-- delete --")
  ) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    stats::setNames(c("name", "choices"))

  choicelist <- new_lists %>%
    dplyr::bind_rows(create_formatted_choices(choices, tool) %>%
                       dplyr::select(name, choices))

  if (others) {

    extra = create_other_df(tool = tool, choices = choices)

    choicelist <- dplyr::bind_rows(
      choicelist,
      extra %>% dplyr::rename(name = other_question)
    )
  }

  choice_validation <- choicelist %>%
    unique() %>%
    data.table::transpose() %>%
    stats::setNames(.[1, ]) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate_all(~ stringr::str_split(., ";\n"))

  nrow_validation <- lapply(choice_validation, function(x) length(x[[1]])) %>%
    unlist() %>%
    max()

  data.val <- data.frame(matrix(NA, nrow = nrow_validation, ncol = 0))

  for (c in colnames(choice_validation)) {
    data.val <- data.val %>%
      dplyr::mutate(!!rlang::sym(c) := c(unlist(choice_validation[[c]]), rep(NA, nrow_validation - length(choice_validation[[c]][[1]]))))
  }

  return(data.val)
}




#' Creates formatted excel for cleaning log. You must pass the kobo_survey and kobo_choices if you want the other drop down.
#'
#' @param write_list List of dataframe
#' @param cleaning_log_name Name for cleaning log from write_list
#' @param change_type_col Change type column name in the cleaning log
#' @param header_front_color Hexcode for header front color (default is white)
#' @param header_front_size Header front size (default is 12)
#' @param header_fill_color Hexcode for header fill color (default is red)
#' @param header_front Front name for header (default is Arial Narrow)
#' @param body_front Front name for body (default is Arial Narrow)
#' @param body_front_size Front size for body (default is 11)
#' @param column_for_color Column name in the dataframe which should be used for colorizing the cell.
#' The default is null.
#' @param use_dropdown Use drop down lists for data validation in the cleaning log output
#' (default is FALSE)
#' @param use_others Binary TRUE or FALSE for whether you want recode_others to have drop down options
#' @param sm_dropdown_type Dropdown list options for select multiple questions:
#' "numerical" (1/0) or "logical" (TRUE/FALSE). The default is logical.
#' @param kobo_survey Kobo survey dataframe
#' @param kobo_choices Kobo choices dataframe
#' @param output_path Output path. Default is NULL which will return a workbook instead of an excel file.
#'
#' @return save a .xlsx file or return a workbook object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' checks_list <- cleaningtools::cleaningtools_raw_data |>
#'   check_pii(uuid_column = "X_uuid") |>
#'   check_duplicate(uuid_column = "X_uuid") |>
#'   check_value(uuid_column = "X_uuid")
#' create_combined_log(list_of_log = checks_list) |>
#'   create_xlsx_cleaning_log()
#'
#' logical_check_example <- cleaningtools::cleaningtools_raw_data |>
#'   check_logical(
#'     check_to_perform = 'treat_cook_water == "always_treat"',
#'     uuid_column = "X_uuid",
#'     description = "description",
#'     check_id = "check_4",
#'     columns_to_clean = "treat_cook_water"
#'   )
#' create_combined_log(logical_check_example) |>
#'   create_xlsx_cleaning_log(
#'     output_path = paste0(tempdir(check = TRUE), "/cleaning_log.xlsx"),
#'     cleaning_log_name = "cleaning_log",
#'     change_type_col = "change_type",
#'     kobo_survey = cleaningtools::cleaningtools_survey,
#'     kobo_choices = cleaningtools::cleaningtools_choices,
#'     use_dropdown = TRUE,
#'     sm_dropdown_type = "logical"
#'   )
#' }
create_xlsx_cleaning_log <- function(write_list,
                                     cleaning_log_name = "cleaning_log",
                                     change_type_col = "change_type",
                                     column_for_color = "check_binding",
                                     header_front_size = 12,
                                     header_front_color = "#FFFFFF",
                                     header_fill_color = "#ee5859",
                                     header_front = "Arial Narrow",
                                     body_front = "Arial Narrow",
                                     body_front_size = 11,
                                     use_dropdown = F,
                                     use_others = F,
                                     sm_dropdown_type = NULL,
                                     kobo_survey = NULL,
                                     kobo_choices = NULL,
                                     output_path = NULL) {
  if (use_dropdown & (is.null(kobo_survey) | is.null(kobo_choices))) {
    stop(glue::glue("Kobo survey and choices sheets should be provided to use dropdown lists"))
  }
  if (!is.null(kobo_survey) && !verify_valid_survey(kobo_survey)) {
    stop(glue::glue("The Kobo survey dataframe is not valid"))
  }
  if (!is.null(kobo_choices) && !verify_valid_choices(kobo_choices)) {
    stop(glue::glue("The Kobo choices dataframe is not valid"))
  }
  if (!is.null(sm_dropdown_type) && !stringr::str_to_lower(sm_dropdown_type) %in% c("logical", "numerical")) {
    stop(glue::glue("Invalid value for sm_dropdown_type - only 'logical' and 'numerical' are accepted"))
  }
  if (!cleaning_log_name %in% names(write_list)) {
    stop(glue::glue(cleaning_log_name, " not found in the given list."))
  }
  if (!change_type_col %in% names(write_list[[cleaning_log_name]])) {
    stop(glue::glue(change_type_col, " not found in ", cleaning_log_name, "."))
  }
  if ("validation_rules" %in% names(write_list)) {
    stop(glue::glue("The list currently has an element named `validation_rules`. Please consider renaming it."))
  }

  tryCatch(
    if (!is.null(kobo_survey) & !is.null(kobo_choices) & use_dropdown == TRUE) {
      data.val <- create_validation_list(kobo_choices,
                                         kobo_survey |> dplyr::filter(!stringr::str_detect(pattern = "(begin|end)(\\s+|_)group", type)),
                                         others = use_others)
    },
    error = function(e) {
      warning("Validation list was not created")
    }
  )

  if (!is.null(kobo_survey) & !is.null(kobo_choices) & use_dropdown == TRUE & exists("data.val", inherits = FALSE)) {
    write_list[["validation_rules"]] <- data.val
  } else {
    write_list[["validation_rules"]] <- data.frame(
      change_type_validation = c("change_response", "blank_response", "remove_survey", "no_action")
    )
  }



  write_list[["readme"]] <- data.frame(
    change_type_validation = c("change_response", "blank_response", "remove_survey", "no_action"),
    description = c(
      "Change the response to new.value",
      "Remove and NA the response",
      "Delete the survey",
      "No action to take."
    )
  )

  workbook <- write_list |> create_formated_wb(
    column_for_color = column_for_color,
    header_front_size = header_front_size,
    header_front_color = header_front_color,
    header_fill_color = header_fill_color,
    header_front = header_front,
    body_front = body_front,
    body_front_size = body_front_size
  )


  hide_sheet <- which(names(workbook) == "validation_rules")

  openxlsx::sheetVisibility(workbook)[hide_sheet] <- F


  row_numbers <- 2:(nrow(write_list[[cleaning_log_name]]) + 1)
  col_number <- which(names(write_list[[cleaning_log_name]]) == change_type_col)


  if (!is.null(kobo_survey) & !is.null(kobo_choices) & use_dropdown == TRUE & exists("data.val", inherits = FALSE)) {
    cl <- write_list[[cleaning_log_name]]

    for (r in 1:nrow(cl)) {
      if (cl[r, "question"] %in% colnames(data.val) & as.character(cl[r, "uuid"]) != "all") {
        openxlsx::dataValidation(workbook,
                                 sheet = cleaning_log_name, cols = which(colnames(cl) == "new_value"),
                                 rows = r + 1, type = "list",
                                 value = create_col_range(as.character(cl[r, "question"]), data.val)
        ) %>%
          suppressWarnings()
      } else if ((stringr::str_detect(string = as.character(cl[r, "question"]), pattern = "\\.") |
                  (stringr::str_detect(string = as.character(cl[r, "question"]), pattern = "\\/") &
                   (stringr::str_detect(string = as.character(cl[r, "question"]), pattern = "/") == 1))) &
                 as.character(cl[r, "uuid"]) != "all") {
        if (is.null(sm_dropdown_type) || stringr::str_to_lower(sm_dropdown_type) == "logical") {
          openxlsx::dataValidation(workbook,
                                   sheet = cleaning_log_name, cols = which(colnames(cl) == "new_value"),
                                   rows = r + 1, type = "list",
                                   value = create_col_range("binaries_sm_options_lgl", data.val)
          )
        } else {
          openxlsx::dataValidation(workbook,
                                   sheet = cleaning_log_name, cols = which(colnames(cl) == "new_value"),
                                   rows = r + 1, type = "list",
                                   value = create_col_range("binaries_sm_options_num", data.val)
          )
        }
      }
    }

    openxlsx::dataValidation(workbook,
                             sheet = cleaning_log_name, cols = col_number,
                             rows = row_numbers, type = "list",
                             value = create_col_range("change_type_validation", data.val)
    ) %>%
      suppressWarnings()
  } else {
    openxlsx::dataValidation(workbook,
                             sheet = cleaning_log_name, cols = col_number,
                             rows = row_numbers, type = "list",
                             value = "'validation_rules'!$A$2:$A$5"
    ) %>%
      suppressWarnings()
  }

  if (is.null(output_path)) {
    return(workbook)
  }

  if (!is.null(output_path)) {
    openxlsx::saveWorkbook(workbook, output_path, overwrite = TRUE)
  }
}
