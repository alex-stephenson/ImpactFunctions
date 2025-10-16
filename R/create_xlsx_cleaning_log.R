create_other_df <- function(tool, choices) {

  # Step 1: Identify relevant 'other' text questions and their parent question names
  other_text_questions <- tool %>%
    dplyr::filter(type == "text", stringr::str_detect(relevant, "(?i)'other'\\)\\s*$")) %>%
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
    dplyr::filter(answer_name != 'other') %>%
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
