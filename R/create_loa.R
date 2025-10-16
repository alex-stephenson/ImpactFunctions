#' Automatically produce a LOA from a Kobo Tool and survey data
#'
#' This function produces a LOA to be used in the production of results tables from
#' `analysistools::create_analysis`.
#' The function creates a LOA based on the Kobo server tool, inferring the analysis_type from
#' the 'type' column in the tool.
#' It adds to this by looking at any data in the survey that is not present in the tool
#' and attempts to guess based on the class of the column.
#'
#' @param kobo_survey Your Kobo survey. Must have columns type and name. Leave blank if you want the LOA from the dataframe instead
#' @param data Your survey data, for capturing composite indicators not from your tool. Leave blank if only tool is required.
#' @param level confidence interval, defaults to 0.95
#' @param group_vars disaggregation variables, to be provided as a string. E.g. c('admin1', 'admin2). Leave blank for no disaggregation.
#' @param sm_sep defaults to '/'
#' @param print_class_type TRUE/FALSE whether to output any analysis_type assumed based on the data type.
#'
#' @return A dataframe containing
#' \describe{
#'   \item{analysis_var}{name of the indicator}
#'   \item{analysis_type}{analysis to perform}
#'   \item{level}{Confidence Interval}
#'   \item{group_var}{indicators to disaggregate by}
#' }
#'
#' @examples
#' create_loa(kobo_survey = tool, data = clean_data, group_vars = c('admin_1', 'admin2'))
#'
#' @importFrom dplyr select mutate case_when rename filter cross_join bind_rows
#' @importFrom stringr str_detect fixed
#' @importFrom tidyr separate_longer_delim
#' @importFrom tibble tibble
#' @importFrom purrr walk2
#' @importFrom cleaningtools auto_detect_sm_parents
#' @export

create_full_loa <- function(kobo_survey = NULL,
                       data = NULL,
                       level = 0.95,
                       group_vars = character(0),
                       sm_sep = "/",
                       print_class_type = TRUE) {

  # Infer tool/data usage automatically
  if (is.null(kobo_survey)) {
    use_tool <- FALSE
  } else {
    use_tool <- TRUE
  }

  if (is.null(data)) {
    use_data <- FALSE
  } else {
    use_data <- TRUE
  }

  if (isFALSE(use_tool) & isFALSE(use_data)) {
    stop("Must provide either tool or data")
  }

  if (isTRUE(use_tool)) {
    stopifnot(all(c("type", "name") %in% names(kobo_survey)))

    tool_loa <- kobo_survey %>%
      select(type, name) %>%
      mutate(
        analysis_type = case_when(
          type == "integer" ~ "mean:median",
          str_detect(type, "select_one") ~ "prop_select_one",
          str_detect(type, "select_mult") ~ "prop_select_multiple"
        ),
        level = 0.95
      ) %>%
      separate_longer_delim(analysis_type, ":") %>%
      rename(analysis_var = name) %>%
      select(-type) %>%
      filter(!is.na(analysis_type))
  }

  if (isTRUE(use_data)) {
    survey_metatdata <- c(
      '_index', '_id', '__version__', 'instanceID', '_xform_id_string',
      'uuid', 'rootUuid', '_status', '_submission_time', '_validation_status',
      '_submitted_by', '_attachments', 'weights'
    )

    sm_q <- cleaningtools::auto_detect_sm_parents(data, sm_separator = sm_sep)

    inferred <- vapply(names(data), function(colname) {
      if (colname %in% sm_q) {
        return("prop_select_multiple")
      } else if (is.character(data[[colname]])) {
        return("prop_select_one")
      } else if (is.numeric(data[[colname]])) {
        return("median:mean")
      } else {
        return(NA_character_)
      }
    }, character(1), USE.NAMES = TRUE)
    custom_loa <- tibble(
      analysis_var = names(inferred),
      analysis_type = as.character(inferred)
    ) %>%
      mutate(level = !!level) %>%
      filter(!analysis_var %in% survey_metatdata)

    if (isTRUE(use_tool)) {
      custom_loa <- custom_loa %>%
        filter(!str_detect(analysis_var, fixed(sm_sep))) %>%
        filter(!analysis_var %in% kobo_survey$name)
    }

    if (isFALSE(use_tool)) {
      custom_loa <- custom_loa %>%
        filter(!str_detect(analysis_var, fixed(sm_sep)))
    }

    if (isTRUE(print_class_type)) {

      message("Indicators inferred from data, not from tool, include...:")

      walk2(custom_loa$analysis_var, custom_loa$analysis_type, function(analysis_var, analysis_type) {
        if (is.na(analysis_type)) {
          message(paste0(analysis_var, " is not type select or numeric, removed"))
        } else if (analysis_type == "prop_select_one") {
          message(paste0(analysis_var, " classified as prop_select_one"))
        } else {
          message(paste0(analysis_var, " is classified as mean / median"))
        }
      })
    }
  }

  if (identical(group_vars, character(0))) {
    group_vars_NA <- data.frame(group_var = NA)
  } else {
    group_vars_NA <- data.frame(group_var = append(NA, group_vars))
  }

  if (isTRUE(use_tool) & isTRUE(use_data)) {
    full_loa <- custom_loa %>%
      separate_longer_delim(analysis_type, ":") %>%
      filter(!is.na(analysis_type)) %>%
      bind_rows(., tool_loa) %>%
      cross_join(group_vars_NA) %>%
      filter(group_var != analysis_var | is.na(group_var))
    return(full_loa)
  }

  if (isTRUE(use_tool) & isFALSE(use_data)) {
    output_loa <- tool_loa %>%
      filter(!is.na(analysis_type)) %>%
      separate_longer_delim(analysis_type, ":") %>%
      cross_join(group_vars_NA) %>%
      filter(group_var != analysis_var | is.na(group_var))
    return(output_loa)
  }

  if (isFALSE(use_tool) & isTRUE(use_data)) {
    output_loa <- custom_loa %>%
      filter(!is.na(analysis_type)) %>%
      separate_longer_delim(analysis_type, ":") %>%
      cross_join(group_vars_NA) %>%
      filter(group_var != analysis_var | is.na(group_var))
    return(output_loa)
  }
}
