#' Apply a simple presentresults style to an openxlsx workbook
#'
#' This helper applies a two-tone style to a results sheet written with \pkg{openxlsx}:
#' a bold red/white header row and a light-brown / grey style for the first three data columns.
#'
#' @param wb An openxlsx Workbook object (created with `openxlsx::createWorkbook()`).
#' @param sheet Sheet name (string) or index where `results_df` has been written.
#' @param results_df A data.frame containing the table that was written into the sheet. Used to determine
#'   row and column counts for applying styles.
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
#' @examples
#' \dontrun{
#' wb <- openxlsx::createWorkbook()
#' openxlsx::addWorksheet(wb, "test")
#' df <- data.frame(A = 1:3, B = letters[1:3], C = LETTERS[1:3])
#' openxlsx::writeData(wb, "test", df)
#' presentresults_simple_style(wb, "test", df)
#' }
presentresults_simple_style <- function(wb, sheet, results_df) {
  if (missing(wb) || missing(sheet) || missing(results_df)) {
    stop("Please provide wb, sheet, and results_df")
  }

  n_rows <- nrow(results_df)
  n_cols <- ncol(results_df)

  # Create styles
  header_style <- openxlsx::createStyle(
    fgFill = "#EE5859",         # red background
    fontColour = "#FFFFFF",     # white text
    textDecoration = "bold",
    halign = "center", valign = "center",
    border = "Bottom"
  )

  first3_style <- openxlsx::createStyle(
    fgFill = "#D2CBB8",         # light brown / beige
    fontColour = "#58585A",     # grey text
    halign = "left", valign = "center"
  )

  # Apply header style (row 1)
  openxlsx::addStyle(wb, sheet = sheet, style = header_style,
                     rows = 1, cols = seq_len(n_cols), gridExpand = TRUE, stack = TRUE)

  # Apply first-3-columns style to data rows only (rows 2 .. n_rows+1)
  if (n_cols >= 1 && n_rows >= 1) {
    cols_to_style <- seq_len(min(3, n_cols))
    data_rows <- seq_len(n_rows) + 1
    openxlsx::addStyle(wb, sheet = sheet, style = first3_style,
                       rows = data_rows, cols = cols_to_style, gridExpand = TRUE, stack = TRUE)
  }

  invisible(TRUE)
}


#' Generate multi-group results tables, save per-group files and optionally aggregate
#'
#' Run an analysistools/presentresults pipeline for each group specified in `loa` (the LOA table),
#' write per-group XLSX result files, and optionally aggregate the per-group sheets into a single workbook.
#'
#' This function mirrors the pipeline used interactively in the user's scripts. It:
#' - iterates over distinct `loa$group_var` values (excluding `NA`),
#' - for each group creates a `loa_g` (including overall rows),
#' - runs `analysistools::create_analysis()` on a `srvyr` survey design,
#' - labels results using `presentresults`, builds a table with `presentresults::create_table_variable_x_group`,
#' - writes a per-group XLSX file with `presentresults::create_xlsx_variable_x_group()` (with a robust fallback),
#' - returns a named list of per-group results and a summary tibble.
#'
#' @param data A data.frame (or tibble) containing the cleaned survey dataset. Must contain the strata and weights columns
#'   referenced by `strata` and `weights_col`.
#' @param weights Logical, whether to use weights. If `TRUE` the column named by `weights_col` will be used.
#' @param weights_col Character, name of the weights column in `data` (default: "weights"). Set to NULL or leave parameter as default if weights not used.
#' @param loa Data.frame describing the list of analyses (LOA). Must include `group_var`, `analysis_var`, and `analysis_type`. Set to NULL or leave parameter as default if weights not used.
#' @param strata Character, name of the strata column to pass into `srvyr::as_survey_design()` (default: "sampling_framework").
#' @param sm_seperator Character of the select-multiple separator used by your dataset (default: ".").
#' @param questions The survey `survey` sheet (Kobo-style) used to create the label dictionary.
#' @param choices The survey `choices` sheet (Kobo-style) used to create the label dictionary.
#' @param label_column Character. The label column in the Kobo sheet to use (default: "label::english").
#' @param use_labels Binary for whether the results table should be made using labels or the raw names
#' @param out_dir Character. Directory to write per-group files into (created if missing).
#' @param file_prefix Character. Prefix used when naming per-group files (default: "Desc_Table").
#' @param value_columns Character vector. Columns to write from the presentresults table (default: c("stat","stat_upp","stat_low","n")).
#' @param overwrite Logical. Whether to overwrite existing per-group files and aggregated workbook.
#' @param aggregate Logical. Whether to create an aggregated workbook collecting all successful per-group sheets.
#' @param agg_filename Optional filename for the aggregated workbook. If `NULL`, a sensible default is used.
#'
#' @return A list with elements:
#' * `results`: named list of per-group result objects (one element per `loa$group_var`),
#' * `summary`: a tibble summarising success/write status and any errors,
#' * `aggregated_path`: character file path of the aggregated workbook, or `NULL` if not created.
#'
#' @details The function uses a robust write strategy: it first attempts to let
#' `presentresults::create_xlsx_variable_x_group()` write the per-group workbook. If
#' that does not create a file, it will try to save any returned `openxlsx::Workbook` object
#' or fall back to creating a workbook and writing the table directly.
#'
#' @export
#' @examples
#' \dontrun{
#' out <- multi_results_table_output(
#'   data = agg_data_weighted,
#'   weights = TRUE,
#'   weights_col = "weights",
#'   loa = loa,
#'   strata = "sampling_framework",
#'   sm_seperator = ".",
#'   questions = survey,
#'   choices = choices,
#'   out_dir = "results_tables",
#'   file_prefix = "HOVA_Aggregated_Desc_Table",
#'   overwrite = TRUE,
#'   aggregate = TRUE
#' )
#' }
multi_results_table_output <- function(data = NULL,
                                       weights = TRUE,
                                       weights_col = "weights",
                                       loa,
                                       strata = "sampling_framework",
                                       sm_seperator = ".",
                                       questions,
                                       choices,
                                       label_column = "label::english",
                                       use_labels = TRUE,
                                       out_dir = "results_tables",
                                       file_prefix = "Desc_Table",
                                       value_columns = c("stat", "stat_upp", "stat_low", "n"),
                                       overwrite = TRUE,
                                       aggregate = TRUE,
                                       agg_filename = NULL) {

  # ---- basic assertions ----
  if (is.null(data)) stop("`data` must be provided and be a data.frame/tibble.")
  if (missing(loa) || !("group_var" %in% names(loa))) stop("`loa` must be provided and contain a 'group_var' column.")

  # helper: safe file/tab name
  safe_name <- function(x) {
    if (is.na(x) || is.null(x)) return("Overall")
    x <- as.character(x)
    x <- janitor::make_clean_names(x)
    stringr::str_replace_all(x, "_+", "_")
  }

  fs::dir_create(out_dir)

  # distinct groups to iterate
  group_names <- loa %>%
    dplyr::pull(group_var) %>%
    unique() %>%
    purrr::discard(is.na)

  strata_sym <- if(!is.null(strata)) {rlang::sym(strata)} else {NULL}
  weights_sym <- if(!is.null(weights_col)) {rlang::sym(weights_col)} else {NULL}

  # robust write helper
  robust_write <- function(df_main_analysis_table, path, value_columns, overwrite) {
    fs::dir_create(dirname(path))

    p_res <- NULL
    p_warns <- character()
    withCallingHandlers({
      p_res <- tryCatch({
        presentresults::create_xlsx_variable_x_group(
          table_group_x_variable = df_main_analysis_table,
          file_path = path,
          value_columns = value_columns,
          overwrite = overwrite
        )
      }, error = function(e) {
        list(.error = TRUE, msg = conditionMessage(e))
      })
    }, warning = function(w) {
      p_warns <<- c(p_warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

    if (file.exists(path)) {
      return(list(ok = TRUE, path = as.character(path), warnings = p_warns))
    }

    if (!is.null(p_res) && inherits(p_res, "Workbook")) {
      try({
        openxlsx::saveWorkbook(p_res, file = path, overwrite = overwrite)
        if (file.exists(path)) return(list(ok = TRUE, path = as.character(path), warnings = p_warns, saved_workbook = TRUE))
      }, silent = TRUE)
    }

    tryCatch({
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, sheetName = "Results")
      openxlsx::writeData(wb, sheet = "Results", x = df_main_analysis_table)
      openxlsx::saveWorkbook(wb, file = path, overwrite = overwrite)
      list(ok = TRUE, path = as.character(path), warnings = p_warns, fallback = TRUE)
    }, error = function(e) {
      list(ok = FALSE, error = conditionMessage(e), warnings = p_warns, path = as.character(path))
    })
  }

  # per-group runner
  run_for_group <- function(g) {
    loa_g <- loa %>%
      dplyr::filter(is.na(group_var) | group_var == !!g) %>%
      dplyr::filter(!(group_var == analysis_var & !is.na(group_var)))

    #loa_g <- dplyr::bind_rows(loa_g %>% dplyr::mutate(group_var = NA), loa_g)

    # staged try/catch to surface stage-specific failures
    tryCatch({
      # survey design
      if (isTRUE(weights)) {
        survey_design <- data %>% srvyr::as_survey_design(strata = !!strata_sym, weights = !!weights_sym)
      } else {
        survey_design <- data %>% srvyr::as_survey_design(strata = !!strata_sym)
      }

      # heavy analysistools step
      survey_output <- analysistools::create_analysis(survey_design, loa = loa_g, sm_separator = sm_seperator)
      results_table <- survey_output$results_table

      kobo_label_review <- NULL

      if(use_labels) {

        kobo_label_review <- presentresults::review_kobo_labels(
          kobo_survey_sheet = questions,
          kobo_choices_sheet = choices,
          label_column = label_column,
          results_table = results_table
        )

        label_dictionary <- presentresults::create_label_dictionary(
          kobo_survey_sheet = questions,
          kobo_choices_sheet = choices,
          label_column = label_column,
          results_table = results_table
        )

        results_table_labeled <- presentresults::add_label_columns_to_results_table(results_table, label_dictionary)

        df_main_analysis_table <- presentresults::create_table_variable_x_group(
          analysis_key = "label_analysis_key",
          results_table = results_table_labeled,
          value_columns = value_columns
        )
      } else {
        df_main_analysis_table <- presentresults::create_table_variable_x_group(
          analysis_key = "analysis_key",
          results_table = results_table,
          value_columns = value_columns
        )
      }

      # NA handling
      df_main_analysis_table <- df_main_analysis_table %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.list), ~ purrr::map(.x, ~ ifelse(is.na(.x), list(NULL), .x)))) %>%
        dplyr::mutate(dplyr::across(dplyr::where(~ !is.list(.x) & is.numeric(.x)), ~ replace(.x, is.na(.x), NA))) %>%
        dplyr::mutate(dplyr::across(dplyr::where(~ !is.list(.x) & !is.numeric(.x)), ~ ifelse(is.na(.x), "NA", .x)))

      names(df_main_analysis_table) <- stringr::str_replace_all(names(df_main_analysis_table), "NA", "Overall")

      fname <- glue::glue("{file_prefix}__{safe_name(g)}.xlsx")
      path <- fs::path(out_dir, fname)

      write_result <- robust_write(df_main_analysis_table, path, value_columns = value_columns, overwrite = overwrite)

      list(
        ok = TRUE,
        group = g,
        safe_name = safe_name(g),
        table = df_main_analysis_table,
        write = write_result,
        results_table = results_table,
        kobo_label_review = kobo_label_review
      )

    }, error = function(e) {
      list(ok = FALSE, group = g, safe_name = safe_name(g), error = conditionMessage(e))
    })
  }

  # run
  results_list <- group_names %>% set_names(purrr::map_chr(., ~ safe_name(.x))) %>% purrr::map(run_for_group)

  summary_df <- purrr::map_dfr(results_list, function(x) {
    if (isTRUE(x$ok)) {
      tibble::tibble(
        group = x$group,
        safe_name = x$safe_name,
        success = TRUE,
        write_ok = if (!is.null(x$write)) x$write$ok else NA,
        path = if (!is.null(x$write)) x$write$path else NA,
        error = NA_character_
      )
    } else {
      tibble::tibble(
        group = x$group,
        safe_name = x$safe_name,
        success = FALSE,
        write_ok = FALSE,
        path = NA_character_,
        error = x$error
      )
    }
  })

  # aggregation
  agg_path <- NULL
  if (isTRUE(aggregate)) {
    if (is.null(agg_filename)) {
      agg_filename <- glue::glue("{file_prefix}_aggregated_results_tables.xlsx")
    }

    written_files <- purrr::map_chr(results_list, function(x) {
      if (isTRUE(x$ok) && !is.null(x$write) && isTRUE(x$write$ok) && file.exists(x$write$path)) {
        x$write$path
      } else {
        NA_character_
      }
    }) %>% na.omit()

    if (length(written_files) == 0) {
      message("No per-group files to aggregate (no successful writes). Skipping aggregation.")
    } else {
      sheet_names <- purrr::map_chr(results_list, "safe_name")[names(written_files)]
      names(written_files) <- sheet_names

      agg_wb <- openxlsx::createWorkbook()

      purrr::imap(written_files, function(path, sheet_name) {
        sheet_title <- substr(sheet_name, 1, 31)
        if (sheet_title %in% openxlsx::sheets(agg_wb)) {
          suffix <- 1
          new_title <- paste0(sheet_title, "_", suffix)
          while (new_title %in% openxlsx::sheets(agg_wb)) {
            suffix <- suffix + 1
            new_title <- paste0(sheet_title, "_", suffix)
          }
          sheet_title <- substr(new_title, 1, 31)
        }

        openxlsx::addWorksheet(wb = agg_wb, sheetName = sheet_title)
        n_sheets <- length(readxl::excel_sheets(path))
        sheet_to_read <- if (n_sheets >= 2) 2 else 1
        rt_data <- readxl::read_excel(path, sheet = sheet_to_read)
        openxlsx::writeData(wb = agg_wb, sheet = sheet_title, x = rt_data)

        if (exists("presentresults_simple_style", mode = "function")) {
          try(presentresults_simple_style(agg_wb, sheet = sheet_title, results_df = rt_data), silent = TRUE)
        }
      })

      agg_path <- fs::path(out_dir, agg_filename)
      openxlsx::saveWorkbook(agg_wb, agg_path, overwrite = overwrite)
      message("Aggregated workbook saved to: ", agg_path)
    }
  }

  list(results = results_list, summary = summary_df, aggregated_path = if (!is.null(agg_path)) as.character(agg_path) else NULL)
}
