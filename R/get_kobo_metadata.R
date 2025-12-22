#' @title Get Kobo Interview Duration
#'
#' @description
#' Calculates interview duration using KoboToolbox metadata logs and merges
#' this information with your dataset.
#'
#' @param dataset A data frame containing your Kobo survey data.
#' @param remove_geo Logical. If TRUE (default), durations related to GPS capture are excluded.
#'
#' @return
#' A list with:
#' \describe{
#'   \item{df_and_duration}{The original dataset with added interview duration and metadata start time.}
#'   \item{audit_files_length}{The parsed audit log with duration calculations.}
#' }
#'
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import httr2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_kobo_metadata(dataset = data_in_processing, asset_id = "abc123XYZ", un = "my_kobo_username")
#' }
get_kobo_metadata <- function(dataset,
                              un,
                              server_url = "https://kobo.impact-initiatives.org/",
                              reset_password = F,
                              remove_geo = TRUE) {

  ## validate if dataset exists


  if (missing(dataset) || is.null(dataset) || !is.data.frame(dataset)) {
    stop("Please provide a valid data frame to the `dataset` argument.")
  }

  if (!"_attachments" %in% colnames(dataset)) {
    stop("Need to have the attachments column returned through the API query")
  }

  if (!"uuid" %in% colnames(dataset)) {
    stop("uuid column not in dataset")
  }

  if (!is.list(dataset$`_attachments`)) {
    stop("`_attachments` must be a list column returned by Kobo API.")
  }

  if (isTRUE(reset_password)) {
    keyring::key_delete(un)
  }

  tryCatch({
    keyring::key_get(un)
  }, error = function(e) {
    keyring::key_set(un, prompt = "Please input your Kobo password. You will only have to do this once.")
    keyring::key_get(un)
  })



  pw <- get_password(un)

  clean_url <- sub("\\/$", "", server_url)

  token <- robotoolbox::kobo_token(username = un, password = pw, url = clean_url)

  read_audit_csv <- function(url, submission_id) {

    req <- request(url) %>%
      req_headers(Authorization = paste("Token", token))

    resp <- req_perform(req)

    raw <- resp_body_raw(resp)

    df <- readr::read_csv(raw, show_col_types = FALSE)

    # Ensure columns exist, otherwise add them
    #needed_cols <- c("old-value", "new-value", "old_value", "new_value")
    #present <- intersect(names(df), needed_cols)

    if (!("old_value" %in% names(df))) {df$old_value <- NA_character_}
    if (!("new_value" %in% names(df))) {df$new_value <- NA_character_}

    df <- df %>%
      mutate(
        old_value = coalesce(
          as.character(old_value),
          as.character(`old-value`)
        ),
        new_value = coalesce(
          as.character(new_value),
          as.character(`new-value`)
        )
      ) %>%
      select(-any_of(c("old-value", "new-value")))


    # Ensure character type
    df <- df %>%
      mutate(
        old_value = as.character(old_value),
        new_value = as.character(new_value),
        submission_id = submission_id
      ) %>%
      rename(uuid = submission_id)
    df
  }

  attachments_expanded <- map2_df(
    dataset$`_attachments`,
    dataset$uuid,
    ~ if (!is.null(.x)) {
      mutate(.x, submission_id = .y)
    } else {
      NULL
    }
  )

  audit_files <- attachments_expanded %>%
    filter(media_file_basename == "audit.csv")

  audit_all <- map2_dfr(
    audit_files$download_url,
    audit_files$submission_id,
    read_audit_csv
  )

  if (remove_geo) {
    audit_all <- audit_all %>%
      dplyr::filter(!stringr::str_detect(node, "geo"))
  }

  audit_summary <- audit_all %>%
    dplyr::mutate(metadata_duration = (end - start) / 60000) %>%
    group_by(uuid) %>%
    dplyr::summarise(
      interview_duration = sum(metadata_duration, na.rm = TRUE),
      .groups = "drop"
    )

  dataset_with_audit <- dataset %>%
    left_join(audit_summary)

  return(list(
    df_and_duration = dataset_with_audit,
    audit_files_length = audit_all
  ))
}

