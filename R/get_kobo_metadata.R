#' @title Get Kobo Interview Duration from Metadata Logs
#'
#' @description
#' Calculates interview duration using KoboToolbox metadata logs and merges
#' this information with your dataset. Only the Kobo username is required;
#' password is securely stored using the `keyring` package.
#'
#' @param dataset A data frame containing your Kobo survey data.
#' @param asset_id A character string specifying the Kobo asset ID (found in the survey URL).
#' @param un Your Kobo username. Used to retrieve a stored password and fetch an API token.
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
#' @import robotoolbox
#' @import stringr
#' @import keyring
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_kobo_metadata(dataset = data_in_processing, asset_id = "abc123XYZ", un = "my_kobo_username")
#' }
get_kobo_metadata <- function(dataset, asset_id, un, remove_geo = TRUE) {

  ## validate if dataset exists

  if (missing(dataset) || is.null(dataset) || !is.data.frame(dataset)) {
    stop("Please provide a valid data frame to the `dataset` argument.")
  }

  # Try to retrieve password from keyring
  pw <- tryCatch({
    keyring::key_get(un)
  }, error = function(e) {
    keyring::key_set(un, prompt = "Please input your Kobo password. You will only have to do this once.")
    keyring::key_get(un)
  })

  message("Authenticating with Kobo server...")


  # Get token and setup Kobo API
  kobo_server_url <- "https://kobo.impact-initiatives.org/"
  kobo_token_value <- robotoolbox::kobo_token(
    username = un,
    password = pw,
    url = sub("\\/$", "", kobo_server_url)
  )

  robotoolbox::kobo_setup(
    url = sub("\\/$", "", kobo_server_url),
    token = kobo_token_value
  )

  message("Authenticated...")

  # Retrieve and process audit files
  audit_files <- robotoolbox::kobo_audit(x = asset_id, progress = TRUE)

  audit_files_length <- audit_files %>%
    dplyr::mutate(metadata_duration = (end_int - start_int) / 60000)

  if (remove_geo) {
    audit_files_length <- audit_files_length %>%
      dplyr::filter(!stringr::str_detect(node, "geo"))
  }

  iv_lengths <- audit_files_length %>%
    dplyr::group_by(`_id`) %>%
    dplyr::summarise(
      interview_duration = sum(metadata_duration, na.rm = TRUE),
      start_time_metadata = first(start),
      .groups = "drop"
    )

  data_in_processing <- dataset %>%
    dplyr::left_join(iv_lengths, by = "_id")

  return(list(
    df_and_duration = data_in_processing,
    audit_files_length = audit_files_length
  ))
}

