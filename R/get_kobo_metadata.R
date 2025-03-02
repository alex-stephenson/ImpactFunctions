#' @title Get Kobo Metadata Duration
#' @description Adds the duration of each interview as a column to your dataset based on the metadata logs
#' @param dataset the name of your survey dataframe.
#' @param asset_id the id of your kobo object. Can be taken from your survey URL.
#' @param kobo_token your kobo API key. Optional in case it is already set as a system variable.
#' @param remove_geo whether to remove time taken to get gps coordinates from the summary. Defaults to TRUE.
#' @import dplyr
#' @import robotoolbox
#' @import stringr
#' @export
#' @examples get_kobo_metadata(dataset = data_in_processing, asset_id = "antAdT3siLrnjTdfcdYcFY")


get_kobo_metadata <- function(dataset, asset_id, kobo_token = NULL, remove_geo = TRUE) {

  if (is.na(kobo_settings_output$token)) {
    if (is.null(kobo_token)) {
      stop("Please provide your Kobo API key to kobo_token parameter")
    }
    kobo_setup(url = "https://kobo.impact-initiatives.org", token = kobo_token)
  }

  audit_files <- robotoolbox::kobo_audit(x = asset_id, progress = T)

  audit_files_length <- audit_files %>%
    dplyr::mutate(metadata_duration = (end_int - start_int ) / 60000)

  if (remove_geo) {
    audit_files_length <- audit_files_length %>%
    dplyr::filter(!stringr::str_detect(node, "geo"))
  }

  iv_lengths <- audit_files_length %>%
    dplyr::group_by(`_id`) %>%
    dplyr::summarise(interview_duration = sum(metadata_duration, na.rm = T),
                     start_time_metadata = first(start))

  data_in_processing <- dataset %>%
    dplyr::left_join(iv_lengths)

  return(data_in_processing)
}
