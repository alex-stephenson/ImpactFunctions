get_password <- function(un) {
  tryCatch({
    password <- keyring::key_get(un)
  }, error = function(e) {
    keyring::key_set(un, prompt = "Please input your Kobo password. You will only have to do this once")
  })
}

#' Kobo Server Query Function
#'
#' Retrieve Kobo data from the server by providing the asset ID.
#' @param asset_id Character. The asset ID of the survey.
#' @param un Character. Your Kobo username.
#' @return A data frame of the Kobo survey results.
#' @export
get_kobo_data <- function(asset_id, un) {
  pw <- get_password(un)

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

  asset <- robotoolbox::kobo_asset(asset_id)

  robotoolbox::kobo_data(asset, select_multiple_sep = "/")
}
