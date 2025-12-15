# Securely retrieve or prompt for Kobo password
get_password <- function(un) {
  if (missing(un) || !is.character(un) || nchar(un) == 0) {
    stop("Please provide a valid Kobo username to `un`.")
  }

  tryCatch({
    keyring::key_get(un)
  }, error = function(e) {
    keyring::key_set(un, prompt = "Please input your Kobo password. You will only have to do this once.")
    keyring::key_get(un)
  })
}



#' Kobo Server Query Function
#'
#' Retrieve Kobo data from the server by providing the asset ID.
#' @param asset_id Character. The asset ID of the survey.
#' @param un Character. Your Kobo username.
#' @param sm_sep Delimiter to use for select multiple options. Defaults to "/"
#' @param server_url Character. Kobo server URL (default: "https://kobo.impact-initiatives.org/").
#' @param reset_password Use parameter if you have previously input an incorrect password and want to update it.
#' @return A data frame of the Kobo survey results. If the survey includes roster questions then it is a list of dataframes, `main` and `hh_roster`.
#' @export
get_kobo_data <- function(asset_id,
                          un,
                          sm_sep = "/",
                          server_url = "https://kobo.impact-initiatives.org/",
                          reset_password = F) {
  if (missing(asset_id) || !nzchar(asset_id)) {
    stop("Please provide a valid asset ID.")
  }
  if (missing(un) || !nzchar(un)) {
    stop("Please provide your Kobo username (`un`).")
  }

  if (isTRUE(reset_password)) {

    keyring::key_delete(un)
    }

  message("Authenticating with Kobo server...")


  pw <- get_password(un)


  clean_url <- sub("\\/$", "", server_url)

  token <- robotoolbox::kobo_token(username = un, password = pw, url = clean_url)

  robotoolbox::kobo_setup(url = clean_url, token = token)

  message("Authenticated...")

  asset <- robotoolbox::kobo_asset(asset_id)
  robotoolbox::kobo_data(asset, progress = T, select_multiple_sep = sm_sep)
}
