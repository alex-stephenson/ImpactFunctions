get_password <- function(un) {
  tryCatch({
    # Attempt to get the password
    password <- keyring::key_get(un)
  }, error = function(e) {
    # If there is an error (key doesn't exist), handle it here
    keyring::key_set(un, prompt = "Please input your Kobo password. You will only have to do this once")
  })
}


#' Kobo Server Query function
#' A function that allows you to retrieve kobo data from the server by providing the asset ID
#' @param asset_id What is the asset ID of the survey youre interested in?
#' @export
#' @examples #kobo_data <- get_kobo_data(asset_id = "a38DR4AhAH2rDqP6e5YHLm") # TEST SURVEY

get_kobo_data <- function(asset_id){


  if (!require("keyring", quietly = TRUE)) install.packages("keyring")
  if (!require("robotoolbox")) remotes::install_gitlab("dickoa/robotoolbox")
  library(keyring)
  library(robotoolbox)


  un = readline(prompt = "Please input your Koob server username: ")
  pw = get_password(un)
  kobo_server_url <- "https://kobo.impact-initiatives.org/"
  kobo_token_value <- kobo_token(username = un, password = pw, url = sub("\\/$","", kobo_server_url) )
  kobo_setup(url = sub("\\/$","", kobo_server_url), token = kobo_token_value)

  form_uid <- asset_id

  asset <- kobo_asset(form_uid)
  tmp_kobolist <- kobo_data(asset, select_multiple_sep = "/")

  tmp_kobolist

}

##kobo_data <- get_kobo_data(asset_id = "a38DR4AhAH2rDqP6e5YHLm") # TEST SURVEY
##kobo_data <- get_kobo_data(asset_id = "antAdT3siLrnjTdfcdYcFY")
