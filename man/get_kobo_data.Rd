% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_kobo_api.R
\name{get_kobo_data}
\alias{get_kobo_data}
\title{Kobo Server Query Function}
\usage{
get_kobo_data(
  asset_id,
  un,
  server_url = "https://kobo.impact-initiatives.org/"
)
}
\arguments{
\item{asset_id}{Character. The asset ID of the survey.}

\item{un}{Character. Your Kobo username.}

\item{server_url}{Character. Kobo server URL (default: "https://kobo.impact-initiatives.org/").}
}
\value{
A data frame of the Kobo survey results. If the survey includes roster questions then it is a list of dataframes, \code{main} and \code{hh_roster}.
}
\description{
Retrieve Kobo data from the server by providing the asset ID.
}
