#' Update Key Informant (KI) Database
#'
#' This function appends new data to an existing Key Informant (KI) database, formats the data, and saves it as an Excel file.
#' The function includes the file path for the KI database.
#' The function includes parameters for date, district, region, IDP site, name, age, role, status, contact information, and field officer.
#' It should be run for all RCs that use KI data.
#' The parameters for the function are used to rename the columns returned by the survey into the standard data model for the KI database, making it possible to append data from different RCs.
#'
#'
#' @param data A data frame containing the new data to be added.
#' @param date Column name (string) for the date of data collection.
#' @param district Column name (string) for the district.
#' @param region Column name (string) for the region.
#' @param idp_site Column name (string) for the IDP site.
#' @param ki_name Column name (string) for the Key Informant's name.
#' @param ki_age Column name (string) for the Key Informant's age.
#' @param ki_role Column name (string) for the Key Informant's role.
#' @param ki_status Column name (string) for the Key Informant's status.
#' @param ki_contact Column name (string) for the Key Informant's contact information.
#' @param FieldOfficer Column name (string) for the responsible field officer.
#' @param r_c A string to categorize the research category.
#'
#' @return An updated data frame combining the original KI database and new data.
#' @export
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select rename mutate bind_rows distinct
#' @importFrom openxlsx createWorkbook addWorksheet writeData addFilter setColWidths createStyle addStyle saveWorkbook
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'
#' update_ki_database(
#'   data = df,
#'   date = "today",
#'   district = "district_name",
#'   region = "localisation_region_label",
#'   idp_site = "idp_site",
#'   ki_name = "ki_name",
#'   ki_age = "ki_age",
#'   ki_role = "ki_role",
#'   ki_status = "ki_status",
#'   ki_contact = "ki_contact",
#'   FieldOfficer = "Responsible_FO",
#'   r_c = "DSA"
#' }



update_ki_database <- function(data, date, district, region, idp_site, ki_name, ki_age, ki_role, ki_status, ki_contact, FieldOfficer, r_c) {

  # Get current user login and read existing KI database
  user_login <- Sys.info()[["user"]]
  current_db_str <- sprintf(r"(C:\Users\%s\ACTED\IMPACT SOM - 02_Research\01_REACH\Data Team\10_Common files\KI_Database\KI_Contact_List.xlsx)", user_login)
  current_KI_db <- readxl::read_xlsx(current_db_str)

  # Select and rename columns in the new data
  df_standard_names <- data %>%
    dplyr::select(
      date = !!rlang::sym(date),
      district = !!rlang::sym(district),
      region = !!rlang::sym(region),
      idp_site = !!rlang::sym(idp_site),
      name = !!rlang::sym(ki_name),
      age = !!rlang::sym(ki_age),
      role = !!rlang::sym(ki_role),
      status = !!rlang::sym(ki_status),
      contact = !!rlang::sym(ki_contact),
      field_officer = !!rlang::sym(FieldOfficer)
    ) %>%
    dplyr::mutate(research = r_c)

  # Append new data to current KI database and remove duplicates
  appended_db <- current_KI_db %>%
    dplyr::bind_rows(df_standard_names) %>%
    dplyr::distinct(district, region, idp_site, name, age, role, status, contact, field_officer, research, .keep_all = TRUE)

  # Create and format the Excel workbook
  KI_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(KI_wb, "KI_database")
  openxlsx::writeData(KI_wb, sheet = "KI_database", x = appended_db)
  openxlsx::addFilter(KI_wb, sheet = "KI_database", row = 1, cols = 1:ncol(appended_db))
  openxlsx::setColWidths(KI_wb, sheet = "KI_database", cols = 1:ncol(appended_db), widths = "auto")

  # Style the header row
  headerStyle <- openxlsx::createStyle(
    fontSize = 12, fontColour = "#FFFFFF", fgFill = "#4F81BD",
    halign = "center", textDecoration = "bold"
  )
  openxlsx::addStyle(KI_wb, sheet = "KI_database", style = headerStyle, rows = 1, cols = 1:ncol(appended_db), gridExpand = TRUE)

  # Save the workbook
  openxlsx::saveWorkbook(KI_wb, sprintf(r"(C:\Users\%s\ACTED\IMPACT SOM - 02_Research\01_REACH\Data Team\10_Common files\KI_Database\KI_Contact_List.xlsx)", user_login), overwrite = TRUE)
  print(paste0("Saved to file: ", current_db_str))

  return(appended_db)

}




# update_ki_database(
#   data = df,
#   date = "today",
#   district = "district_name",
#   region = "localisation_region_label",
#   idp_site = "idp_site",
#   ki_name = "ki_name",
#   ki_age = "ki_age",
#   ki_role = "ki_role",
#   ki_status = "ki_status",
#   ki_contact = "ki_contact",
#   FieldOfficer = "Responsible_FO",
#   r_c = "DSA"
# )








