# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)


## wrap it in a function to do 

process_cleaning_log <- function(tool_path, 
                                 survey_sheet = "survey", 
                                 choice_sheet = "choices", 
                                 sample_survey_data_path,
                                 sample_survey_data_path_sheet = NULL,
                                 file_path, 
                                 output_dir,
                                 output_as_list = FALSE,
                                 output_as_excel = TRUE) {

  "%!in%" <- Negate("%in%")
  
  stopifnot(file.exists(tool_path), file.exists(file_path), dir.exists(output_dir))

  ## start by preparing the options available in the tool for both select one and select multiple
  
  cat("Loading Kobo tool from:", tool_path, "\n\n")
  
  kobo_tool_name <- tool_path
  
  kobo_survey <- tryCatch(read_excel(tool_path, sheet = survey_sheet, .name_repair = "unique_quiet"),
                          error = function(e) stop("Error reading survey sheet: ", e))
  kobo_choice <- tryCatch(read_excel(tool_path, sheet = choice_sheet, .name_repair = "unique_quiet"),
                          error = function(e) stop("Error reading choices sheet: ", e))
  
  
  
  kobo_survey_sm <- kobo_survey %>%
    filter(str_detect(type, "multiple")) %>%
    pull(name)
  
  kobo_survey_one <- kobo_survey %>%
    filter(str_detect(type, "select_one")) %>%
    select(type, name) %>%
    mutate(list_name = str_remove_all(type, "select_one "))
  
  kobo_survey_one_list <- kobo_survey_one %>%
    dplyr::rename(survey_name = name) %>%
    left_join(kobo_choice, by = join_by("list_name" == "list_name"), relationship = "many-to-many") %>%
    select(survey_question = survey_name, survey_answer = name) %>%
    mutate(survey_options = paste0(survey_question, "/", survey_answer)) ## get all of the list + answer combinations, for subsequent checking
  
  q_list <- kobo_survey_one_list %>%
    select(survey_question) %>%
    distinct() %>%
    pull(survey_question)
  
  q_list_answers <- kobo_survey_one_list %>%
    select(survey_options) %>%
    distinct() %>%
    pull(survey_options)
  
  if (length(kobo_survey_sm) == 0) warning("No 'select_multiple' questions found")
  if (length(kobo_survey_one_list) == 0) warning("No 'select_one' questions found")
  

  
  # Extract the file name for tagging output
  
  
  cat("Reading cleaning log from:", file_path, "\n\n")

  clog_name <- basename(file_path)
  
  # Read the cleaning log
  cleaning_log <- tryCatch(read_excel(file_path, sheet = "cleaning_log"),
                           error = function(e) stop("Error reading cleaning log sheet: ", e)) %>%
    mutate(new_value = str_replace_all(new_value, "  ", " "),
           old_value = str_replace_all(old_value, "  ", " "))
  
  
  # Debugging: Validate cleaning log structure
  required_columns <- c("question", "new_value", "old_value", "change_type", "check_binding")
  missing_cols <- setdiff(required_columns, colnames(cleaning_log))
  if (length(missing_cols) > 0) stop("Cleaning log is missing required columns: ", paste(missing_cols, collapse = ", "))
  
  
  # Action check
  action_check <- cleaning_log %>%
    mutate(
      clog = clog_name,
      clog_issue = case_when(
        new_value == old_value & change_type == "no_action" ~ "Correct",
        new_value != old_value & change_type == "change_response" ~ "Correct",
        new_value == old_value & change_type == "change_response" ~ "The new value and the old value are the same but the action type is change_response",
        new_value != old_value & change_type == "no_action" ~ "The new value and the old value are different but the action is no_action",
        TRUE ~ NA
      )
    ) %>%
    filter(clog_issue != "Correct")
  
  cat("Action check issues found:", nrow(action_check), "\n\n")
  
  # Parent question validation
  parent_questions <- cleaning_log %>%
    
    filter(question %in% kobo_survey_sm,
           !str_detect(question, "/")) %>%
    mutate(
      added_options = map2_chr(
        str_split(new_value, "\\s+"),
        str_split(old_value, "\\s+"),
        ~ paste(setdiff(.x, .y), collapse = " ")
      )
    ) %>%
    filter(added_options != "")
  
  validation_results <- parent_questions %>%
    rowwise() %>%
    mutate(
      valid_children = ifelse( ## column we will use to see if a child is valid
        added_options == "", ## if no added options then we skip
        TRUE,
        all(
          str_split(added_options, "\\s+")[[1]] %>% ## split each added option into one
            map_lgl(~ { # use map_lgl to check each option 
              child_question <- paste0(question, "/", .x) # create a child_question value that will create the child questions we would expect to see, separated by /
              child_exists <- cleaning_log %>% ## now filter the cleaning log dataset to see where the the binding id and newly constructed child options are the same
                filter(
                  question == child_question,
                  check_binding == check_binding
                ) %>%
                nrow() > 0 ## if there is a match then child exists, otherwise it is FALSE (child does not exist in child rows)
              child_exists
            })
        )
      )
    ) %>%
    ungroup()
  
  summary_report <- validation_results %>%
    filter(valid_children == FALSE) %>%
    mutate(clog_issue = paste0(added_options, " is added in the clog but the corresponding score is not changed from 0 to 1."))
  
  
  cat("Answers with select multiple child questions found:", nrow(summary_report), "\n\n")
  
  
  # Check whether the questions are valid -- select multiple
  
  colnames = colnames(read_excel(sample_survey_data_path, n_max = 3))
  
  child_q_check <- cleaning_log %>%
    filter(question %in% kobo_survey_sm)
  
  individual_rows <- child_q_check %>%
    filter(!str_detect(question, "/"),
           change_type == "change_response") %>%
    separate_rows(new_value, sep = "\\s+") %>%
    filter(new_value != "") %>%
    mutate(question_answer = paste0(question, "/", new_value)) %>%
    filter(question_answer %!in% colnames) %>%
    mutate(clog_issue = "This option is not in the tool [select multiple]")
  
  
  # Check whether the questions are valid -- select one

  select_single_qs <- cleaning_log %>% 
    filter(question %in% q_list,
           change_type == "change_response") %>%
    mutate(question_answer = paste0(question, "/", new_value)) %>%
    filter(question_answer %!in% q_list_answers) %>%
    mutate(clog_issue = "This option is not in the tool [select one]")
  
  tool_answer_check <- rbind(individual_rows, select_single_qs)
  
  cat("Answers not found in the tool options:", nrow(summary_report), "\n\n")
  
  
  if(output_as_excel == TRUE) {
  
  # Write to Excel
  output_path <- file.path(output_dir, paste0("cleaning_log_report_", clog_name))
  wb <- createWorkbook()
  addWorksheet(wb, "Action Check")
  writeData(wb, "Action Check", action_check)
  addWorksheet(wb, "Tool answer check")
  writeData(wb, "Tool answer check", tool_answer_check)
  addWorksheet(wb, "Child answer check")
  writeData(wb, "Child answer check", summary_report)
  saveWorkbook(wb, output_path, overwrite = TRUE)
  
  cat("Processed:", clog_name, "-> Output:", output_path, "\n\n")
  } 
  
  if (output_as_list == TRUE) {
  return(list(action_check = action_check, tool_answer_check = tool_answer_check, child_answer_check = summary_report))
  }
}



## DSA VIII Test
process_cleaning_log(tool_path = r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 02_Research\01_REACH\2024_25\03_DDSU\SOM2204 _DSA VIII 2025\02_Data_Collection\01_Tool/REACH_2024_SOM_DSA_Survey_Tool_VIII.xlsx)",
                     sample_survey_data_path = r"(C:\Users\alex.stephenson\Downloads/DSA_2024_REACH_SOM_Processed_Data_2024-12-24.xlsx)",
                     file_path = r"(C:\Users\alex.stephenson\Downloads/cleaning_log_Suleiman_Mohamed_Nov_18_2024_092434F.xlsx)",
                     output_dir = r"(C:\Users\alex.stephenson\Downloads)",
                     output_as_list = TRUE,
                     output_as_excel = FALSE)



## apply to all files in a directory:

# Define the directories
input_dir <- r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 01_REACH\2024_25\01_ISU\SOM1901_HSM\03_Data & Data Analysis\DEC_24\01CLeaning scripts\04_data_cleaning\hassan - Clogs\Finished_Clogs)"  
output_dir <- r"(C:\Users\alex.stephenson\Downloads/example_clogs_report)"  # Replace with the desired output directory

all_outputs <- list()

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List all Excel files in the input directory
excel_files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Iterate over each Excel file and process it
for (file_path in excel_files) {

  tryCatch({
    checks_output <- process_cleaning_log(
      tool_path = r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 01_REACH\2024_25\01_ISU\SOM1901_HSM\03_Data & Data Analysis\DEC_24\00_tool/SOM_REACH_HSM_December_2024_Tool_Pilot_ver_AS_EDITS_v2.xlsx)",  # Replace with the path to the tool file
      survey_sheet = "survey", 
      choice_sheet = "choices",
      sample_survey_data_path = r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 01_REACH\2024_25\01_ISU\SOM1901_HSM\03_Data & Data Analysis\DEC_24\01CLeaning scripts\_code\outputs\dashboard/unchecked_data_for_dashboard_Dec_19_2024_082211.xlsx)",  # Replace with the sample survey data path
      sample_survey_data_path_sheet = NULL,
      file_path = file_path,
      output_dir = output_dir,
      output_as_excel = FALSE,
      output_as_list = TRUE
    )
  }, error = function(e) {
    cat("Error processing file:", file_path, "\nError message:", e$message, "\n")
  })
  
  #all_outputs <- append(all_outputs, list(checks_output))
  all_outputs[[basename(file_path)]] <- checks_output
  
}








         
