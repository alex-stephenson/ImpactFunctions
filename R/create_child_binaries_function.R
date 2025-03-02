# rm(list = ls())
#
#
# tool_path =  r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 01_REACH\2024_25\01_ISU\SOM1901_HSM\03_Data & Data Analysis\DEC_24\00_tool\SOM_REACH_HSM_December_2024_Tool_Pilot_ver_AS_EDITS_v2.xlsx)"
#
# # importing survey as we only need to run these checks for select_multiple questions
# kobo_survey <- read_excel(tool_path, sheet = "survey")
# kobo_choice <- read_excel(tool_path, sheet = "choices")
#
#
# fo_in_charge <- "kala"
# fo_finished_clogs_path  <- paste0(r"(C:\Users\alex.stephenson\ACTED\IMPACT SOM - 01_REACH\2024_25\01_ISU\SOM1901_HSM\03_Data & Data Analysis\DEC_24\01CLeaning scripts\04_data_cleaning\kala - Clogs\Finished_Clogs\)")
# fo_finished_clogs_file_names <- setdiff(list.files(path = fo_finished_clogs_path), list.dirs(path = fo_finished_clogs_path, recursive = FALSE, full.names = FALSE))
# # rbind all the clogs into a big dataframe
# finished_clogs <- fo_finished_clogs_file_names %>%
#   map_dfr(~read_excel(paste0(fo_finished_clogs_path, .x), sheet = 'cleaning_log'))
#


create_sm_clogs <- function(
  clog_data,
  kobo_survey,
  kobo_choice
  ){

    ## STEP 1 - WE SHOULD WORK OUT IF THERE ARE MULTIPLE CLOGS FOR THE SAME SELECT MULTIPLE QUESTIONS AND COMBINE THE ANSWERS

  finished_clogs <- clog_data %>%
    dplyr::rename(orig_new_value = 'new_value') %>%
    mutate('new_value' = "") %>%
    mutate(uuid_question_id = paste0(uuid, "/", question)) %>%
    left_join(kobo_survey, by = c("question" = "name"))

    ## find duplicates
    uuid_question_count <- finished_clogs %>%
      group_by(uuid_question_id) %>%
      dplyr::summarise(instances = n())

    ## add the count so we can filter, and also select only the SM questions
    clogs_in_processing <- finished_clogs %>%
      left_join(uuid_question_count, by = "uuid_question_id") %>%
      filter(str_detect(question,"/") == FALSE, str_detect(type, "select_multiple")) %>%
      select(uuid, question, type, old_value, change_type, orig_new_value, uuid_question_id, instances)


    ## this aggregates the new value options if there are multiple and should make sure they are distinct.
    ## the way I've approached this is to group by the question, pivot it into longer rows based on a space delimiter, remove duplicates and the group by
    ## to basically rebuild the new_value
    updated_new_value <- clogs_in_processing %>%
      filter(instances > 1) %>% ## only do this for the duplicates
      mutate(orig_new_value = str_squish(orig_new_value)) %>%
      group_by(uuid_question_id) %>%
      separate_longer_delim(orig_new_value, delim = " ") %>%
      filter(!is.na(orig_new_value)) %>%
      distinct() %>%
      ungroup() %>%
      group_by(uuid_question_id) %>%
      summarise(new_value_merge = paste0(orig_new_value, collapse = " "))


    ## now lets add these new values back in to the original question data. We need to do this so that so we have the question name etc
    amended_new_value <- finished_clogs %>%
      left_join(updated_new_value) %>%
      filter(str_detect(question,"/") == FALSE, str_detect(type, "select_multiple")) %>%
      mutate(new_value = ifelse(is.na(new_value_merge), orig_new_value, new_value_merge))

    ## STEP 2 -- IF ANY OF THE QUESTIONS HAVE A MUTUALLY EXCLUSIVE CHOICE, WE SHOULD REMOVE THE OTHER OPTIONS
    ## for this, I've written it to identify the mutually exclusive options from the constraints

    nullif_selected_new_values <- amended_new_value %>%
      mutate(constraint_var = str_extract_all(constraint, "‘([^’]+)’"),
             constraint_var = map(constraint_var, ~ paste(.x, collapse = ", ")),
             constraint_var = str_replace_all(constraint_var, ('’|‘'), ""))


    ## now if there is one of these values in the new_values column, we need to remove all the other text
    nullif_selected_new_values <- nullif_selected_new_values %>%
      rowwise() %>%
      mutate(
        # Split new_value and constraint_var into lists of words
        new_value_list = list(str_split(new_value, " ")[[1]]), # Use list() to store the vector
        constraint_list = list(str_split(constraint_var, ", ")[[1]]), # Use list() for constraint_var
        # Check if any exclusive options are present in new_value
        exclusive_present = any(unlist(new_value_list) %in% unlist(constraint_list)),
        # If exclusive option is present, retain only exclusive values
        new_value_cleaned = if (exclusive_present) {
          paste(intersect(unlist(new_value_list), unlist(constraint_list)), collapse = " ")
        } else {
          new_value
        }
      ) %>%
      ungroup() %>%
      select(-new_value_list, -constraint_list, -exclusive_present)

    ## now lets make the final clog output for these 'parent' questions

    final_concat_new_values <- nullif_selected_new_values %>%
      select(uuid, question, new_value = new_value_cleaned)

    ## STEP 3 -- CREATE CHILD BINARIES BASED ON THE NEW VALUE DATA, FIRST WE WILL DO THE POSITIVES (IE SHOULD BE NEW VALUE 1)

    updated_positive_child_binaries <- amended_new_value %>%
      filter(str_detect(question,"/") == FALSE, str_detect(type, "select_multiple")) %>%
      select(uuid, question, type, old_value, new_value, change_type, uuid_question_id) %>%
      mutate(new_value = str_squish(new_value)) %>%
      separate_longer_delim(new_value, delim = " ") %>%
      mutate(question = paste0(question, "/", new_value),
             new_value = 1) %>%
      select(uuid, question, new_value)


    ## STEP 4 - NOW WE NEED TO MAKE THE NEGATIVE CHILD BINARIES - IE IF IT WAS IN THE OLD VALUE AND NO LONGER IN THE NEW VALUE WE
    ## SHOULD ALSO TURN IT FROM 1 TO 0.

    # the way I've approached this is similar, just with some extra steps. We need to do the split and re-merge
    # to get the duplicate questions.


    old_and_new_values <- finished_clogs %>%
      filter(str_detect(question,"/") == FALSE, str_detect(type, "select_multiple")) %>%
      select(uuid_question_id, old_value) %>%
      group_by(uuid_question_id) %>%
      separate_longer_delim(old_value, delim = " ") %>%
      filter(!is.na(old_value)) %>%
      distinct() %>%
      ungroup() %>%
      group_by(uuid_question_id) %>%
      summarise(old_value = paste0(old_value, collapse = " ")) %>%
      left_join(amended_new_value %>% select(uuid_question_id, new_value))


    ## then remove all of the options in new_values that also appear in old_values. This means we're just left with the old_values that were
    ## no present in the new values, and so should be 'turned off'
    correct_old_values <- old_and_new_values %>%
      rowwise() %>%
      mutate(
        old_value = str_split(old_value, " ") %>%
          map(~ .x[!.x %in% str_split(new_value, " ")[[1]]]) %>%
          map_chr(~ paste(.x, collapse = " ")) ## remove any of the old values that also appear in new value as they should change to 0
      ) %>%
      ungroup() %>%
      filter(!(old_value == ""))

    ## now just make the child options

    updated_negative_child_binaries <- correct_old_values %>%
      left_join(finished_clogs %>% select(uuid_question_id, question) %>% distinct()) %>%
      mutate(question = paste0(question, "/", old_value),
             new_value = 0,
             uuid = str_split_i(uuid_question_id, "/", 1)) %>%
      select(-old_value, -uuid_question_id, uuid, question, new_value)

    ## join all of the differnt outputs - the fixed parent row, the turned on child binaries and the turned off child binaries.

    select_multiple_clogs <- bind_rows(final_concat_new_values, updated_positive_child_binaries, updated_negative_child_binaries) %>%
      arrange(uuid, question)

    return(select_multiple_clogs)

}

#create_sm_clogs(finished_clogs, kobo_survey = kobo_survey, kobo_choice = kobo_choice)


