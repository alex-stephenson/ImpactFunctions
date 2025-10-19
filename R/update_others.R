#' Update Change Log for Recoding "Other" Responses in a Kobo Survey
#'
#' This function updates a change log for a Kobo survey by recoding "other" responses.
#' It takes survey tool metadata (`survey` and `choices`) and a cleaning log (`cleaning_log`)
#' that holds reviewer responses for reclassification. It then adjusts the change log by:
#'   \itemize{
#'     \item Updating the parent (select one/select multiple) question with the free text recoded value.
#'     \item Nullifying the free text question (indicating the "other" response has been processed).
#'   }
#'
#' @param survey A data frame representing the survey tool. Typically obtained from the "survey" sheet.
#' @param choices A data frame representing the answer options. Typically obtained from the "choices" sheet.
#' @param cleaning_log A data frame containing the cleaning log(s).
#' @param sm_sep Separator used for select multiple questions
#' @param other_string the string of your other select multiple binaries questions, normally "Other" or 'other'
#'
#' @return A data frame with the updated change log entries.
#'
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Identify free text questions that capture "other" responses by filtering for questions
#'           which have "other" specified in the \code{relevant} field.
#'     \item Extract the parent question names from the \code{relevant} field (using regex extraction).
#'     \item Filter the survey for select questions and extract the associated \code{list_name} using
#'           a split on the \code{type} column.
#'     \item Validate that the new response provided in the cleaning log (combined with the parent
#'           question) exists in the list of valid choices.
#'     \item For recoding, two separate workflows are applied for select_one and select_multiple questions.
#'     \item The clog is remade in the UUID, question, change_type, new_value format, with the old other text and select_one /
#'     select_mulitple 'turned off' and the reclassified answer 'turned on'.
#'
#'   }
#'   \figure{updated_others.jpg}{options: width=80%}
#'
#' @importFrom dplyr filter mutate select left_join arrange
#' @importFrom stringr str_detect str_extract str_remove_all str_split str_split_i
#' @export

update_others <- function(survey,
                          choices,
                          cleaning_log,
                          sm_sep = "/",
                          other_string = "other") {

  ## return all questions that are other free text
  other_text_questions <- survey %>%
    dplyr::filter(type == "text", stringr::str_detect(relevant, "(?i)'other'\\)\\s*$"))

  ## now from these others identify what the name of the parent question is from the relevance criteria
  other_questions_strings <- other_text_questions %>%
    mutate(relevant_criteria = str_extract(relevant, "\\{[A-Za-z0-9_]+\\}"),
           relevant_criteria = str_remove_all(relevant_criteria, "\\{|\\}")) %>%
    pull(relevant_criteria)

  ## get the name of the answer options list from the tool
  list_names <- survey %>%
    filter(str_detect(type, "select")) %>%
    mutate(list_name = str_split_i(type, "[ ]+", 2))

  # now filter for the other parent questions
  parent_questions <- list_names %>%
    filter(name %in% other_questions_strings)

  # extract the relevant answers options
  parent_questions_list_name <- parent_questions %>%
    pull(list_name)

  # now we filter the choices tab based on this, so we only have the answers for other options
  other_answer_options <- choices %>%
    filter(list_name %in% parent_questions_list_name)

  ## calculate each other answer and its corresponding question
  other_parent_question <- other_text_questions %>%
    mutate(relevant_criteria = str_extract(relevant, "\\{[A-Za-z0-9_]+\\}"),
           relevant_criteria = str_remove_all(relevant_criteria, "\\{|\\}")) %>%
    select(name, parent_question = relevant_criteria)

  ## get combination of questions and answers
  questions_and_answers <- parent_questions %>%
    select(type, name, list_name) %>%
    left_join((choices %>% select(list_name, answer_name = name)), by = join_by(list_name == list_name), relationship = "many-to-many") %>%
    mutate(question_answer = paste0(name,sm_sep, answer_name)) %>%
    pull(question_answer)

  ## identify whether question is select one or select multiple

  select_type <- survey %>%
    filter(str_detect(type, "select")) %>%
    mutate(select_type = str_split_i(type, "[ ]+", 1)) %>%
    select(name, select_type)


  ## check if the answers are valid
  clogs_validated <- cleaning_log %>%
    filter(change_type == "change_response",
           issue == "recode other") %>%
    left_join(other_parent_question, by = join_by(question == name)) %>%
    mutate(question_answer = paste0(parent_question,sm_sep, new_value),
           answer_valid = ifelse(question_answer %in% questions_and_answers, TRUE, FALSE)) %>%
    left_join(select_type, by = join_by(parent_question == name))

  valid_clogs <- clogs_validated %>%
    filter(answer_valid == TRUE)

  ## select multiple update
  update_select_multiple <- valid_clogs %>%
    filter(select_type == "select_multiple") %>%
    select(uuid, question = question_answer, change_type) %>%
    mutate(new_value = 1)

  turn_off_other <- update_select_multiple %>%
    mutate(question = paste0(str_split_i(question, stringr::fixed(sm_sep), 1), sm_sep, other_string),
           new_value = 0)

  nullify_select_multiple <- valid_clogs %>%
    filter(select_type == "select_multiple") %>%
    select(uuid, question, change_type) %>%
    mutate(new_value= NA)


  select_mulitple_clog <- rbind(update_select_multiple, turn_off_other, nullify_select_multiple)



  ## select multiple update
  update_select_one <- valid_clogs %>%
    filter(select_type == "select_one")%>%
    select(uuid, question = parent_question, change_type, new_value)

  nullify_select_one <- valid_clogs %>%
    filter(select_type == "select_one") %>%
    select(uuid, question, change_type) %>%
    mutate(new_value= NA)

  select_one_clog <- rbind(update_select_one, nullify_select_one)

  others_clog <- rbind(select_mulitple_clog, select_one_clog) %>%
    arrange(uuid) %>%
    mutate(new_value = as.character(new_value))

  cleaning_log_amended <- cleaning_log %>%
    filter(!(uuid %in% others_clog$uuid & question %in% others_clog$question & change_type == "change_response")) %>%
    bind_rows(others_clog)

  return(cleaning_log_amended)
}
