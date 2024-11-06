
#' Calculate stratified sample
#' @description A function that allows you sample a population based on strata. It will output the _sampled_ population, meaning the returned data frame will be your sample output.
#' @param data the dataframe you want to sample
#' @param grp_by the group or strata
#' @param conf_level the confidence level
#' @param margin_of_error the margin of error
#' @param prop the proportion, default = 0.5
#' @export
#' @examples output <- calc_strat_samples(data =  DSA_VIII_data, grp_by = Neighbourhood, conf_level = 0.95, margin_of_error = 0.05)
calc_strat_samples <- function(data, grp_by, conf_level, margin_of_error, prop = 0.5) {

  calc_size <- function(pop) {
 #   sample_size <- ((1.96^2 * 0.5 * (1 - 0.5)) / (0.05^2)) / (1 + (((1.96^2 * 0.5 * (1 - 0.5)) / (0.05^2)) - 1) / pop)

    z_score <- qnorm((1 + conf_level) / 2)


    # Calculate sample size using the formula
    sample_size <- (z_score^2 * prop * (1 - prop)) / (margin_of_error^2)
    sample_size <- sample_size / (1 + (sample_size - 1) / pop)
    sample_size <- ceiling(sample_size)
    return(sample_size)
  }

  summarised_DSAs <- function(data, grp_by) {
    data_summarised <- data %>%
      dplyr::group_by({{grp_by}}) %>%
      dplyr::summarise(n = n())

    return(data_summarised)
  }

  grp_output <- summarised_DSAs(data, grp_by = Neighbourhood)
  grp_output$sample <- sapply(grp_output$n, calc_size)

  data_nested <- data %>%
    tidyr::nest(.by = {{grp_by}}) %>%
    dplyr::left_join(grp_output, by =join_by({{grp_by}} == {{grp_by}}))


  for (i in 1:length(data_nested$data)) {
    data_nested$data[[i]] <- sample_n(data_nested$data[[i]], data_nested$sample[[i]])
  }

  data_unnested <- data_nested %>%
    tidyr::unnest(., data) %>%
    dplyr::mutate(incl = TRUE)

  return(data_unnested)
}

