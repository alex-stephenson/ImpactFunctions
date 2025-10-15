#' Calculate Odds Ratios Between a Binary Outcome and Multiple Grouping Variables
#'
#' @description
#' Fits logistic regression models between a single binary outcome variable
#' (e.g. a survey indicator) and one or more categorical grouping variables.
#' Each model uses one grouping variable at a time, with the user specifying
#' the reference category for each grouping variable.
#'
#' The function returns tidy odds ratios, confidence intervals, and p-values.
#'
#' @param df A data frame containing the variables of interest.
#' @param question A character string giving the name of the binary outcome variable.
#'   This variable should be coded as 0/1, TRUE/FALSE, or a binary factor.
#' @param group_vars_refs A named list specifying grouping variables and their
#'   corresponding reference levels. For example:
#'   \code{list(governorate = "deir_al_balah", sampling_framework = "a_makeshift_site")}.
#'
#' @details
#' The function loops through each grouping variable, fitting a simple logistic regression
#' model of the form \eqn{outcome ~ group}. The odds ratios are exponentiated model
#' coefficients with 95\% confidence intervals.
#'
#' Grouping variables with only one unique non-missing level are skipped.
#' The same applies if the outcome variable is not binary.
#'
#' @return A tibble with one row per non-reference level per model, containing:
#' \itemize{
#'   \item \code{question} - the outcome variable name
#'   \item \code{group_var} - the grouping variable name
#'   \item \code{ref_group} - the reference category used
#'   \item \code{term} - the level being compared
#'   \item \code{estimate} - the odds ratio
#'   \item \code{conf.low}, \code{conf.high} - 95\% confidence interval bounds
#'   \item \code{p.value} - p-value for the contrast
#' }
#'
#' @examples
#' \dontrun{
#' group_vars_refs <- list(
#'   governorate = "deir_al_balah",
#'   sampling_framework = "a_makeshift_site"
#' )
#'
#' results <- get_odds_ratios_single(
#'   df = model_data,
#'   question = "morbidity_04",
#'   group_vars_refs = group_vars_refs
#' )
#' }
#'
#' @importFrom dplyr mutate filter select n_distinct
#' @importFrom forcats fct_relevel
#' @importFrom broom tidy
#' @importFrom purrr map_dfr
#' @importFrom glue glue
#' @importFrom stats glm as.formula
#' @export
#'
get_odds_ratios <- function(df, question, group_vars_refs) {

  map_dfr(names(group_vars_refs), function(gv) {
    ref <- group_vars_refs[[gv]]

    # Skip if grouping variable has fewer than 2 unique non-NA levels
    if (n_distinct(na.omit(df[[gv]])) < 2) {
      message(glue::glue("Skipping {gv}: only one unique value"))
      return(NULL)
    }

    # Skip if outcome has fewer than 2 unique non-NA levels
    if (n_distinct(na.omit(df[[question]])) < 2) {
      message(glue::glue("Skipping {question}: outcome not binary"))
      return(NULL)
    }

    # Make sure grouping variable is a factor with the specified reference
    df <- df %>%
      mutate(
        !!gv := factor(.data[[gv]]),
        !!gv := fct_relevel(.data[[gv]], ref)
      )

    form <- as.formula(paste0(question, " ~ ", gv))
    model <- try(glm(form, data = df, family = binomial), silent = TRUE)

    if (inherits(model, "try-error")) {
      message(glue::glue("Model failed for {question} ~ {gv}"))
      return(NULL)
    }

    tidy_res <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(question = question, group_var = gv, ref_group = ref) %>%
      select(question, group_var, ref_group, term, estimate, conf.low, conf.high, p.value)

    if (nrow(tidy_res) == 0) {
      message(glue::glue("No contrasts for {question} ~ {gv} (possibly single-level factor)"))
      return(NULL)
    }

    tidy_res
  })
}
