#' Calculate design effect, effective sample size, and margin of error from post-stratification weights
#'
#' This helper function estimates the statistical precision of a weighted sample by
#' calculating the effective sample size and design effect (following Kish 1992).
#' It is useful when post-stratification weights are applied to correct for unequal
#' probabilities of selection across strata.
#'
#' @param w A numeric vector containing post-stratification weights.
#'
#' @return A list containing:
#' \describe{
#'   \item{n}{Number of observations.}
#'   \item{mean_weight}{Mean of weights.}
#'   \item{sd_weight}{Standard deviation of weights.}
#'   \item{cv_weight}{Coefficient of variation — how uneven the weights are.}
#'   \item{design_effect}{Variance inflation due to weighting.}
#'   \item{effective_n}{Equivalent unweighted sample size after weighting.}
#'   \item{margin_of_error_95}{Approximate 95\% margin of error (in percentage points) for a proportion near 0.5.}
#' }
#'
#' @examples
#' weights <- c(0.8, 1.0, 1.2, 0.9, 1.5, 0.7, 1.1, 1.3)
#' post_strat_representativity(weights)
#'
#'
#' post_strat_representativity(dataframe$weights)
#'
#' @export
post_strat_representativity <- function(w) {

  if(!is.numeric(w)) stop("w provided is not numeric")

  mean_w <- mean(w)
  sd_w   <- sd(w)
  cv_w   <- sd_w / mean_w
  n      <- length(w)

  n_eff  <- n / (1 + cv_w^2)
  deff   <- n / n_eff
  moe_95 <- 1.96 * sqrt(0.25 / n_eff)

  list(
    n = n,
    mean_weight = mean_w,
    sd_weight = sd_w,
    cv_weight = cv_w,
    design_effect = deff,
    effective_n = n_eff,
    margin_of_error_95 = moe_95 * 100
  )
}

