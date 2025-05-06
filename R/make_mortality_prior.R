#' Make a prior for natural mortality
#'
#' Make a prior for natural mortality using either an informative or
#' diffuse prior. The informative prior is based on the Hamel--Cope prior,
#' which is based on the maximum age of a fish species. The diffuse prior is
#' based on the estimated natural mortality of a fish species.
#'
#' @param data A data frame returned by [get_FishLife_traits()] containing
#'   life-history information for at least one fish species.
#' @param type A character string indicating the type of prior to be created.
#'   Either `"diffuse"` or `"informative"`. The default is `"diffuse"`.
#'
#' @export
#' @keywords mortality prior
#' @return
#' A tibble is returned that contains three columns, `"name"` from the input
#' data frame with the species name, `"parameter"` specifying which parameter
#' the prior is for, and `"prior"` that contains the S4 class `prior`.
#' @examples
#' # uninformative or diffuse prior
#' make_mortality_prior(traits_example, type = "diffuse")
#' # informative prior
#' make_mortality_prior(traits_example, type = "informative")
make_mortality_prior <- function(
    data,
    type = c("diffuse", "informative")) {
  type <- rlang::arg_match(type)

  # Informative (Hamel--Cope) prior
  # example from Pacific hake is 5.40/25 = 0.216, which is the median natural
  # mortality in normal space but if you take the log of it then the mean of
  # the log-normal prior is log(0.216) = -1.53 and the sd in log space no
  # matter the species is 0.31.
  if (type == "informative") {
    out <- dplyr::filter(
      data,
      trait == "log(age_max)"
    ) |>
      dplyr::mutate(
        parameter = "natural mortality",
        prior = purrr::map(
          mean,
          make_informative_mortality_prior
        )
      )
  }
  # Diffuse prior
  if (type == "diffuse") {
    out <- dplyr::filter(
      data,
      trait == "log(natural_mortality)"
    ) |>
      dplyr::mutate(
        parameter = "natural mortality",
        prior = purrr::map2(
          .x = mean,
          .y = se,
          .f = make_diffuse_mortality_prior
        )
      )
  }
  return(dplyr::select(out, -trait, -mean, -se))
}

#' Make an information prior for natural mortality
#'
#' The Hamel--Cope prior for natural mortality is thought to be a good,
#' informative prior for natural mortality. It is based on the maximum age of
#' a fish species.
#'
#' @details
#' The Hamel--Cope prior is based on the assumption that natural mortality is
#' lognormally distributed. The mean of the lognormal distribution is
#' calculated as 5.40 divided by the maximum age of the fish species. The
#' standard deviation of the lognormal distribution is set to 0.31.
#'
#' @param log_maximum_age A numeric value representing the maximum age of the
#'   fish species on the log scale. This value must be greater than 0.
#' @keywords informative mortality prior
#' @export
#' @return
#' An informative lognormal distribution is returned using the `prior` class.
#' @examples
#' prior_35 <- make_informative_mortality_prior(log(35))
#' exp(get_parameters(prior_35)[["mean_log"]])
#' traits_example |>
#'   dplyr::filter(
#'     trait == "log(age_max)"
#'   ) |>
#'   dplyr::mutate(
#'     natural_mortality = purrr::map(
#'       mean,
#'       make_informative_mortality_prior
#'     )
#'   )
make_informative_mortality_prior <- function(log_maximum_age) {
  if (length(log_maximum_age) != 1) {
    cli::cli_abort("{.var log_maximum_age} must be a single value")
  }
  if (!is.numeric(log_maximum_age)) {
    cli::cli_abort("{.var log_maximum_age} must be numeric")
  }
  if (log_maximum_age < 0) {
    cli::cli_abort("{.var log_maximum_age} must be greater than 0")
  }
  median_normal <- 5.40 / exp(log_maximum_age)
  mean_lognormal <- log(median_normal)
  standard_deviation_lognormal <- 0.31
  methods::new(
    "prior",
    distribution = "lognormal",
    parameters = list(
      mean_log = mean_lognormal,
      sd_log = standard_deviation_lognormal
    ),
    type = "informative"
  )
}

# TODO: change se to standard deviation on the log scale
make_diffuse_mortality_prior <- function(
    log_natural_mortality,
    standard_deviation_lognormal) {
  if (length(log_natural_mortality) != 1) {
    cli::cli_abort("{.var log_natural_mortality} must be a single value")
  }
  if (!is.numeric(log_natural_mortality)) {
    cli::cli_abort("{.var log_natural_mortality} must be numeric")
  }
  methods::new(
    "prior",
    distribution = "lognormal",
    parameters = list(
      mean_log = log_natural_mortality,
      sd_log = standard_deviation_lognormal
    ),
    trait = "natural mortality",
    type = "diffuse"
  )
}
