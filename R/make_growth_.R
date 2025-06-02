# growth coefficient K
# in Von Bertalanffy growth there is a k parameter
# We have access to log(growth_coefficient)

# pick a distribution, if we choose normal that would be the easiest
# look at fishlife but the assumption should be that it is normal

# Make prior for log(growth_coefficient)
# Make prior for growth_coefficient
#' Make a prior for the growth coefficient
#'
#' TODO: Provide more details on the parameter.
#'
#' @param data A data frame for a single species because you might
#'   need more than one trait to create a prior.
#' @param type A character string indicating the type of prior. The default
#'   is "informative".
make_growth_coefficient_prior <- function(
    data,
    type = c("informative", "diffuse")) {
  if (length(unique(data[["name"]])) > 1) {
    cli::cli_abort(c(
      "{.fn make_growth_coefficient_prior} works with just one group.",
      "i" = "Please filter the name column for just one entry."
    ))
  }
  type <- rlang::arg_match(type)
  if (type == "diffuse") {
    cli::cli_abort(c(
      "No {.var {type}} prior available for the growth coefficient",
      "i" = "Please use {.fn make_growth_coefficient_prior} with
      {.val type = 'informative'}"
    ))
  }
  x <- transform_data_frame(data) |>
    dplyr::filter(trait == "log(growth_coefficient)")

  parameters <- dplyr::case_when(
    type == "informative" ~ list(
      mean = x[["mean_normal"]],
      sd = x[["sd_normal"]]
    )
  )
  if (parameters[[1]] == "error") {
    cli::cli_abort(c(
      "No {.var {type}} prior available for the growth coefficient"
    ))
  }

  # Return an object of class `prior`
  methods::new(
    Class = "prior",
    distribution = "normal",
    parameters = parameters,
    trait = "growth_coefficient",
    type = type,
    group = x[["name"]],
    data = x
  )
}
