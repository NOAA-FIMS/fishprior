# prior class ----
utils::globalVariables(c(
  "group",
  "name",
  "trait",
  "value",
  "mean",
  "se",
  "mean_normal",
  "sd_normal"
))

setClass(
  "prior",
  slot = c(
    distribution = "character",
    parameters = "list",
    trait = "character",
    type = "character",
    group = "character",
    data = "data.frame"
  )
)

## Print methods ----

methods::setMethod(
  f = "show",
  signature = "prior",
  definition = function(object) {
    cli::cli_inform(c(
      "A `prior` object",
      "i" = "Distribution: {get_distribution(object)}",
      "i" = "Trait: {get_trait(object)}"
    ))
  }
)

methods::setMethod(
  f = "print",
  signature = "prior",
  definition = function(x, ...) {
    methods::show(x)
  }
)

## Getters ----

#' Get information out of the prior class
#'
#' @param object An object of class `prior`.
#' @name get_
#' @keywords getters
NULL

#' @export
#' @rdname get_
#' @keywords getters
methods::setGeneric(
  name = "get_distribution",
  def = function(object) {
    standardGeneric("get_distribution")
  }
)

#' @export
#' @rdname get_
#' @keywords getters
methods::setMethod(
  f = "get_distribution",
  signature = "prior",
  definition = function(object) {
    return(object@distribution)
  }
)

#' @export
#' @rdname get_
#' @keywords getters
methods::setGeneric(
  name = "get_parameters",
  def = function(object) {
    standardGeneric("get_parameters")
  }
)
#' @rdname get_
#' @keywords getters
methods::setMethod(
  f = "get_parameters",
  signature = "prior",
  definition = function(object) {
    return(object@parameters)
  }
)

#' @export
#' @rdname get_
#' @keywords getters
methods::setGeneric(
  name = "get_trait",
  def = function(object) {
    standardGeneric("get_trait")
  }
)
#' @rdname get_
#' @keywords getters
methods::setMethod(
  f = "get_trait",
  signature = "prior",
  definition = function(object) {
    return(object@trait)
  }
)

#' @export
#' @rdname get_
#' @keywords getters
methods::setGeneric(
  name = "get_group",
  def = function(object) {
    standardGeneric("get_group")
  }
)
#' @rdname get_
#' @keywords getters
methods::setMethod(
  f = "get_group",
  signature = "prior",
  definition = function(object) {
    return(object@group)
  }
)

#' @export
#' @rdname get_
#' @keywords getters
methods::setGeneric(
  name = "get_type",
  def = function(object) {
    standardGeneric("get_type")
  }
)
#' @rdname get_
#' @keywords getters
methods::setMethod(
  f = "get_type",
  signature = "prior",
  definition = function(object) {
    return(object@type)
  }
)

#' @export
#' @rdname get_
#' @keywords getters
methods::setGeneric(
  name = "get_data",
  def = function(object) {
    standardGeneric("get_data")
  }
)
#' @rdname get_
#' @keywords getters
methods::setMethod(
  f = "get_data",
  signature = "prior",
  definition = function(object) {
    return(object@data)
  }
)

## Evaluators ----

methods::setGeneric(
  name = "evaluate",
  def = function(object, n = 1000) {
    standardGeneric("evaluate")
  }
)

methods::setMethod(
  f = "evaluate",
  signature = "prior",
  definition = function(object, n = 1000) {
    distribution <- get_distribution(object)
    if (!distribution %in% c("normal", "lognormal")) {
      cli::cli_abort(c(
        "Unsupported distribution: {.val {distribution}}",
        "i" = "Supported distributions are: {.val normal}, {.val lognormal}."
      ))
    }
    parameters <- get_parameters(object)

    if (distribution == "normal") {
      values <- stats::rnorm(
        n,
        mean = parameters[["mean"]],
        sd = parameters[["sd"]]
      )
    }
    if (distribution == "lognormal") {
      values <- stats::rlnorm(
        n,
        meanlog = parameters[["mean"]],
        sdlog = parameters[["sd"]]
      )
    }

    tibble::tibble(
      group = get_group(object),
      trait = get_trait(object),
      value = values
    )
  }
)

## Plotters ----

methods::setMethod(
  f = "plot",
  signature = "prior",
  definition = function(x, y, ...) {
    plot_data <- evaluate(x)
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = value,
        col = name
      )
    ) +
      ggplot2::geom_histogram() +
      ggplot2::labs(
        title = paste("Prior for", get_trait(x)),
        x = get_trait(x),
        y = "Density"
      )
  }
)
