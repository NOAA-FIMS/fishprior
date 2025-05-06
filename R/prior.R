setClass(
  "prior",
  slot = c(
    distribution = "character",
    parameters = "list",
    trait = "character",
    type = "character"
  )
)

#' Get information out of the prior class
#'
#' @param object An object of class `prior`.
#' @name get_
#' @keywords getters
NULL

#' @export
#' @rdname get_
#' @keywords getters
setGeneric(
  name = "get_distribution",
  def = function(object) {
    standardGeneric("get_distribution")
  }
)
setMethod(
  f = "get_distribution",
  signature = "prior",
  definition = function(object) {
    return(object@distribution)
  }
)

#' @export
#' @rdname get_
#' @keywords getters
setGeneric(
  name = "get_parameters",
  def = function(object) {
    standardGeneric("get_parameters")
  }
)
#' @rdname get_
#' @keywords getters
setMethod(
  f = "get_parameters",
  signature = "prior",
  definition = function(object) {
    return(object@parameters)
  }
)

#' @export
#' @rdname get_
#' @keywords getters
setGeneric(
  name = "get_trait",
  def = function(object) {
    standardGeneric("get_trait")
  }
)
#' @rdname get_
#' @keywords getters
setMethod(
  f = "get_trait",
  signature = "prior",
  definition = function(object) {
    return(object@trait)
  }
)

#' @export
#' @rdname get_
#' @keywords getters
setGeneric(
  name = "get_type",
  def = function(object) {
    standardGeneric("get_type")
  }
)
#' @rdname get_
#' @keywords getters
setMethod(
  f = "get_type",
  signature = "prior",
  definition = function(object) {
    return(object@type)
  }
)
