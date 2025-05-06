setClass(
  "prior",
  slot = c(
    distribution = "character",
    parameters = "list",
    trait = "character",
    type = "character"
  )
)

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

setGeneric(
  name = "get_parameters",
  def = function(object) {
    standardGeneric("get_parameters")
  }
)
setMethod(
  f = "get_parameters",
  signature = "prior",
  definition = function(object) {
    return(object@parameters)
  }
)

setGeneric(
  name = "get_trait",
  def = function(object) {
    standardGeneric("get_trait")
  }
)
setMethod(
  f = "get_trait",
  signature = "prior",
  definition = function(object) {
    return(object@trait)
  }
)

setGeneric(
  name = "get_type",
  def = function(object) {
    standardGeneric("get_type")
  }
)
setMethod(
  f = "get_type",
  signature = "prior",
  definition = function(object) {
    return(object@type)
  }
)
