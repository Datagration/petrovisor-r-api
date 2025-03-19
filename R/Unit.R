library("R6")

#' @title Unit
#'
#' @description Class representing a PetroVisor unit object.
#'
#' @export Unit
#'
#' @field name The name of the unit.
#' @field measurementName The unit's measurement name.
#' @field factor The unit's factor. Used for conversion between the unit and
#'   it's base (SI) unit.
#' @field summand The unit's summand. Used for conversion between the unit and
#'   it's base (SI) unit.
#'
#' @examples
#' \dontrun{
#' Unit$new(name = "hyper m",
#'          measurementName = "Length",
#'          factor = 10000000000000,
#'          summand = 0)
#'}
Unit <- R6Class("Unit",
  public = list(
    name = NULL,
    measurementName = NULL,
    factor = NULL,
    summand = NULL,

    #' @description Create a new Unit instance.
    #'
    #' @param name The name of the unit.
    #' @param measurementName The unit's measurement name.
    #' @param factor The unit's factor. Used for conversion between the unit and
    #'   it's base (SI) unit.
    #' @param summand The unit's summand. Used for conversion between the unit
    #'   and it's base (SI) unit.
    initialize = function(name = NULL,
                          measurementName = NULL,
                          factor = NULL,
                          summand = NULL){
      self$name <- name
      self$measurementName <- measurementName
      self$factor <- factor
      self$summand <- summand
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        MeasurementName = if(is.null(self$measurementName)) {
          ""
        } else {
          self$measurementName
        },
        Factor = if(is.null(self$factor)) "" else self$factor,
        Summand = if(is.null(self$summand)) "" else self$summand)
      return(dl)
    }
  )
)
