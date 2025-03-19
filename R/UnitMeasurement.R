library("R6")

#' @title UnitMeasurement
#'
#' @description Class representing a PetroVisor unit measurement object.
#'
#' @export UnitMeasurement
#'
#' @field name The name of the unit measurement.
#' @field canonicalUnitName The name of the unit measurement's canonical unit.
#'
#' @examples
#' \dontrun{
#' UnitMeasurement$new(name = "Length", canonicalUnitName = "m")
#'}
UnitMeasurement <- R6Class("UnitMeasurement",
  public = list(
    name = NULL,
    canonicalUnitName = NULL,

    #' @description Create a new UnitMeasurement instance.
    #'
    #' @param name The name of the unit measurement.
    #' @param canonicalUnitName The name of the unit measurement's canonical
    #'   unit.
    initialize = function(name = NULL, canonicalUnitName = NULL){
      self$name <- name
      self$canonicalUnitName <- canonicalUnitName
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        CanonicalUnitName = if(is.null(self$canonicalUnitName)) {
          ""
        } else {
          self$canonicalUnitName
        })
      return(dl)
    }
  )
)
