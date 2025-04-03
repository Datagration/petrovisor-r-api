library("R6")

#' @title UnitMeasurement
#'
#' @description Class representing a PetroVisor unit measurement object.
#'
#' @export UnitMeasurement
#'
#' @field name The name of the unit measurement.
#' @field canonical_unit_name The name of the unit measurement's canonical unit.
#'
#' @examples
#' \dontrun{
#' UnitMeasurement$new(name = "Length", canonical_unit_name = "m")
#'}
UnitMeasurement <- R6Class("UnitMeasurement",
  public = list(
    name = NULL,
    canonical_unit_name = NULL,

    #' @description Create a new UnitMeasurement instance.
    #'
    #' @param name The name of the unit measurement.
    #' @param canonical_unit_name The name of the unit measurement's canonical
    #'   unit.
    initialize = function(name = NULL, canonical_unit_name = NULL) {
      self$name <- name
      self$canonical_unit_name <- canonical_unit_name
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        CanonicalUnitName = if (is.null(self$canonical_unit_name)) {
          ""
        } else {
          self$canonical_unit_name
        }
      )
      return(dl)
    }
  )
)
