library("R6")

#' @title DataSetRequest
#'
#' @description Class representing a PetroVisor dataset request object.
#'
#' @export DataSetRequest
#'
#' @field entityName The name of the entity for which data is requested.
#' @field signalName The name of the requested signal.
#' @field unitName The name of the unit the data is requested in.
#'
#' @examples
#' \dontrun{
#' DataSetRequest$new(entityName = "Well01",
#'                    signalName = "surface x-coordinate",
#'                    unitName = "m")
#'}
DataSetRequest <- R6Class("DataSetRequest",
  public = list(
    entityName = NULL,
    signalName = NULL,
    unitName = NULL,

    #' @description Create a new DataSetRequest instance.
    #'
    #' @param entityName The name of the entity for which data is requested.
    #' @param signalName The name of the requested signal.
    #' @param unitName The name of the unit the data is requested in.
    initialize = function(entityName = NULL,
                          signalName = NULL,
                          unitName = NULL){
      self$entityName <- entityName
      self$signalName <- signalName
      self$unitName <- unitName
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Entity = if(is.null(self$entityName)) "" else self$entityName,
        Signal = if(is.null(self$signalName)) "" else self$signalName,
        Unit = if(is.null(self$unitName)) "" else self$unitName)
      return(dl)
    }
  )
)
