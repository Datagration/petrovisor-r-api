library("R6")

#' @title DataSetRequest
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Class representing a PetroVisor dataset request object.
#'
#' **This class is deprecated and no longer used.** It was originally designed
#' for use with DataServices but was never integrated into the actual
#' implementation. DataServices uses direct parameter passing instead.
#'
#' This class is retained only for backwards compatibility and may be removed
#' in a future version.
#'
#' @export DataSetRequest
#'
#' @field entityName The name of the [Entity] for which data is requested.
#' @field signalName The name of the requested [Signal].
#' @field unitName The name of the [Unit] the data is requested in.
#'
#' @seealso
#' * [DataServices] for loading data (does not use DataSetRequest)
#' * [Entity] for entity definitions
#' * [Signal] for signal definitions
#' * [Unit] for unit definitions
#' * `vignette("working-with-data")` for data loading examples
#'
#' @examples
#' \dontrun{
#' # DEPRECATED: This class is not used by DataServices
#' DataSetRequest$new(entityName = "Well01",
#'                    signalName = "surface x-coordinate",
#'                    unitName = "m")
#'}
DataSetRequest <- R6Class("DataSetRequest", # nolint: object_name_linter
  public = list(
    entityName = NULL,
    signalName = NULL,
    unitName = NULL,

    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Create a new DataSetRequest instance.
    #'
    #' **Deprecated:** This class is not used by DataServices and will be
    #' removed in a future version.
    #'
    #' @param entityName The name of the [Entity] for which data is requested.
    #' @param signalName The name of the requested [Signal].
    #' @param unitName The name of the [Unit] the data is requested in.
    initialize = function(entityName = NULL,
                          signalName = NULL,
                          unitName = NULL) {
      lifecycle::deprecate_warn(
        when = "3.6.0",
        what = "DataSetRequest$new()",
        details = paste(
          "DataSetRequest is not used by DataServices.",
          "Use DataServices methods directly with entity names,",
          "signal names, and unit names as parameters."
        )
      )
      self$entityName <- entityName
      self$signalName <- signalName
      self$unitName <- unitName
    },

    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Convert the object to a list. This function is mainly used
    #' by [DataServices] to convert the objects to lists and then
    #' call the web API.
    #'
    #' **Deprecated:** This method is not used by DataServices.
    toList = function() {
      lifecycle::deprecate_warn(
        when = "3.6.0",
        what = "DataSetRequest$toList()",
        details = "DataSetRequest is deprecated and not used by DataServices."
      )
      dl <- list(
        Entity = if (is.null(self$entityName)) "" else self$entityName,
        Signal = if (is.null(self$signalName)) "" else self$signalName,
        Unit = if (is.null(self$unitName)) "" else self$unitName
      )
      return(dl)
    }
  )
)
