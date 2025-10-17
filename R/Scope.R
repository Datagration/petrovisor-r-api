library("R6")

#' @title Scope
#'
#' @description Class representing a PetroVisor scope object.
#'
#' @export Scope
#'
#' @field name The name of the scope.
#' @field start The scope's start date.
#' @field end The scope's end date.
#' @field time_increment The scope's time increment. Allowed values are:
#'   \code{"EverySecond"}, \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
#'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
#'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}
#' @field depth_increment The scope's depth increment. Allowed values are:
#'   \code{"Meter"}, \code{"HalfMeter"}, \code{"EighthMeter"},
#'   \code{"TenthMeter"}, \code{"Foot"}, \code{"HalfFoot"}
#' @field start_depth The scope's start depth.
#' @field end_depth The scope's end depth.
#' @field formula The scope's definition as string (P# syntax).
#' @field description The description of the item.
#' @field labels A list of strings holding the labels of the scope.
#'
#' @seealso
#' * [RepositoryService] for managing scopes
#' * [DataServices] for using scopes in data queries
#' * [EntitySet] for entity filtering
#' * [MLModel] for ML training context
#' * `vignette("working-with-data")` for data examples
#' * `vignette("machine-learning")` for ML examples
#'
#' @examples
#' \dontrun{
#' Scope$new(name = "MyScope",
#'           start = "2020-01-01T00:00:00.000Z",
#'           end = "2020-03-01T00:00:00.000Z",
#'           time_increment = "Daily")
#'}
Scope <- R6Class("Scope", # nolint: object_name_linter
  public = list(
    name = NULL,
    start = NULL,
    end = NULL,
    time_increment = NULL,
    depth_increment = NULL,
    start_depth = NULL,
    end_depth = NULL,
    formula = NULL,
    description = NULL,
    labels = NULL,

    #' @description Create a new Scope instance.
    #'
    #' @param name The name of the scope.
    #' @param start The scope's start date.
    #' @param end The scope's end date.
    #' @param time_increment The scope's time increment. Allowed values are:
    #'   \code{"EverySecond"}, \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
    #'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
    #'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}
    #' @param depth_increment The scope's depth increment. Allowed values are:
    #'   \code{"Meter"}, \code{"HalfMeter"}, \code{"EighthMeter"},
    #'   \code{"TenthMeter"}, \code{"Foot"}, \code{"HalfFoot"}
    #' @param start_depth The scope's start depth.
    #' @param end_depth The scope's end depth.
    #' @param formula The scope's definition as string (P# syntax).
    #' @param description The description of the item.
    #' @param labels A list of strings holding the labels of the scope.
    initialize = function(name = NULL,
                          start = NULL,
                          end = NULL,
                          time_increment = c("", "EverySecond", "EveryMinute",
                                             "EveryFiveMinutes",
                                             "EveryFifteenMinutes", "Hourly",
                                             "Daily", "Monthly", "Quarterly",
                                             "Yearly"),
                          depth_increment = c("", "Meter", "HalfMeter",
                                              "EighthMeter", "TenthMeter",
                                              "Foot", "HalfFoot"),
                          start_depth = NULL,
                          end_depth = NULL,
                          formula = NULL,
                          description = NULL,
                          labels = list()) {
      self$name <- name
      self$start <- start
      self$end <- end
      self$time_increment <- match.arg(time_increment)
      self$depth_increment <- match.arg(depth_increment)
      self$start_depth <- start_depth
      self$end_depth <- end_depth
      self$formula <- formula
      self$description <- description
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = self$name %||% "",
        Start = self$start %||% "",
        End = self$end %||% "",
        TimeIncrement = self$time_increment %||% "",
        DepthIncrement = self$depth_increment %||% "",
        StartDepth = self$start_depth %||% "",
        EndDepth = self$end_depth %||% "",
        Formula = self$formula %||% "",
        Description = self$description %||% "",
        Labels = self$labels
      )
      return(dl)
    }
  )
)
