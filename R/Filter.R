library("R6")

#' @title Filter
#'
#' @description Class representing a PetroVisor filter object.
#'
#' @export Filter
#'
#' @field name The name of the filter.
#' @field entityNamePattern The pattern to filter the available entities by.
#' @field signalNamePattern The pattern to filter the available signals by.
#' @field entitySetName The name of the selected entity set.
#' @field checkedEntityNames The names of the checked entities.
#' @field checkedSignalNames The names of the checked signals.
#' @field checkedUnitNames The names of the checked units.
#' @field start The filter's start date.
#' @field end The filter's end date.
#' @field step The filter's time increment. Allowed values are:
#'   \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
#'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
#'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}
#' @field isLocked This flag specifies whether the filter is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the filter belongs to.
#' @field isFavorite This flag specifies whether the filter is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the filter.
#' @examples
#' \dontrun{
#' Filter$new(name = "MyFilter",
#'            entitySetName = "All Wells",
#'            checkedSignalNames = list("oil rate", "gas rate"),
#'            checkedUnitNames = list("m3/d", "m3/d"),
#'            start = "2020-01-01T00:00:00.000Z",
#'            end = "2020-03-01T00:00:00.000Z",
#'            step = "Daily")
#'}
Filter <- R6Class("Filter",
  public = list(
    name = NULL,
    entityNamePattern = NULL,
    signalNamePattern = NULL,
    entitySetName = NULL,
    checkedEntityNames = NULL,
    checkedSignalNames = NULL,
    checkedUnitNames = NULL,
    start = NULL,
    end = NULL,
    step = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Filter instance.
    #'
    #'
    #' @param name The name of the filter.
    #' @param entityNamePattern The pattern to filter the available entities by.
    #' @param signalNamePattern The pattern to filter the available signals by.
    #' @param entitySetName The name of the selected entity set.
    #' @param checkedEntityNames The names of the checked entities.
    #' @param checkedSignalNames The names of the checked signals.
    #' @param checkedUnitNames The names of the checked units.
    #' @param start The filter's start date.
    #' @param end The filter's end date.
    #' @param step The filter's time increment. Allowed values are:
    #'   \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
    #'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
    #'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}
    #' @param isLocked This flag specifies whether the filter is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the filter belongs to.
    #' @param isFavorite This flag specifies whether the filter is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the filter.
    initialize = function(name = NULL,
                          entityNamePattern = NULL,
                          signalNamePattern = NULL,
                          entitySetName = NULL,
                          checkedEntityNames = NULL,
                          checkedSignalNames = NULL,
                          checkedUnitNames = NULL,
                          start = NULL,
                          end = NULL,
                          step = c("EveryMinute", "EveryFiveMinutes",
                                   "EveryFifteenMinutes", "Hourly",
                                   "Daily", "Monthly", "Quarterly",
                                   "Yearly"),
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$entityNamePattern <- entityNamePattern
      self$signalNamePattern <- signalNamePattern
      self$entitySetName <- entitySetName
      self$checkedEntityNames <- checkedEntityNames
      self$checkedSignalNames <- checkedSignalNames
      self$checkedUnitNames <- checkedUnitNames
      self$start <- start
      self$end <- end
      self$step <- match.arg(step)
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        EntityNamePattern = if(is.null(self$entityNamePattern)) {
          ""
        } else {
          self$entityNamePattern
        },
        SignalNamePattern = if(is.null(self$signalNamePattern)) {
          ""
        } else {
          self$signalNamePattern
        },
        EntitySetName = if(is.null(self$entitySetName)) {
          ""
        } else {
          self$entitySetName
        },
        CheckedEntities = if(is.null(self$checkedEntityNames)) {
          ""
        } else {
          self$checkedEntityNames
        },
        CheckedSignals = if(is.null(self$checkedSignalNames)) {
          ""
        } else {
          self$checkedSignalNames
        },
        CheckedUnits = if(is.null(self$checkedUnitNames)) {
          ""
        } else {
          self$checkedUnitNames
        },
        Start = if(is.null(self$start)) "" else self$start,
        End = if(is.null(self$end)) "" else self$end,
        Step = if(is.null(self$step)) "" else self$step,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)
