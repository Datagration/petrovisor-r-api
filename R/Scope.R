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
#' @field timeIncrement The scope's time increment. Allowed values are:
#'   \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
#'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
#'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}
#' @field formula The scope's definition as string (P# syntax).
#' @field isLocked This flag specifies whether the scope is locked
#'   (i.e. managed through the repository). Defaults to \code{FALSE}.
#' @field user The user the scopes belongs to.
#' @field isFavorite This flag specifies whether the scope is marked as favorite
#'   item, and thus shown in the favorites tab on the home module in PetroVisor.
#'   Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the scope.
#' @examples
#' \dontrun{
#' Scope$new(name = "MyScope",
#'           start = "2020-01-01T00:00:00.000Z",
#'           end = "2020-03-01T00:00:00.000Z",
#'           timeIncrement = "Daily")
#'}
Scope <- R6Class("Scope",
  public = list(
    name = NULL,
    start = NULL,
    end = NULL,
    timeIncrement = NULL,
    formula = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Scope instance.
    #'
    #' @param name The name of the scope.
    #' @param start The scope's start date.
    #' @param end The scope's end date.
    #' @param timeIncrement The scope's time increment. Allowed values are:
    #'   \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
    #'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
    #'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}
    #' @param formula The scope's definition as string (P# syntax).
    #' @param isLocked This flag specifies whether the scope is locked
    #'   (i.e. managed through the repository). Defaults to \code{FALSE}.
    #' @param user The user the scopes belongs to.
    #' @param isFavorite This flag specifies whether the scope is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the scope.
    initialize = function(name = NULL,
                          start = NULL,
                          end = NULL,
                          timeIncrement = c("EveryMinute", "EveryFiveMinutes",
                                            "EveryFifteenMinutes", "Hourly",
                                            "Daily", "Monthly", "Quarterly",
                                            "Yearly"),
                          formula = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$start <- start
      self$end <- end
      self$timeIncrement <- match.arg(timeIncrement)
      self$formula <- formula
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
        Start = if(is.null(self$start)) "" else self$start,
        End = if(is.null(self$end)) "" else self$end,
        TimeIncrement = if(is.null(self$timeIncrement)) {
          ""
        } else {
          self$timeIncrement
        },
        Formula = if(is.null(self$formula)) "" else self$formula,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
        )
      return(dl)
    }
  )
)
