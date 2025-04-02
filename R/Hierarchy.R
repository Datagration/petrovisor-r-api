library("R6")

#' @title Hierarchy
#'
#' @description Class representing a PetroVisor hierarchy object.
#'
#' @export Hierarchy
#'
#' @field name The name of the hierarchy.
#' @field relationship The child-parent relationships (named list) stored in the
#'   hierarchy.
#' @field is_time_dependent Whether the hierarchy is time dependent or static.
#' @field time_stamp The time stamp of the time dependent hierarchy.
#' @field description The description of the item.
#' @field labels A list of strings holding the labels of the scope.
#' @examples
#' \dontrun{
#' Hierarchy$new(name = "MyHierarchy",
#'               relationship = list(Well1 = "Parent1",
#'                                   Well2 = "Parent1",
#'                                   Parent1 = NA))
#'}
Hierarchy <- R6Class("Hierarchy",
  public = list(
    name = NULL,
    relationship = NULL,
    is_time_dependent = NULL,
    time_stamp = NULL,
    description = NULL,
    labels = NULL,

    #' @description Create a new Scope instance.
    #'
    #' @param name The name of the hierarchy.
    #' @param relationship The child-parent relationships (named list) stored in
    #'   the hierarchy.
    #' @param is_time_dependent Whether the hierarchy is time dependent or
    #'   static.
    #' @param time_stamp The time stamp of the time dependent hierarchy.
    #' @param description The description of the item.
    #' @param labels A list of strings holding the labels of the scope.
    initialize = function(name = NULL,
                          relationship = NULL,
                          is_time_dependent = FALSE,
                          time_stamp = NULL,
                          description = NULL,
                          labels = list()) {
      self$name <- name
      self$relationship <- relationship
      self$is_time_dependent <- is_time_dependent
      self$time_stamp <- time_stamp
      self$description <- description
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        Relationship = if (is.null(self$relationship)) {
          ""
        } else {
          self$relationship
        },
        IsTimeDependent = if (is.null(self$is_time_dependent)) {
          ""
        } else {
          self$is_time_dependent
        },
        Description = if (is.null(self$description)) "" else self$description,
        Labels = self$labels
      )

      if (!is.null(self$time_stamp)) {
        dl$TimeStamp <- self$time_stamp
      }

      return(dl)
    }
  )
)
