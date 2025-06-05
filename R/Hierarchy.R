library("R6")

#' @title Hierarchy
#'
#' @description Class representing a PetroVisor hierarchy object.
#'
#' @export Hierarchy
#'
#' @field name The name of the hierarchy.
#' @field relationship The child-parent relationships stored in the hierarchy.
#'   A single data frame with the columns \code{child} and \code{parent} if the
#'   hierarchy is static. For time-dependent hierarchies a list of data frames.
#'   The names of the list refer to the date of the relationship.
#' @field is_time_dependent Whether the hierarchy is time dependent or static.
#' @field time_stamp The (first) time stamp of the time dependent hierarchy.
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
    #' @param relationship The child-parent relationships stored in the
    #'   hierarchy. A single data frame with the columns \code{child} and
    #'   \code{parent} if the hierarchy is static. For time-dependent
    #'   hierarchies a list of data frames. The names of the list refer to the
    #'   date of the relationship.
    #' @param is_time_dependent Whether the hierarchy is time dependent or
    #'   static.
    #' @param time_stamp The (first) time stamp of the time dependent hierarchy.
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
      # convert relationship to named list
      if (as.logical(self$is_time_dependent)) {
        rel_list <- as.list(self$relationship[[1]]$parent)
        names(rel_list) <- self$relationship[[1]]$child
      } else {
        rel_list <- as.list(self$relationship$parent)
        names(rel_list) <- self$relationship$child
      }

      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        Relationship = rel_list,
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
