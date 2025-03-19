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
#' @field isLocked This flag specifies whether the hierarchy is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the hierarchy belongs to.
#' @field isFavorite This flag specifies whether the hierarchy is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the scope.
#' @examples
#' \dontrun{
#' Hierarchy$new(name = "MyHierarchy",
#'               relationship = list(Well1 = "Parent1",
#'                                   Well2 = "Parent1",
#'                                   Well3 = "Parent2"))
#'}
Hierarchy <- R6Class("Hierarchy",
  public = list(
    name = NULL,
    relationship = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Scope instance.
    #'
    #' @param name The name of the hierarchy.
    #' @param relationship The child-parent relationships (named list) stored in
    #'   the hierarchy.
    #' @param isLocked This flag specifies whether the hierarchy is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the hierarchy belongs to.
    #' @param isFavorite This flag specifies whether the hierarchy is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the scope.
    initialize = function(name = NULL,
                          relationship = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$relationship <- relationship
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
        Relationship = if(is.null(self$relationship)) {
          ""
        } else {
          self$relationship
        },
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)
