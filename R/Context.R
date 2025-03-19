library("R6")

#' @title Context
#'
#' @description Class representing a PetroVisor context object.
#'
#' @export Context
#'
#' @field name The name of the context.
#' @field entitySet The context's entity set (object of class EntitySet).
#' @field scope The context's scope (object of class Scope).
#' @field hierarchy (Optional) The hierarchy used for automatic aggregation
#'   (Object of type Hierarchy).
#' @field formula The context's definition as string (P# syntax).
#' @field isLocked This flag specifies whether the context is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the context belongs to.
#' @field isFavorite This flag specifies whether the context is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the context.
#' @examples
#' \dontrun{
#' Context$new(name = "MyContext",
#'           entitySet = EntitySet$new(name = "MyEntities",
#'               entities = c(Entity$new(name = "Well1",
#'                                       entityTypeName = "Well",
#'                                       alias = "WellAlias1"),
#'                            Entity$new(name = "Well2",
#'                                       entityTypeName = "Well",
#'                                       alias = "WellAlias2")),
#'           scope = Scope$new(name = "MyScope",
#'                             start = "2020-01-01T00:00:00.000Z",
#'                             end = "2020-03-01T00:00:00.000Z",
#'                             timeIncrement = "Daily"))
#'}
Context <- R6Class("Context",
  public = list(
    name = NULL,
    entitySet = NULL,
    scope = NULL,
    hierarchy = NULL,
    formula = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Context instance.
    #'
    #' @param name The name of the context.
    #' @param entitySet The context's entity set (object of class EntitySet).
    #' @param scope The context's scope (object of class Scope).
    #' @param hierarchy (Optional) The hierarchy used for automatic aggregation
    #'   (Object of type Hierarchy).
    #' @param formula The context's definition as string (P# syntax).
    #' @param isLocked This flag specifies whether the context is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the context belongs to.
    #' @param isFavorite This flag specifies whether the context is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the context.
    initialize = function(name = NULL,
                          entitySet = NULL,
                          scope = NULL,
                          hierarchy = NULL,
                          formula = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$entitySet <- entitySet
      self$scope <- scope
      self$hierarchy <- hierarchy
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
        EntitySet = if(is.null(self$entitySet)) {
          ""
        } else {
          self$entitySet$toList()
        },
        Scope = if(is.null(self$scope)) "" else self$scope$toList(),
        Hierarchy = if(is.null(self$hierarchy)) "" else self$hierarchy$toList(),
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
