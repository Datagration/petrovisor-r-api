library("R6")

#' @title EntitySet
#'
#' @description Class representing a PetroVisor entity set object.
#'
#' @export EntitySet
#'
#' @field name The name of the entity set.
#' @field entities A list of entities (instances of class Entity).
#' @field formula The entity set's definition as string (P# syntax).
#' @field isLocked This flag specifies whether the entity set is locked
#'   (i.e. managed through the repository). Defaults to \code{FALSE}.
#' @field user The user the entity set belongs to.
#' @field isFavorite This flag specifies whether the entity set is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the entity set.
#' @examples
#' \dontrun{
#' EntitySet$new(name = "MyEntities",
#'               entities = c(Entity$new(name = "Well1",
#'                                       entityTypeName = "Well",
#'                                       alias = "WellAlias1"),
#'                            Entity$new(name = "Well2",
#'                                       entityTypeName = "Well",
#'                                       alias = "WellAlias2"))
#'}
EntitySet <- R6Class("EntitySet",
  public = list(
    name = NULL,
    entities = NULL,
    formula = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Scope instance.
    #'
    #' @param name The name of the scope.
    #' @param entities A list of entities (instances of class Entity).
    #' @param formula The scope's definition as string (P# syntax).
    #' @param isLocked This flag specifies whether the scope is locked
    #'   (i.e. managed through the repository). Defaults to \code{FALSE}.
    #' @param user The user the scopes belongs to.
    #' @param isFavorite This flag specifies whether the scope is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the scope.
    initialize = function(name = NULL,
                          entities = NULL,
                          formula = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$entities <- entities
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
      # create list from list of entities
      entityList <- list()
      if(!is.null(self$entities)){
        for (i in 1:length(self$entities)){
          entityList[[i]] <- self$entities[[i]]$toList()
        }
      } else {
        entityList[[1]] <- ""
      }


      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Entities = if(is.null(self$entities)) "" else entityList,
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
