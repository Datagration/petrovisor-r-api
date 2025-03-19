library("R6")

#' @title Entity
#'
#' @description Class representing a PetroVisor entity object.
#'
#' @export Entity
#'
#' @field name The name of the entity.
#' @field entityTypeName The name of the entity's type.
#' @field alias The alias of the entity.
#'
#' @examples
#' \dontrun{
#' Entity$new(name = "NewWell", entityTypeName = "Well", alias = "NewWellAlias")
#' }
Entity <- R6Class("Entity",
  public = list(
    name = NULL,
    entityTypeName = NULL,
    alias = NULL,

    #' @description Create a new Entity instance.
    #'
    #' @param name The name of entity.
    #' @param entityTypeName The name of the entity' type.
    #' @param alias The alias of the entity.
    initialize = function(name = NULL, entityTypeName = NULL, alias = NULL) {
      self$name <- name
      self$entityTypeName <- entityTypeName
      self$alias <- alias
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        EntityTypeName = if (is.null(self$entityTypeName)) {
          ""
        } else {
          self$entityTypeName
        },
        Alias = if (is.null(self$alias)) "" else self$alias
      )
      return(dl)
    }
  )
)
