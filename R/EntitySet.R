library("R6")

#' @title EntitySet
#'
#' @description Class representing a PetroVisor entity set object.
#'
#' @export EntitySet
#'
#' @field name The name of the entity set.
#' @field entities list of entity objects.
#' @field formula The entity set's definition as string (P# syntax).
#' @field description The description of the item.
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
    description = NULL,
    labels = NULL,

    #' @description Create a new EntitySet instance.
    #'
    #' @param name The name of the entity set.
    #' @param entities list of entity objects.
    #' @param formula The entity set's definition as string (P# syntax).
    #' @param description The description of the item.
    #' @param labels A list of strings holding the labels of the entity set.
    initialize = function(name = NULL,
                          entities = NULL,
                          formula = NULL,
                          description = NULL,
                          labels = list()) {
      self$name <- name
      self$entities <- entities
      self$formula <- formula
      self$description <- description
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      # create list from list of entities
      entity_list <- list()
      if (!is.null(self$entities)) {
        for (i in seq_along(self$entities)){
          entity_list[[i]] <- self$entities[[i]]$toList()
        }
      } else {
        entity_list[[1]] <- ""
      }

      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        Entities = if (is.null(self$entities)) "" else entity_list,
        Formula = if (is.null(self$formula)) "" else self$formula,
        Description = if (is.null(self$description)) "" else self$description,
        Labels = self$labels
      )
      return(dl)
    }
  )
)
