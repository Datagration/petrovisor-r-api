library("R6")

#' @title Entity
#'
#' @description Class representing a PetroVisor entity object.
#'
#' @export Entity
#'
#' @field name The name of the entity.
#' @field entity_type_name The name of the entity's type.
#' @field alias The alias of the entity.
#' @field is_opportunity Whether the entity is an opportunity or not.
#'
#' @examples
#' \dontrun{
#' Entity$new(
#'   name = "NewWell",
#'   entity_type_name = "Well",
#'   alias = "NewWellAlias",
#'   is_opportunity = FALSE)
#' }
Entity <- R6Class("Entity",
  public = list(
    name = NULL,
    entity_type_name = NULL,
    alias = NULL,
    is_opportunity = FALSE,

    #' @description Create a new Entity instance.
    #'
    #' @param name The name of entity.
    #' @param entity_type_name The name of the entity' type.
    #' @param alias The alias of the entity.
    #' @param is_opportunity Whether the entity is an opportunity or not.
    initialize = function(
        name = NULL,
        entity_type_name = NULL,
        alias = NULL,
        is_opportunity = FALSE) {
      self$name <- name
      self$entity_type_name <- entity_type_name
      self$alias <- alias
      self$is_opportunity <- is_opportunity
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        EntityTypeName = if (is.null(self$entity_type_name)) {
          ""
        } else {
          self$entity_type_name
        },
        Alias = if (is.null(self$alias)) "" else self$alias,
        IsOpportunity = if (is.null(self$is_opportunity)) {
          FALSE
        } else {
          self$is_opportunity
        }
      )
      return(dl)
    }
  )
)
