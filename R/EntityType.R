library("R6")

#' @title EntityType
#'
#' @description Class representing a PetroVisor entityType object.
#'
#' @export EntityType
#'
#' @field name The name of the entity type.
#' @field image The image of the entity type represented as string.
#'
#' @examples
#' \dontrun{
#' EntityType$new(name = "Section")
#'}
EntityType <- R6Class("EntityType",
  public = list(
    name = NULL,
    image = NULL,

    #' @description Create a new EntityType instance.
    #'
    #' @param name The name of entity type.
    #' @param image The image of the entity type represented as string.
    initialize = function(name = NULL, image = NULL) {
      self$name <- name
      self$image <- image
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        Image = if (is.null(self$image)) "" else self$image
      )
      return(dl)
    }
  )
)
