library("R6")

#' @title EntityType
#'
#' @description Class representing a PetroVisor entityType object.
#'
#' @export EntityType
#'
#' @field name The name of the entity type.
#' @field rank The rank of the entity type.
#'
#' @examples
#' \dontrun{
#' EntityType$new(name = "Section", rank = 13)
#'}
EntityType <- R6Class("EntityType",
  public = list(
    name = NULL,
    rank = NULL,

    #' @description Create a new EntityType instance.
    #'
    #' @param name The name of entity type.
    #' @param rank The rank of the entity type.
    initialize = function(name = NULL, rank = NULL){
      self$name <- name
      self$rank <- rank
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Rank = if(is.null(self$rank)) "" else self$rank)
      return(dl)
    }
  )
)
