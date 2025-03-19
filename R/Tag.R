library("R6")

#' @title Tag
#'
#' @description Class representing a PetroVisor tag object.
#'
#' @export Tag
#'
#' @field name The name of the tag.
#' @field tagGroup The tag's tag group.
#'
#' @examples
#' \dontrun{
#' Tag$new(name = "NewTag", tagGroup = "Group 1")
#'}
Tag <- R6Class("Tag",
  public = list(
    name = NULL,
    tagGroup = NULL,

    #' @description Create a new Tag instance.
    #'
    #' @param name The name of tag.
    #' @param tagGroup The tag's tag group.
    initialize = function(name = NULL, tagGroup = NULL){
      self$name <- name
      self$tagGroup <- tagGroup
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        TagGroup = if(is.null(self$tagGroup)) "" else self$tagGroup)
      return(dl)
    }
  )
)
