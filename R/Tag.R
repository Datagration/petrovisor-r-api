library("R6")

#' @title Tag
#'
#' @description Class representing a PetroVisor tag object.
#'
#' @export Tag
#'
#' @field name The name of the tag.
#' @field tag_group The tag's tag group.
#'
#' @examples
#' \dontrun{
#' Tag$new(name = "NewTag", tag_group = "Group 1")
#'}
Tag <- R6Class("Tag",
  public = list(
    name = NULL,
    tag_group = NULL,

    #' @description Create a new Tag instance.
    #'
    #' @param name The name of tag.
    #' @param tag_group The tag's tag group.
    initialize = function(name = NULL, tag_group = NULL) {
      self$name <- name
      self$tag_group <- tag_group
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        TagGroup = if (is.null(self$tag_group)) "" else self$tag_group
      )
      return(dl)
    }
  )
)
