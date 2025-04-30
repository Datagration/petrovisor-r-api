library("R6")

#' @title TagEntry
#'
#' @description Class representing a PetroVisor tag entry.
#'
#' @export TagEntry
#'
#' @field tag_name The name of the tag.
#' @field entity_name The name of the tagged entity.
#' @field start The start date of the tag entry.
#' @field end The end date of the tag entry.
#'
#' @examples
#' \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")
#'
#' # create a new tag entry
#' TagEntry$new(entity_name = "Well 01" ,
#'              tag_name = "Active",
#'              start = "2020-02-01T00:00:00.000Z")
#' }
TagEntry <- R6Class("TagEntry",
  public = list(
    tag_name = NULL,
    entity_name = NULL,
    start = NULL,
    end = NULL,

    #' @description Create a new TagEntry instance.
    #'
    #' @param tag_name The name of the tag.
    #' @param entity_name The name of the tagged entity.
    #' @param start The start date of the tag entry.
    #' @param end (Optional) The end date of the tag entry.
    initialize = function(tag_name = NULL,
                          entity_name = NULL,
                          start = NULL,
                          end = NULL) {
      self$tag_name <- tag_name
      self$entity_name <- entity_name
      self$start <- start
      self$end <- end
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the TagEntriesService to convert the objects to lists and then
    #' call the web API.
    to_list = function() {
      dl <- list(
        TagName = if (is.null(self$tag_name)) "" else self$tag_name,
        EntityName = if (is.null(self$entity_name)) "" else self$entity_name,
        Start = if (is.null(self$start)) "" else self$start,
        End = if (is.null(self$end)) "" else self$end
      )
      return(dl)
    }
  )
)
