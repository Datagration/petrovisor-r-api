library("R6")

#' @title TagEntry
#'
#' @description Class representing a PetroVisor tag entry.
#'
#' @export TagEntry
#'
#' @field tag The tag of the tag entry. Has to be an instance of the class
#' 'Tag'.
#' @field entity The tagged entity. Has to be an instance of the class 'Entity'.
#' @field start The start date of the tag entry.
#' @field end The end date of the tag entry.
#'
#' @examples
#' \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")
#'
#' # retrieve an entity from the database
#' well <- sp$repositoryService$GetItemByName(type = "Entity", name = "Well01")
#' # retrieve a tag from the datbase
#' active <- sp$repositoryService$GetItemByName(type = "Tag", name = "Active")
#'
#' # create a new tag entry
#' TagEntry$new(entity = well, tag = active, start = "2020-02-01T00:00:00.000Z")
#' }
TagEntry <- R6Class("TagEntryFilter",
  public = list(
    tag = NULL,
    entity = NULL,
    start = NULL,
    end = NULL,

    #' @description Create a new TagEntry instance.
    #'
    #' @param tag The tag of the tag entry. Has to be an instance of the class
    #' 'Tag'.
    #' @param entity The tagged entity. Has to be an instance of the class
    #' 'Entity'.
    #' @param start The start date of the tag entry.
    #' @param end (Optional) The end date of the tag entry.
    initialize = function(tag = NULL, entity = NULL, start = NULL, end = NULL){
      self$tag <- tag
      self$entity <- entity
      self$start <- start
      self$end <- end
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the TagEntriesService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Tag = if(is.null(self$tag)) "" else self$tag$toList(),
        Entity = if(is.null(self$entity)) "" else self$entity$toList(),
        Start = if(is.null(self$start)) "" else self$start,
        End = if(is.null(self$end)) "" else self$end
      )
      return(dl)
    }
  )
)

#' @title TagEntryFilter
#'
#' @description Class representing a PetroVisor tag entry filter.
#'
#' @export TagEntryFilter
#'
#' @field entityName The name of the entity for which tag entries shall be
#' retrieved.
#' @field entityNames A list of entity names. Only tag entries for the given
#' entities will be returned.
#' @field tagName The name of the tag for which tag entries shall be retrieved.
#' @field tagNames A lisf of tag names. Only tag entries for the given tags will
#' be returned.
#' @field tagGroupName The name of a tag group for which tag entries shall be
#' retrieved.
#' @field tagGroupNames A list of tag group names. Only tag entries for the
#' given tag groups will be returned.
#' @field start Start of the range for which tag entries shall be retrieved.
#' @field end End of the range for which tag entries shall be retrieved.
#'
#' @examples
#' \dontrun{
#' TagEntryFilter$new(entityName = "Well01")
#'}
TagEntryFilter <- R6Class("TagEntryFilter",
  public = list(
    entityName = NULL,
    entityNames = NULL,
    tagName = NULL,
    tagNames = NULL,
    tagGroupName = NULL,
    tagGroupNames = NULL,
    start = NULL,
    end = NULL,

    #' @description Create a new TagEntryFilter instance.
    #'
    #' @param entityName The name of the entity for which tag entries shall be
    #' retrieved.
    #' @param entityNames A list of entity names. Only tag entries for the given
    #' entities will be returned.
    #' @param tagName The name of the tag for which tag entries shall be
    #' retrieved.
    #' @param tagNames A lisf of tag names. Only tag entries for the given tags
    #' will be returned.
    #' @param tagGroupName The name of a tag group for which tag entries shall
    #' be retrieved.
    #' @param tagGroupNames A list of tag group names. Only tag entries for the
    #' given tag groups will be returned.
    #' @param start Start of the range for which tag entries shall be retrieved.
    #' @param end End of the range for which tag entries shall be retrieved
    initialize = function(entityName = NULL,
                          entityNames = NULL,
                          tagName = NULL,
                          tagNames = NULL,
                          tagGroupName = NULL,
                          tagGroupNames = NULL,
                          start = NULL,
                          end = NULL){
      self$entityName <- entityName
      self$entityNames <- entityNames
      self$tagName <- tagName
      self$tagNames <- tagNames
      self$tagGroupName <- tagGroupName
      self$tagGroupNames <- tagGroupNames
      self$start <- start
      self$end <- end
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the TagEntriesService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Entity = if(is.null(self$entityName)) "" else self$entityName,
        Entities = if(is.null(self$entityNames)) "" else self$entityNames,
        Tag = if(is.null(self$tagName)) "" else self$tagName,
        Tags = if(is.null(self$tagNames)) "" else self$tagNames,
        TagGroup = if(is.null(self$tagGroupName)) "" else self$tagGroupName,
        TagGroups = if(is.null(self$tagGroupNames)) "" else self$tagGroupNames,
        Start = if(is.null(self$start)) "" else self$start,
        End = if(is.null(self$end)) "" else self$end
      )
      return(dl)
    }
  )
)
