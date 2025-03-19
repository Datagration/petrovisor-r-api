# ToDo: add documentation
# ToDo: apply google R-style guide

library("R6")
library("httr")

#' @title TagEntriesService
#'
#' @description Provides access to functionality related to tag entries provided
#' through the web API.
#'
#' @details A new instance of this class will be created by the ServiceProvider
#' automatically.
#'
#' @export TagEntriesService
#'
#' @examples \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")
#'
#' # get tag entries of group "Info"
#' tagEntries <- sp$tagEntriesService$GetTagEntries(
#'   TagEntriesFilter$new(TagGroup = "Info")
#' )
#'
#' # delete tag entries
#' sp$tagEntriesService$DeleteTagEntry(
#'   entityName = "Well01",
#'   tagName = "Active",
#'   start = "2020-01-01T00:00:00.000Z"
#' )
#' }
TagEntriesService <- R6Class("TagEntriesService",
  public = list(

    #' @description Create a new TagEntriesService instance. This is done by the
    #' ServiceProvider automatically.
    #'
    #' @param url the URL for the API calls.
    #' @param tokenType the type of the issued token.
    #' @param token the issued token.
    initialize = function(url, tokenType, token) {
      private$url <- url
      private$tokenType <- tokenType
      private$token <- token
    },

    #' @description Add a new tag entry or update the end date of an existing
    #' one.
    #'
    #' @param tagEntry The tag entry to add or update. Has to be an instance of
    #' the class 'TagEntry'.
    #'
    #' @return An instance of the class 'TagEntry' representing the added or
    #' updated tag entry.
    AddOrEditTagEntry = function(tagEntry) {
      ret <- httr::POST(
        paste0(
          private$url,
          "TagEntries"
        ),
        body = jsonlite::toJSON(tagEntry$toList(), auto_unbox = TRUE),
        content_type_json(),
        add_headers(Authorization = paste(private$tokenType, private$token))
      )

      # if there is a conflict, try to update the tag entry
      if (httr::http_status(ret)$reason == "Conflict") {
        ret <- httr::POST(
          paste0(
            private$url,
            "TagEntries/UpdateEnd"
          ),
          body = jsonlite::toJSON(tagEntry$toList(), auto_unbox = TRUE),
          content_type_json(),
          add_headers(Authorization = paste(private$tokenType, private$token))
        )
      }

      httr::stop_for_status(ret)
      cont <- jsonlite::fromJSON(httr::content(ret, as = "text"))

      # return the created tag entry
      return(TagEntry$new(
        tag = Tag$new(
          name = cont$Tag$Name,
          tagGroup = cont$Tag$TagGroup
        ),
        entity = Entity$new(
          name = cont$Entity$Name,
          entityTypeName =
            cont$Entity$EntityTypeName,
          alias = cont$Entity$Alias
        ),
        start = cont$Start,
        end = if ("End" %in% names(cont)) cont$Start
      ))
    },

    #' @description Add multiple new tag entries at once.
    #'
    #' @param entries A list of tag entries to add. The items of the list must
    #' be instances of the class 'TagEntry'.
    #' @param deleteExisting If this flag is set to \code{TRUE}, existing tag
    #' entries in the specified new tag entries range are deleted prior to
    #' inserting the new tag entries.
    AddTagEntries = function(entries, deleteExisting = c(TRUE, FALSE)) {
      urlExtension <- if (deleteExisting) "ReplaceMultiple" else "AddMultiple"

      # create list from list of tag entries
      dl <- list()
      for (i in 1:length(entries)) {
        dl[[i]] <- entries[[i]]$toList()
      }

      ret <- httr::POST(
        paste0(
          private$url,
          "TagEntries/",
          urlExtension
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        content_type_json(),
        add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    },

    #' @description Delete all tag entries with a start date in the specified
    #' time range.
    #'
    #' @param entityName The name of the entity for which to delete the tag
    #' entries.
    #' @param tagName The name of the tag for which to delete the tag entries.
    #' @param start The start date of the range.
    #' @param end (Optional) The end date of the range.
    DeleteTagEntries = function(entityName, tagName, start, end = NULL) {
      ret <- httr::DELETE(
        paste0(
          private$url,
          "TagEntries/Range"
        ),
        query = list(
          Tag = tagName,
          Entity = entityName,
          Start = start,
          End = if (is.null(end)) "" else end
        ),
        add_headers(Authorization = paste(private$tokenType, private$token))
      )
      httr::stop_for_status(ret)
    },

    #' @description Delete the specified tag entry.
    #'
    #' @param entityName The name of the tag entry's entity.
    #' @param tagName The name of the tag entry's tag.
    #' @param start The start date of the tag entry.
    DeleteTagEntry = function(entityName, tagName, start) {
      ret <- httr::DELETE(
        paste0(
          private$url,
          "TagEntries"
        ),
        query = list(
          Tag = tagName,
          Entity = entityName,
          Start = start
        ),
        add_headers(Authorization = paste(private$tokenType, private$token))
      )
      httr::stop_for_status(ret)
    },

    #' @description Get tag entries according to the given filter.
    #'
    #' @param tagEntriesFilter The filter specifying which tag entries to
    #' retrieve from the database.
    #'
    #' @return A dataframe containing the tag entries.
    #'
    #' @examples \dontrun{
    #' sp <- ServiceProvider$new("192.168.0.123",
    #'                           8095,
    #'                           "workspace",
    #'                           "user",
    #'                           "password")
    #' tagEntries <- sp$tagEntriesService$GetTagEntries(
    #'   TagEntriesFilter$new(TagGroup = "Info"))
    #' }
    GetTagEnties = function(tagEntriesFilter) {
      ret <- httr::POST(
        paste0(
          private$url,
          "TagEntries/Filtered"
        ),
        body = jsonlite::toJSON(tagEntriesFilter$toList(), auto_unbox = TRUE),
        content_type_json(),
        add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
      cont <- httr::content(ret, as = "text")
      return(jsonlite::fromJSON(cont))
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL,
    severity = NULL
  )
)
