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
#' tagEntries <- sp$tag_entries$GetTagEntries(
#'   TagEntriesFilter$new(TagGroup = "Info")
#' )
#'
#' # delete tag entries
#' sp$tag_entries$DeleteTagEntry(
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
    #' @param token_type the type of the issued token.
    #' @param token the issued token.
    initialize = function(url, token_type, token) {
      private$url <- url
      private$token_type <- token_type
      private$token <- token
    },

    #' @description Add new or update single or multiple tag entries at once.
    #'
    #' @param tag_entries A dataframe specifying the tag entries to save or
    #'  update. The dataframe has to have for columns with the names
    #'  \code{tag_name}, \code{entity_name}, \code{start}, \code{end}.
    save = function(tag_entries) {

      # Create objects from data frame entries
      tag_entries$objects <- apply(
        tag_entries,
        1,
        function(x) {
          TagEntry$new(tag_name = x[["tag_name"]],
                       entity_name = x[["entity_name"]],
                       start = x[["start"]],
                       end = x[["end"]])
        }
      )

      # Build body for request
      body <- lapply(
        tag_entries$objects,
        function(x) {
          x$to_list()
        }
      )
      names(body) <- NULL

      ret <- httr::POST(
        paste0(
          private$url,
          "TagEntries/Add"
        ),
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        content_type_json(),
        add_headers(Authorization = paste(private$token_type, private$token))
      )

      httr::stop_for_status(ret)
    },

    #' @description Delete all tag entries with a start date in the specified
    #'  time range.
    #'
    #' @param entity_name The name of the entity for which to delete the tag
    #'  entries.
    #' @param tag_name The name of the tag for which to delete the tag entries.
    #' @param start The start date of the range.
    #' @param end (Optional) The end date of the range.
    delete_range = function(entity_name,
                            tag_name,
                            start,
                            end = NULL) {
      # Build query
      query <- list(Start = start)
      if (!is.null(entity_name)) query$Entity <- entity_name
      if (!is.null(tag_name)) query$Tag <- tag_name
      if (!is.null(end)) query$End <- end

      ret <- httr::DELETE(
        paste0(
          private$url,
          "TagEntries/Range"
        ),
        query = query,
        add_headers(Authorization = paste(private$token_type, private$token))
      )

      httr::stop_for_status(ret)
    },

    #' @description Delete the specified tag entries.
    #'
    #' @param tag_entries A dataframe specifying the tag entries to delete. The
    #'  dataframe has to have for columns with the names \code{tag_name},
    #'  \code{entity_name}, \code{start}, \code{end}.
    delete = function(tag_entries) {
      # Create objects from data frame entries
      tag_entries$objects <- apply(
        tag_entries,
        1,
        function(x) {
          TagEntry$new(tag_name = x[["tag_name"]],
                       entity_name = x[["entity_name"]],
                       start = x[["start"]],
                       end = x[["end"]])
        }
      )

      # Build body for request
      body <- lapply(
        tag_entries$objects,
        function(x) {
          x$to_list()
        }
      )
      names(body) <- NULL

      ret <- httr::POST(
        paste0(
          private$url,
          "TagEntries/Delete"
        ),
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        content_type_json(),
        add_headers(Authorization = paste(private$token_type, private$token))
      )
      httr::stop_for_status(ret)
    },

    #' @description Get tag entries according to the given filter.
    #'
    #' @param entity_names Names of the entities for which tag entries shall be
    #'  retrieved. Has to be specified if \code{entity_set_name} is not used.
    #' @param entity_set_name Name of the entity set defining the entities for
    #'  which tag entries shall be retrieved. Has to be specified if
    #'  \code{entity_names} is not used.
    #' @param tag_names Tags for which tag entries shall be retrieved. Has to be
    #'  specified if \code{tag_group_names} is not used.
    #' @param tag_group_names Tag groups for which tag entries shall be
    #'  retrieved. Has to be specified if \code{tag_names} is not used.
    #' @param start Together with \code{end} defines a date range that is used
    #'  to filter tag entries. All tag entries overlapping with this date range
    #'  will be returned.
    #' @param end Together with \code{start} defines a date range that is used
    #'  to filter tag entries. All tag entries overlapping with this date range
    #'  will be returned.
    #' @param start_date_begins Together with \code{start_date_ends} defines a
    #'  date range for the start dates of the tag entries. Tag entries with a
    #'  start date within this date range will be returned.
    #' @param start_date_ends Together with \code{start_date_begins} defines a
    #'  date range for the start dates of the tag entries. Tag entries with a
    #'  start date within this date range will be returned.
    #' @param end_date_begins Together with \code{end_date_ends} defines a
    #'  date range for the end dates of the tag entries. Tag entries with an
    #'  end date within this date range will be returned.
    #' @param end_date_ends Together with \code{end_date_begins} defines a
    #'  date range for the end dates of the tag entries. Tag entries with an
    #'  end date within this date range will be returned.
    #' @param is_end_date_set Whether to return open or closed tag entries only.
    #'  If not specified or set to NULL all tag entries (closed and ongoing)
    #'  will be returned.
    #'
    #' @return A dataframe containing the tag entries or a list of length zero
    #'  if no tag entries for the specified filter are available.
    #'
    #' @examples \dontrun{
    #' tagEntries <- sp$tag_entries$load(tag_group_names = c("Info"))
    #' }
    load = function(entity_names = list(),
                    entity_set_name = "",
                    tag_names = list(),
                    tag_group_names = list(),
                    start = NULL,
                    end = NULL,
                    start_date_begins = NULL,
                    start_date_ends = NULL,
                    end_date_begins = NULL,
                    end_date_ends = NULL,
                    is_end_date_set = NULL) {
      filter <- list(
        Entities = as.list(entity_names),
        EntitySet = entity_set_name,
        Tags = as.list(tag_names),
        TagGroups = as.list(tag_group_names)
      )
      if (!is.null(start)) filter$Start <- start
      if (!is.null(end)) filter$End <- end
      if (!is.null(start_date_begins)) {
        filter$StartDateBegins <- start_date_begins
      }
      if (!is.null(start_date_ends)) filter$StartDateEnds <- start_date_ends
      if (!is.null(end_date_begins)) filter$EndDateBegins <- end_date_begins
      if (!is.null(end_date_ends)) filter$EndDateEnds <- end_date_ends
      if (!is.null(is_end_date_set)) filter$EndDateSet <- is_end_date_set

      ret <- httr::POST(
        paste0(
          private$url,
          "TagEntries/Filtered"
        ),
        body = jsonlite::toJSON(filter, auto_unbox = TRUE),
        content_type_json(),
        add_headers(Authorization = paste(private$token_type, private$token))
      )

      httr::stop_for_status(ret)
      cont <- httr::content(ret, as = "text")
      tag_entries <- jsonlite::fromJSON(cont)

      # If no tag entries available, return empty list
      if (length(tag_entries) == 0) return(tag_entries)

      if (ncol(tag_entries) == 3) {
        colnames(tag_entries) <- c("tag_name", "entity_name", "start")
      } else {
        colnames(tag_entries) <- c("tag_name", "entity_name", "start", "end")
      }

      return(tag_entries)
    }
  ),
  private = list(
    url = NULL,
    token_type = NULL,
    token = NULL
  )
)
