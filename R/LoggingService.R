library("R6")

#' @title LoggingService
#'
#' @description Provides access to logging functionality provided through the
#' web API.
#'
#' @details A new instance of this class will be created by the ServiceProvider
#' automatically.
#'
#' @export LoggingService
#'
#' @examples \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")
#'
#' # get available categories
#' availableCategories <- sp$logs$GetAvailableCategories()
#'
#' # get log entries
#' allLogEntries <- sp$logs$GetLogEntries()
#' warnings <- sp$logs$GetLogEntries(severity = "Warning")
#' signIns <- sp$logs$GetLogEntries(category = "SignIn")
#'
#' # add log entry
#' entry <- LogEntry$new(message = "Test",
#'                       category = "Tag",
#'                       severity = "Information")
#' sp$logs$AddLogEntry(entry)
#'
#' # add several log entries at once
#' entry1 <- LogEntry$new(message = "Test1",
#'                        category = "Tag",
#'                        severity = "Information")
#' entry2 <- LogEntry$new(message = "Test2",
#'                        category = "Tag",
#'                        severity = "Information")
#' sp$logs$AddLogEntries(list(entry1, entry2))
#'
#' # remove all log entries
#' sp$logs$CleanUpLogEntries()
#' }
LoggingService <- R6Class(
  "LoggingService",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(

    #' @description Create a new LoggingService instance. This is done by
    #'  the ServiceProvider automatically.
    #'
    #' @param url the URL for the API calls.
    #' @param token_type the type of the issued token.
    #' @param token the issued token.
    initialize = function(url, token_type, token) {
      private$url <- url
      private$token_type <- token_type
      private$token <- token
    },

    #' @description Get all available categories from the existing log
    #'  entries.
    #'
    #' @return A character vector containing all available categories.
    load_categories = function() {
      categories <- private$get(private$url,
                                "LogEntries/Categories",
                                private$token_type,
                                private$token)

      return(categories)
    },

    #' @description Retrieve all log entries matching the given filter from
    #'  the database.
    #'
    #' @param last_entries Return only the latest n tag entries.
    #' @param start Return log entries after the specified date (inclusive).
    #' @param end Return log entries before the specified date (inclusive).
    #' @param categories Return log entries of the specified categaories.
    #' @param user_name Return log entries of the specified user.
    #' @param severities Return log entries of the given severities.
    #' @param message_text_filter Return log entries whose massage
    #' contains the specified text.
    #' @param workflow Return log entries for the specified workflow.
    #' @param schedule Return log entries for the specified schedule.
    #'
    #' @return A dataframe containing the requested log entries. The number
    #'  of returned columns depends on the available information.
    #'  Columns that contain no information are not shown in the output.
    load = function(last_entries = NULL,
                    start = NULL,
                    end = NULL,
                    categories = NULL,
                    user_name = NULL,
                    severities = NULL,
                    message_text_filter = NULL,
                    workflow = NULL,
                    schedule = NULL) {

      # Construct request body
      body <- list()
      if (!is.null(last_entries)) body$LastEntries <- last_entries
      if (!is.null(start)) body$Start <- start
      if (!is.null(end)) body$End <- end
      if (!is.null(categories)) body$Categories <- as.list(categories)
      if (!is.null(user_name)) body$User <- user_name
      if (!is.null(severities)) body$Severities <- as.list(severities)
      if (!is.null(message_text_filter)) {
        body$LogEntryContentContains <- message_text_filter
      }
      if (!is.null(workflow)) body$Workflow <- workflow
      if (!is.null(schedule)) body$schedule <- schedule

      # get return data
      log_entries <- private$post(body,
                                  private$url,
                                  "LogEntries/Filtered",
                                  private$token_type,
                                  private$token,
                                  expect_data = TRUE)
      return(log_entries)
    },

    #' @description Add one or several log entries to the database at once.
    #'
    #' @param log_entries a list of LogEntry-objects.
    save = function(log_entries) {
      # Create dataframe from list of log entries
      dl <- list()
      for (i in seq_along(log_entries)) {
        dl[[i]] <- log_entries[[i]]$to_list()
      }

      # add entries to database
      private$post(dl,
                   private$url,
                   "LogEntries/AddMultiple",
                   private$token_type,
                   private$token)
    },

    #' @description Remove log entries from the database.
    #'
    #' @param days_to_keep Keep the log entries of the last n days in the
    #'  log. Defaults to 0.
    #' @param category Remove log entries of the specified category only.
    delete = function(days_to_keep = 0, category) {
      # if no category is given, use the normal /Clean call
      # else use /CleanCategory
      if (missing(category)) {
        query <- list(KeepTimeSpan = days_to_keep)
        route <- "LogEntries/Clean"
      } else {
        query <- list(KeepTimeSpan = days_to_keep,
                      Category = category)
        route <- "LogEntries/CleanCategory"
      }

      private$post(NULL,
                   private$url,
                   route,
                   private$token_type,
                   private$token,
                   expect_data = FALSE,
                   query = query)
    }
  ),
  private = list(
    url = NULL,
    token_type = NULL,
    token = NULL
  )
)
