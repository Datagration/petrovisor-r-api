library("R6")

#' @title LoggingService
#'
#' @description Provides access to logging functionality provided through the
#' web API.
#'
#' @details A new instance of this class will be created by the ServiceProvider
#' automatically.
#'
#' @seealso
#' * [ServiceProvider] for accessing the logging service via `sp$logs`
#' * [LogEntry] for log entry structure
#' * `vignette("getting-started")` for basic usage
#'
#' @export LoggingService
#'
#' @examples \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")
#'
#' # get available categories
#' availableCategories <- sp$logs$load_categories()
#'
#' # get log entries
#' allLogEntries <- sp$logs$load()
#' warnings <- sp$logs$load(severities = "Warning")
#' signIns <- sp$logs$load(categories = "SignIn")
#'
#' # add log entry
#' entry <- LogEntry$new(message = "Test",
#'                       category = "Tag",
#'                       severity = "Information")
#' sp$logs$save(list(entry))
#'
#' # add several log entries at once
#' entry1 <- LogEntry$new(message = "Test1",
#'                        category = "Tag",
#'                        severity = "Information")
#' entry2 <- LogEntry$new(message = "Test2",
#'                        category = "Tag",
#'                        severity = "Information")
#' sp$logs$save(list(entry1, entry2))
#' }
LoggingService <- R6Class( # nolint: object_name_linter
  "LoggingService",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(

    #' @description Create a new LoggingService instance. This is done by
    #'  the [ServiceProvider] automatically.
    #'
    #' @param url the URL for the API calls.
    #' @param token_type the type of the issued token.
    #' @param token the issued token.
    initialize = function() {},

    #' @description Get all available categories from the existing log
    #'  entries.
    #'
    #' @return A character vector containing all available categories.
    load_categories = function() {
      categories <- super$get(route = "LogEntries/Categories")

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
      log_entries <- super$post(body = body,
                                route = "LogEntries/Filtered",
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
      super$post(body = dl,
                 route = "LogEntries/AddMultiple")
    }
  )
)
