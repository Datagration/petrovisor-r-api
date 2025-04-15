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
#' warnings <- sp$logs$GetLogEntries(severity = "WARNING")
#' signIns <- sp$logs$GetLogEntries(category = "SignIn")
#'
#' # add log entry
#' entry <- LogEntry$new(message = "Test", category = "Tag")
#' sp$logs$AddLogEntry(entry)
#'
#' # add several log entries at once
#' entry1 <- LogEntry$new(message = "Test1", category = "Tag")
#' entry2 <- LogEntry$new(message = "Test2", category = "Tag")
#' sp$logs$AddLogEntries(list(entry1, entry2))
#'
#' # remove all log entries
#' sp$logs$CleanUpLogEntries()
#' }
LoggingService <- R6Class("LoggingService",
    public = list(

        #' @description Create a new LoggingService instance. This is done by the
        #' ServiceProvider automatically.
        #'
        #' @param url the URL for the API calls.
        #' @param tokenType the type of the issued token.
        #' @param token the issued token.
        initialize = function(url, tokenType, token) {
            private$url <- url
            private$tokenType <- tokenType
            private$token <- token
            private$severity <- append(list(NULL), list(
                "INFORMATION",
                "WARNING",
                "ERROR",
                "DEBUG",
                "ALARM"
            ))
        },

        #' @description Get all available categories from the existing log entries.
        #'
        #' @return A character vector containing all available categories.
        GetAvailableCategories = function() {
            ret <- httr::GET(
                paste0(
                    private$url,
                    "LogEntries/Categories"
                ),
                add_headers(Authorization = paste(private$tokenType, private$token))
            )
            httr::stop_for_status(ret)
            cont <- httr::content(ret, as = "text")
            return(jsonlite::fromJSON(cont))
        },

        #' @description Retrieve all log entrties matching the given filter from the
        #' database.
        #'
        #' @param lastEntries Return only the latest n tag entries.
        #' @param start Return log entries after the specified date (inclusive).
        #' @param end Return log entries before the specified date (inclusive).
        #' @param category Return log entries of the specified category.
        #' @param categories Return log entries of the specified categaories.
        #' @param userName Return log entries of the specified user.
        #' @param severity Return log entries of the given severity.
        #' @param logEntryContentContains Return log entries whose massage
        #' contains the specified text.
        #' @param entityName Return log entries of the given entity.
        #' @param signalName Return log entries of the specified signal.
        #' @param tagName Return log entries of the specified tag.
        #' @param unitName Return log entries of the given unit.
        #' @param scriptName Return log entries of the specified script.
        #'
        #' @return A dataframe containing the requested log entries. The number of
        #' returned columns depends on the available information. Columns that
        #' contain no information are not shown in the output.
        GetLogEntries = function(lastEntries = NULL,
                                 start = NULL,
                                 end = NULL,
                                 category = NULL,
                                 categories = NULL,
                                 userName = NULL,
                                 severity = NULL,
                                 logEntryContentContains = NULL,
                                 entityName = NULL,
                                 signalName = NULL,
                                 tagName = NULL,
                                 unitName = NULL,
                                 scriptName = NULL) {
            # check input arguments
            severity <- match.arg(severity, private$severity, FALSE)[[1]][1]

            # construct request body
            body <- list()
            if (!is.null(lastEntries)) {
                body <- append(body, list(LastEntries = lastEntries))
            }
            if (!is.null(start)) {
                body <- append(body, list(Start = start))
            }
            if (!is.null(end)) {
                body <- append(body, list(End = end))
            }
            if (!is.null(category)) {
                body <- append(body, list(Category = category))
            }
            if (!is.null(categories)) {
                body <- append(body, list(Categories = categories))
            }
            if (!is.null(userName)) {
                body <- append(body, list(User = userName))
            }
            if (!is.null(severity)) {
                body <- append(body, list(Severity = severity))
            }
            if (!is.null(logEntryContentContains)) {
                body <- append(
                    body,
                    list(logEntryContentContains = logEntryContentContains)
                )
            }
            if (!is.null(entityName)) {
                body <- append(body, list(Entity = entityName))
            }
            if (!is.null(signalName)) {
                body <- append(body, list(Signal = signalName))
            }
            if (!is.null(tagName)) {
                body <- append(body, list(Tag = tagName))
            }
            if (!is.null(unitName)) {
                body <- append(body, list(Unit = unitName))
            }
            if (!is.null(scriptName)) {
                body <- append(body, list(Script = scriptName))
            }

            if (length(body) == 0) {
                body <- {}
            }

            # get return data
            ret <- httr::POST(
                paste0(
                    private$url, "LogEntries/Filtered"
                ),
                body = jsonlite::toJSON(body, auto_unbox = TRUE),
                content_type_json(),
                add_headers(Authorization = paste(private$tokenType, private$token))
            )

            httr::stop_for_status(ret)
            cont <- httr::content(ret, as = "text")
            return(jsonlite::fromJSON(cont))
        },

        #' @description Add a logentry to the database.
        #'
        #' @param logEntry A logEntry-object created by instantiating the class
        #' 'LogEntry'.
        AddLogEntry = function(logEntry) {
            ret <- httr::POST(
                paste0(
                    private$url,
                    "LogEntries/AddMultiple"
                ),
                body = jsonlite::toJSON(logEntry$toList()),
                content_type_json(),
                add_headers(Authorization = paste(private$tokenType, private$token))
            )
            httr::stop_for_status(ret)
        },

        #' @description Add several log entries to the database at once.
        #'
        #' @param logEntries a list of logEntry-objects.
        AddLogEntries = function(logEntries) {
            # create dataframe from list of log entries
            dl <- list()
            for (i in 1:length(logEntries)) {
                dl[[i]] <- logEntries[[i]]$toList()
            }

            # add entries to database
            ret <- httr::POST(
                paste0(
                    private$url,
                    "LogEntries/AddMultiple"
                ),
                body = jsonlite::toJSON(df),
                content_type_json(),
                add_headers(Authorization = paste(private$tokenType, private$token))
            )
            httr::stop_for_status(ret)
        },

        #' @description Remove log entries from the database.
        #'
        #' @param daysToKeep Keep the log entries of the last n days in the log.
        #' Defaults to 0.
        #' @param category Remove log entries of the specified category only.
        CleanUpLogEntries = function(daysToKeep = 0, category) {
            # if no category is given, use the normal /Clean call
            # else use /CleanCategory
            if (missing(category)) {
                ret <- httr::POST(
                    paste0(
                        private$url,
                        "LogEntries/Clean"
                    ),
                    query = list(KeepTimeSpan = daysToKeep),
                    add_headers(Authorization = paste(private$tokenType, private$token))
                )
                httr::stop_for_status(ret)
            } else {
                ret <- httr::POST(
                    paste0(
                        private$url,
                        "LogEntries/CleanCategory"
                    ),
                    query = list(KeepTimeSpan = daysToKeep, Category = category),
                    add_headers(Authorization = paste(private$tokenType, private$token))
                )
                httr::stop_for_status(ret)
            }
        }
    ),
    private = list(
        url = NULL,
        tokenType = NULL,
        token = NULL,
        severity = NULL
    )
)
