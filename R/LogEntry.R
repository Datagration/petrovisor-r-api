library("R6")

#' @title LogEntry
#'
#' @description Class to define and create new log entries.
#'
#' @export LogEntry
#'
#' @field timestamp The timestamp of the tag entry. If none is specified, the
#'  current date and time will be inserted automatically.
#' @field message The message of the log entry.
#' @field category The category of the log entry. May be any string.
#' @field user_name If not specified, the current user will be inserted
#'  automatically.
#' @field severity The severity of the tag entry. One of: \code{Information},
#'  \code{Warning}, \code{Error}, \code{Debug}, \code{Alarm}.
#' @field workspace The workspace of the log entry. If none is specified,
#'  the current workspace will be inserted automatically.
#' @field item_name The name of the item the log entry is related to.
#' @field item_type The name of the item type the log entry is related to.
#' @field item_change The type of change the log entry is related to. One of:
#'  \code{Created}, \code{Modified}, \code{Deleted}, \code{Renamed},
#'  \code{Other}.
#' @field schedule_name The name of the schedule affected by the operation.
#' @field workflow_name The name of the workflow affected by the operation.
#' @field start_time The start time of an operation.
#' @field end_time The end time of an operation.
#' @field elapsed_time The time an operation needed to execute.
#' @field message_details Optional details of the message.
#' @field directory User directory (tenant).
#'
#' @examples
#' \dontrun{
#' LogEntry$new(category = "MyCategory",
#'              message = "This is the message",
#'              message_details = "and here are some details",
#'              severity = "Debug")
#' }
LogEntry <- R6Class("LogEntry",
  public = list(
    timestamp = NULL,
    message = NULL,
    category = NULL,
    user_name = NULL,
    severity = NULL,
    workspace = NULL,
    item_name = NULL,
    item_type = NULL,
    item_change = NULL,
    schedule_name = NULL,
    workflow_name = NULL,
    start_time = NULL,
    end_time = NULL,
    elapsed_time = NULL,
    message_details = NULL,
    directory = NULL,

    #' @description Create a new LogEntry instance.
    #'
    #' @param timestamp The timestamp of the tag entry. If none is specified,
    #'  the current date and time will be inserted automatically.
    #' @param message The message of the log entry.
    #' @param category The category of the log entry. May be any string.
    #' @param user_name If not specified, the current user will be inserted
    #'  automatically.
    #' @param severity The severity of the tag entry. One of:
    #'  \code{Information}, \code{Warning}, \code{Error}, \code{Debug},
    #'  \code{Alarm}.
    #' @param workspace The workspace of the log entry. If none is specified,
    #'  the current workspace will be inserted automatically.
    #' @param item_name The name of the item the log entry is related to.
    #' @param item_type The name of the item type the log entry is related to.
    #' @param item_change The type of change the log entry is related to.
    #'  One of: \code{Created}, \code{Modified}, \code{Deleted}, \code{Renamed},
    #'  \code{Other}.
    #' @param schedule_name The name of the schedule affected by the operation.
    #' @param workflow_name The name of the workflow affected by the operation.
    #' @param start_time The start time of an operation.
    #' @param end_time The end time of an operation.
    #' @param elapsed_time The time an operation needed to execute.
    #' @param message_details Optional details of the message.
    #' @param directory User directory (tenant).
    initialize = function(timestamp = NULL,
                          message = NULL,
                          category = NULL,
                          user_name = NULL,
                          severity = c("Information",
                                       "Warning",
                                       "Error",
                                       "Debug",
                                       "Alarm"),
                          workspace = NULL,
                          item_name = NULL,
                          item_type = c("Unknown",
                                        "Signal",
                                        "Unit",
                                        "EntityType",
                                        "BlobFile",
                                        "Entity",
                                        "EntitySet",
                                        "Hierarchy",
                                        "Calculation",
                                        "Workspace",
                                        "Tag",
                                        "Scope",
                                        "EventSubscription",
                                        "Context",
                                        "EventCalculation",
                                        "CleansingCalculation",
                                        "PSharpScript",
                                        "TicketTimeInterval",
                                        "Label",
                                        "UserGroup",
                                        "WorkflowSchedule",
                                        "Message",
                                        "FilterDefinition",
                                        "TagEntry",
                                        "Ticket",
                                        "DataConnection",
                                        "DataSource",
                                        "DataIntegrationSet",
                                        "Dashboard",
                                        "Plot",
                                        "Scenario",
                                        "Snapshot",
                                        "User",
                                        "ChartDefinition",
                                        "PivotTable",
                                        "DCA",
                                        "MLModel",
                                        "CustomWorkflowActivity",
                                        "RWorkflowActivity",
                                        "CleansingScript",
                                        "ProcessTemplate",
                                        "Workflow",
                                        "WorkspaceValue",
                                        "ReferenceTable",
                                        "WorkspacePackage",
                                        "PythonWorkflowActivity",
                                        "DataGrid",
                                        "WebWorkflowActivity",
                                        "PowerBIItem",
                                        "DataIntegrationSession",
                                        "UserSetting"),
                          item_change = c("Other",
                                          "Created",
                                          "Modified",
                                          "Deleted",
                                          "Renamed"),
                          schedule_name = NULL,
                          workflow_name = NULL,
                          start_time = NULL,
                          end_time = NULL,
                          elapsed_time = NULL,
                          message_details = NULL,
                          directory = NULL) {
      self$timestamp <- timestamp
      self$message <- message
      self$category <- category
      self$user_name <- user_name
      self$severity <- match.arg(severity)
      self$workspace <- workspace
      self$item_name <- item_name
      self$item_type <- match.arg(item_type)
      self$item_change <- match.arg(item_change)
      self$schedule_name <- schedule_name
      self$workflow_name <- workflow_name
      self$start_time <- start_time
      self$end_time <- end_time
      self$elapsed_time <- elapsed_time
      self$message_details <- message_details
      self$directory <- directory
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the LoggingService to convert the objects to lists and then call
    #' the web API.
    to_list = function() {
      dl <- list(
        Timestamp = if (is.null(self$timestamp)) {
          format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
        } else {
          self$timestamp
        },
        Message = if (is.null(self$message)) "" else self$message,
        Category = if (is.null(self$category)) "" else self$category,
        UserName = if (is.null(self$user_name)) "" else self$user_name,
        Severity = self$severity,
        Workspace = if (is.null(self$workspace)) "" else self$workspace,
        ItemName = if (is.null(self$item_name)) "" else self$item_name,
        ItemType = self$item_type,
        ItemChange = self$item_change,
        Schedule = if (is.null(self$schedule_name)) "" else self$schedule_name,
        Workflow = if (is.null(self$workflow_name)) "" else self$workflow_name,
        StartTime = if (is.null(self$start_time)) "" else self$start_time,
        EndTime = if (is.null(self$end_time)) "" else self$end_time,
        ElapsedTime = if (is.null(self$elapsed_time)) "" else self$elapsed_time,
        MessageDetails = if (is.null(self$message_details)) {
          ""
        } else {
          self$message_details
        },
        Directory = if (is.null(self$directory)) "" else self$directory
      )

      return(dl)
    }
  )
)
