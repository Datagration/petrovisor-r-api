library("R6")

#' @title LogEntry
#'
#' @description Class to define and create new log entries.
#'
#' @export LogEntry
#'
#' @field category The category of the log entry. May be any string.
#' @field elapsedTime The time an operation needed to execute.
#' @field endTime The end time of an operation.
#' @field entityName Thename of the entity affected by the operation.
#' @field message The message of the log entry.
#' @field messageDetails Details of the log entry message.
#' @field numberOfItems The number of items changed by the operation.
#' @field scheduleName The name of the schedule affected by the operation.
#' @field scriptName The name of the script affected by the operation
#' @field severity The severity of the tag entry.
#' @field signalName The name of the signal affected by the operation.
#' @field startTime The start time of an operation.
#' @field tagName The name of the tag affected by the operation.
#' @field timestamp The timestamp of the tag entry. If none is specified, the
#' current date and time will be inserted automatically.
#' @field unitName The name of the unit affected by the operation.
#' @field userName If not specified, the current user will be inserted
#' automatically.
#' @field valueAfter The value after a value change.
#' @field valueBefore The value before a value change.
#' @field workflowName The name of the executed workflow. May be any string.
#' @field workspace The workspace of the log entry. If none is specified,
#' the current workspace will be inserted automatically.
#'
#' @examples
#' \dontrun{
#' LogEntry$new(category = "MyCategory",
#'              message = "This is the message",
#'              messageDetails = "and here are some details")
#' }
LogEntry <- R6Class("LogEntry",
  public = list(
    category = NULL,
    elapsedTime = NULL,
    endTime = NULL,
    entityName = NULL,
    message = NULL,
    messageDetails = NULL,
    numberOfItems = NULL,
    scheduleName = NULL,
    scriptName = NULL,
    severity = NULL,
    signalName = NULL,
    startTime = NULL,
    tagName = NULL,
    timestamp = NULL,
    unitName = NULL,
    userName = NULL,
    valueAfter = NULL,
    valueBefore = NULL,
    workflowName = NULL,
    workspace = NULL,

    #' @description Create a new LogEntry instance.
    #'
    #' @param category The category of the log entry. May be any string.
    #' @param elapsedTime The time an operation needed to execute.
    #' @param endTime The end time of an operation.
    #' @param entityName Thename of the entity affected by the operation.
    #' @param message The message of the log entry.
    #' @param messageDetails Details of the log entry message.
    #' @param numberOfItems The number of items changed by the operation.
    #' @param scheduleName The name of the schedule affected by the operation.
    #' @param scriptName The name of the script affected by the operation
    #' @param severity The severity of the tag entry.
    #' @param signalName The name of the signal affected by the operation.
    #' @param startTime The start time of an operation.
    #' @param tagName The name of the tag affected by the operation.
    #' @param timestamp The timestamp of the tag entry. If none is specified,
    #' the current date and time will be inserted automatically.
    #' @param unitName The name of the unit affected by the operation.
    #' @param userName If not specified, the current user will be inserted
    #' automatically.
    #' @param valueAfter The value after a value change.
    #' @param valueBefore The value before a value change.
    #' @param workflowName The name of the executed workflow. May be any string.
    #' @param workspace The workspace of the log entry. If none is specified,
    #' the current workspace will be inserted automatically.
    initialize = function(category = NULL,
                          elapsedTime = NULL,
                          endTime = NULL,
                          entityName = NULL,
                          message = NULL,
                          messageDetails = NULL,
                          numberOfItems = NULL,
                          scheduleName = NULL,
                          scriptName = NULL,
                          severity = NULL,
                          signalName = NULL,
                          startTime = NULL,
                          tagName = NULL,
                          timestamp = NULL,
                          unitName = NULL,
                          userName = NULL,
                          valueAfter = NULL,
                          valueBefore = NULL,
                          workflowName = NULL,
                          workspace = NULL){
      self$category <- category
      self$elapsedTime <- elapsedTime
      self$endTime <- endTime
      self$entityName <- entityName
      self$message <- message
      self$messageDetails <- messageDetails
      self$numberOfItems <- numberOfItems
      self$scheduleName <- scheduleName
      self$scriptName <- scriptName
      self$severity <- severity
      self$signalName <- signalName
      self$startTime <- startTime
      self$tagName <- tagName
      self$timestamp <- timestamp
      self$unitName <- unitName
      self$userName <- userName
      self$valueAfter <- valueAfter
      self$valueBefore <- valueBefore
      self$workflowName <- workflowName
      self$workspace <- workspace
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the LoggingService to convert the objects to lists and then call
    #' the web API.
    toList = function(){
      dl <- list(
        Timestamp = if(is.null(self$timestamp)) "" else self$timestamp,
        Message = if(is.null(self$message)) "" else self$message,
        Category = if(is.null(self$category)) "" else self$category,
        UserName = if(is.null(self$userName)) "" else self$userName,
        Severity = if(is.null(self$severity)) "" else self$severity,
        Workspace = if(is.null(self$workspace)) "" else self$workspace,
        Schedule = if(is.null(self$scheduleName)) "" else self$scheduleName,
        Workflow = if(is.null(self$workflowName)) "" else self$workflowName,
        StartTime = if(is.null(self$startTime)) "" else self$startTime,
        EndTime = if(is.null(self$endTime)) "" else self$endTime,
        Script = if(is.null(self$scriptName)) "" else self$scriptName,
        Entity = if(is.null(self$entityName)) "" else self$entityName,
        Signal = if(is.null(self$signalName)) "" else self$signalName,
        Unit = if(is.null(self$unitName)) "" else self$unitName,
        Tag = if(is.null(self$tagName)) "" else self$tagName,
        NumberOfItems = if(is.null(self$numberOfItems)) "" else self$numberOfItems,
        ValueBefore = if(is.null(self$valueBefore)) "" else self$valueBefore,
        ValueAfter = if(is.null(self$valueAfter)) "" else self$valueAfter,
        ElapsedTime = if(is.null(self$elapsedTime)) "" else self$elapsedTime,
        MessageDetails = if(is.null(self$messageDetails)) "" else self$messageDetails)
      return(dl)
    }
  )
)
