library("R6")

#' @title Workflow
#'
#' @description Class representing a PetroVisor workflow object.
#'
#' @export Workflow
#'
#' @field name The name of the workflow.
#' @field activities A list of activities. The items in the list must be
#'   objects of the class \code{WorkflowActivity}.
#' @field isLocked This flag specifies whether the workflow is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the workflow belongs to.
#' @field isFavorite This flag specifies whether the workflow is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the workflow.
#' @examples
#' \dontrun{
#' Workflow$new()
#'}
Workflow <- R6Class("Workflow",
  public = list(
    name = NULL,
    activities = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Workflow instance.
    #'
    #' @param name The name of the workflow.
    #' @param activities A list of activities. The items in the list must be
    #'   objects of the class \code{WorkflowActivity}.
    #' @param isLocked This flag specifies whether the workflow is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the workflow belongs to.
    #' @param isFavorite This flag specifies whether the workflow is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module in
    #'   PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the workflow.
    initialize = function(name = NULL,
                          activities = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$activities <- activities
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of activities
      activityList <- list()
      if(!is.null(self$activities)){
        for (i in 1:length(self$activities)){
          activityList[[i]] <- self$activities[[i]]$toList()
        }
      } else {
        activityList[[1]] <- ""
      }

      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Activities = if(is.null(self$activities)) "" else activityList,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)

#' @title WorkflowActivity
#'
#' @description Class representing a workflow activity object.
#'
#' @export WorkflowActivity
#'
#' @field activityType String giving the activity type.
#' @field activityName The activity's name.
#' @field settings The activity's settings as string.
#' @field mappedInputArguments A list of mapped input arguments. The items in
#'   the list must be objects of the class \code{ActivityMappedArgument}.
#' @field mappedOutputArguments a list of mapped output arguments. The items
#'   in the list must be objects of the class \code{ActivityMappedArgument}.
#' @field inputArgumentsInfo A list of argument information. The items in the
#'   list must be objects of the class \code{ActivityArgument}.
#' @examples
#' \dontrun{
#' WorkflowActivity$new()
#'}
WorkflowActivity <- R6Class("WorkflowActivity",
  public = list(
    activityType = NULL,
    activityName = NULL,
    settings = NULL,
    mappedInputArguments = NULL,
    mappedOutputArguments = NULL,
    inputArgumentsInfo = NULL,

    #' @description Create a new workflow activity instance.
    #'
    #' @param activityType String giving the activity type.
    #' @param activityName The activity's name.
    #' @param settings The activity's settings as string.
    #' @param mappedInputArguments A list of mapped input arguments. The items
    #'   in the list must be objects of the class \code{ActivityMappedArgument}.
    #' @param mappedOutputArguments a list of mapped output arguments. The items
    #'   in the list must be objects of the class \code{ActivityMappedArgument}.
    #' @param inputArgumentsInfo A list of argument information. The items in
    #'   the list must be objects of the class \code{ActivityArgument}.
    initialize = function(activityType = NULL,
                          activityName = NULL,
                          settings = NULL,
                          mappedInputArguments = NULL,
                          mappedOutputArguments = NULL,
                          inputArgumentsInfo = NULL){
      self$activityType <- activityType
      self$activityName <- activityName
      self$settings <- settings
      self$mappedInputArguments <- mappedInputArguments
      self$mappedOutputArguments <- mappedOutputArguments
      self$inputArgumentsInfo <- inputArgumentsInfo
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of mappedInputArguments
      mappedInputArgumentsList <- list()
      if(!is.null(self$mappedInputArguments)){
        for (i in 1:length(self$mappedInputArguments)){
          mappedInputArgumentsList[[i]] <- self$mappedInputArguments[[i]]$toList()
        }
      } else {
        mappedInputArgumentsList[[1]] <- ""
      }

      # create list from list of mappedOutputArguments
      mappedOutputArgumentsList <- list()
      if(!is.null(self$mappedOutputArguments)){
        for (i in 1:length(self$mappedOutputArguments)){
          mappedOutputArgumentsList[[i]] <-
            self$mappedOutputArguments[[i]]$toList()
        }
      } else {
        mappedOutputArgumentsList[[1]] <- ""
      }

      # create list from list of inputArgumentsInfo
      inputArgumentsInfoList <- list()
      if(!is.null(self$inputArgumentsInfo)){
        for (i in 1:length(self$inputArgumentsInfo)){
          inputArgumentsInfoList[[i]] <- self$inputArgumentsInfo[[i]]$toList()
        }
      } else {
        inputArgumentsInfoList[[1]] <- ""
      }

      dl <- list(
        ActivityType = if(is.null(self$activityType)) "" else self$activityType,
        ActivityName = if(is.null(self$activityName)) "" else self$activityName,
        Settings = if(is.null(self$settings)) "" else self$settings,
        MappedInputArguments = if(is.null(self$mappedInputArguments)) {
          ""
        } else {
          mappedInputArgumentsList
        },
        MappedOutputArguments = if(is.null(self$mappedOutputArguments)) {
          ""
        } else {
          mappedOutputArgumentsList
        },
        InputArgumentsInfo = if(is.null(self$inputArgumentsInfo)) {
          ""
        } else {
          inputArgumentsInfoList
        }
      )
      return(dl)
    }
  )
)

#' @title CustomWorkflowActivity
#'
#' @description Class representing a custom workflow activity object.
#'
#' @export CustomWorkflowActivity
#'
#' @field name The name of the custom workflow activity.
#' @field className The custom workflow activity's class name.
#' @field assemblyContent The content of the custom workflow activity.
#' @examples
#' \dontrun{
#' CustomWorkflowActivity$new()
#'}
CustomWorkflowActivity <- R6Class("CustomWorkflowActivity",
  public = list(
    name = NULL,
    className = NULL,
    assemblyContent = NULL,

    #' @description Create a new custom workflow activity instance.
    #'
    #' @param name The name of the custom workflow activity.
    #' @param className The custom workflow activity's class name.
    #' @param assemblyContent The content of the custom workflow activity.
    initialize = function(name = NULL,
                          className = NULL,
                          assemblyContent = NULL){
      self$name <- name
      self$className <- className
      self$assemblyContent <- assemblyContent
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        ClassName = if(is.null(self$className)) "" else self$className,
        AssemblyContent = if(is.null(self$assemblyContent)) {
          ""
        } else {
          self$assemblyContent
        }
      )
      return(dl)
    }
  )
)

#' @title RWorkflowActivity
#'
#' @description Class representing an R workflow activity object.
#'
#' @export RWorkflowActivity
#'
#' @field name The name of the R workflow activity.
#' @field providerConfiguration The configuration of the provider. Must be an
#'   object of the class \code{RProviderConfiguration}.
#' @field scriptName The name of the activity's R script.
#' @field functionName The name of the activity's R function. The function has
#'   to present in the referenced R script.
#' @field input A list of input arguments for the activity. The items in the
#'   list must be objects of the class \code{ActivityArgument}.
#' @field output A list of output arguments for the activity. The items in the
#'   list must be objects of the class \code{ActivityArgument}.
#' @examples
#' \dontrun{
#' RWorkflowActivity$new()
#'}
RWorkflowActivity <- R6Class("RWorkflowActivity",
  public = list(
    name = NULL,
    providerConfiguration = NULL,
    scriptName = NULL,
    functionName = NULL,
    input = NULL,
    output = NULL,

    #' @description Create a new R workflow activity instance.
    #'
    #' @param name The name of the R workflow activity.
    #' @param providerConfiguration The configuration of the provider. Must be
    #'   an object of the class \code{RProviderConfiguration}.
    #' @param scriptName The name of the activity's R script.
    #' @param functionName The name of the activity's R function. The function
    #'   has to present in the referenced R script.
    #' @param input A list of input arguments for the activity. The items in the
    #'   list must be objects of the class \code{ActivityArgument}.
    #' @param output A list of output arguments for the activity. The items in
    #'   the list must be objects of the class \code{ActivityArgument}.
    initialize = function(name = NULL,
                          providerConfiguration = NULL,
                          scriptName = NULL,
                          functionName = NULL,
                          input = NULL,
                          output = NULL){
      self$name <- name
      self$providerConfiguration <- providerConfiguration
      self$scriptName <- scriptName
      self$functionName <- functionName
      self$input <- input
      self$output <- output
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of inputs
      inputList <- list()
      if(!is.null(self$input)){
        for (i in 1:length(self$input)){
          inputList[[i]] <- self$input[[i]]$toList()
        }
      } else {
        inputList[[1]] <- ""
      }

      # create list from list of outputs
      outputList <- list()
      if(!is.null(self$output)){
        for (i in 1:length(self$output)){
          outputList[[i]] <- self$output[[i]]$toList()
        }
      } else {
        outputList[[1]] <- ""
      }

      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        ProviderConfiguration = if(is.null(self$providerConfiguration)) {
          ""
        } else {
          self$providerConfiguration$toList()
        },
        ScriptName = if(is.null(self$scriptName)) "" else self$scriptName,
        FunctionName = if(is.null(self$functionName)) "" else self$functionName,
        Input = if(is.null(self$input)) "" else inputList,
        Output = if(is.null(self$output)) "" else outputList
      )
      return(dl)
    }
  )
)

#' @title RProviderConfiguration
#'
#' @description Class representing an R provider configuration object.
#'
#' @export RProviderConfiguration
#'
#' @field providerType A string specifying the provider type. Allowed values
#'  are: \code{RNET} and \code{Rserve}.
#' @field rServerAddress The URL or host name of the server running R.
#' @field rServerPort The port Rserve is available at.
#' @field rServerScriptsFolder The path to the folder containing the scripts.
#' @examples
#' \dontrun{
#' RProviderConfiguration$new()
#'}
RProviderConfiguration <- R6Class("RProviderConfiguration",
  public = list(
    providerType = NULL,
    rServerAddress = NULL,
    rServerPort = NULL,
    rServerScriptsFolder = NULL,

    #' @description Create a new R provider configuration instance.
    #'
    #' @param providerType A string specifying the provider type. Allowed values
    #'  are: \code{RNET} and \code{Rserve}.
    #' @param rServerAddress The URL or host name of the server running R.
    #' @param rServerPort The port Rserve is available at.
    #' @param rServerScriptsFolder The path to the folder containing the
    #'   scripts.
    initialize = function(providerType = c("RNET", "Rserve"),
                          rServerAddress = NULL,
                          rServerPort = NULL,
                          rServerScriptsFolder = NULL){
      self$providerType <- match.arg(providerType)
      self$rServerAddress <- rServerAddress
      self$rServerPort <- rServerPort
      self$rServerScriptsFolder <- rServerScriptsFolder
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        ProviderType = if(is.null(self$providerType)) "" else self$providerType,
        RServerAddress = if(is.null(self$rServerAddress)) {
          ""
        } else {
          self$rServerAddress
        },
        RServerPort = if(is.null(self$rServerPort)) "" else self$rServerPort,
        RServerScriptsFolder = if(is.null(self$rServerScriptsFolder)) {
          ""
        } else {
          self$rServerScriptsFolder
        }
      )
      return(dl)
    }
  )
)

#' @title ActivityArgument
#'
#' @description Class representing an activity argument object.
#'
#' @export ActivityArgument
#'
#' @field argumentName The name of the argument.
#' @field acceptableArgumentTypes A list of strings containing the acceptable
#'   argument types.
#' @field defaultArgumentType The default argument type. One of:
#'   \code{TimeSignalUnit}, \code{DepthSignalUnit}, \code{StaticSignalUnit},
#'   \code{StringSignalUnit}, \code{StringTimeSignalUnit}, \code{PVTSignalUnit},
#'   \code{Hierarchy}, \code{TagEntries}, \code{WorkspaceValue}, \code{Model}
#' @field defaultSignalName The name of the default signal.
#' @field defaultUnitName The name of the default unit.
#' @field defaultString The default string.
#' @examples
#' \dontrun{
#' ActivityArgument$new()
#'}
ActivityArgument <- R6Class("ActivityArgument",
  public = list(
    argumentName = NULL,
    acceptableArgumentTypes = NULL,
    defaultArgumentType = NULL,
    defaultSignalName = NULL,
    defaultUnitName = NULL,
    defaultString = NULL,

    #' @description Create a new activity argument instance.
    #'
    #' @param argumentName The name of the argument.
    #' @param acceptableArgumentTypes A list of strings containing the
    #'   acceptable argument types.
    #' @param defaultArgumentType The default argument type. One of:
    #'   \code{TimeSignalUnit}, \code{DepthSignalUnit}, \code{StaticSignalUnit},
    #'   \code{StringSignalUnit}, \code{StringTimeSignalUnit},
    #'   \code{PVTSignalUnit}, \code{Hierarchy}, \code{TagEntries},
    #'   \code{WorkspaceValue}, \code{Model}
    #' @param defaultSignalName The name of the default signal.
    #' @param defaultUnitName The name of the default unit.
    #' @param defaultString The default string.
    initialize = function(argumentName = NULL,
                          acceptableArgumentTypes = NULL,
                          defaultArgumentType = c("TimeSignalUnit",
                                                  "DepthSignalUnit",
                                                  "StaticSignalUnit",
                                                  "StringSignalUnit",
                                                  "StringTimeSignalUnit",
                                                  "PVTSignalUnit",
                                                  "Hierarchy",
                                                  "TagEntries",
                                                  "WorkspaceValue",
                                                  "Model"),
                          defaultSignalName = NULL,
                          defaultUnitName = NULL,
                          defaultString = NULL){
      self$argumentName <- argumentName
      self$acceptableArgumentTypes <- acceptableArgumentTypes
      self$defaultArgumentType <- match.arg(defaultArgumentType)
      self$defaultSignalName <- defaultSignalName
      self$defaultUnitName <- defaultUnitName
      self$defaultString <- defaultString
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        ArgumentName = if(is.null(self$argumentName)) "" else self$argumentName,
        AcceptableArgumentTypes = if(is.null(self$acceptableArgumentTypes)) {
          ""
        } else {
          self$acceptableArgumentTypes
        },
        DefaultArgumentType = if(is.null(self$defaultArgumentType)) {
          ""
        } else {
          self$defaultArgumentType
        },
        DefaultSignal = if(is.null(self$defaultSignalName)) {
          ""
        } else {
          self$defaultSignalName
        },
        DefaultUnit = if(is.null(self$defaultUnitName)) {
          ""
        } else {
          self$defaultUnitName
        },
        DefaultString = if(is.null(self$defaultString)) {
          ""
        } else {
          self$defaultString
        }
      )
      return(dl)
    }
  )
)

#' @title ActivityMappedArgument
#'
#' @description Class representing an activity mapped argument object.
#'
#' @export ActivityMappedArgument
#'
#' @field argumentName The name of the argument.
#' @field argumentType The argument type. One of:
#'   \code{TimeSignalUnit}, \code{DepthSignalUnit}, \code{StaticSignalUnit},
#'   \code{StringSignalUnit}, \code{StringTimeSignalUnit}, \code{PVTSignalUnit},
#'   \code{Hierarchy}, \code{TagEntries}, \code{WorkspaceValue}, \code{Model}
#' @field acceptableArgumentTypes A list of strings containing the acceptable
#'   argument types.
#' @field mappedSignalName The name of the mapped signal.
#' @field mappedUnitName The name of the mapped unit.
#' @field mappedString The mapped string.
#' @examples
#' \dontrun{
#' ActivityMappedArgument$new()
#'}
ActivityMappedArgument <- R6Class("ActivityMappedArgument",
  public = list(
    argumentName = NULL,
    argumentType = NULL,
    acceptableArgumentTypes = NULL,
    mappedSignalName = NULL,
    mappedUnitName = NULL,
    mappedString = NULL,

    #' @description Create a new activity argument instance.
    #'
    #' @param argumentName The name of the argument.
    #' @param argumentType The argument type. One of:
    #'   \code{TimeSignalUnit}, \code{DepthSignalUnit}, \code{StaticSignalUnit},
    #'   \code{StringSignalUnit}, \code{StringTimeSignalUnit},
    #'   \code{PVTSignalUnit}, \code{Hierarchy}, \code{TagEntries},
    #'   \code{WorkspaceValue}, \code{Model}
    #' @param acceptableArgumentTypes A list of strings containing the
    #'   acceptable argument types.
    #' @param mappedSignalName The name of the mapped signal.
    #' @param mappedUnitName The name of the mapped unit.
    #' @param mappedString The mapped string.
    initialize = function(argumentName = NULL,
                          argumentType = c("TimeSignalUnit",
                                           "DepthSignalUnit",
                                           "StaticSignalUnit",
                                           "StringSignalUnit",
                                           "StringTimeSignalUnit",
                                           "PVTSignalUnit",
                                           "Hierarchy",
                                           "TagEntries",
                                           "WorkspaceValue",
                                           "Model"),
                          acceptableArgumentTypes = NULL,
                          mappedSignalName = NULL,
                          mappedUnitName = NULL,
                          mappedString = NULL){
      self$argumentName <- argumentName
      self$argumentType <- match.arg(argumentType)
      self$acceptableArgumentTypes <- acceptableArgumentTypes
      self$mappedSignalName <- mappedSignalName
      self$mappedUnitName <- mappedUnitName
      self$mappedString <- mappedString
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        ArgumentName = if(is.null(self$argumentName)) "" else self$argumentName,
        ArgumentType = if(is.null(self$argumentType)) "" else self$argumentType,
        AcceptableArgumentTypes = if(is.null(self$acceptableArgumentTypes)) {
          ""
        } else {
          self$acceptableArgumentTypes
        },

        MappedSignal = if(is.null(self$mappedSignalName)) {
          ""
        } else {
          self$mappedSignalName
        },
        MappedUnit = if(is.null(self$mappedUnitName)) {
          ""
        } else {
          self$mappedUnitName
        },
        MappedString = if(is.null(self$mappedString)) {
          ""
        } else {
          self$mappedString
        }
      )
      return(dl)
    }
  )
)

#' @title WorkflowSchedule
#'
#' @description Class representing a PetroVisor workflow schedule object.
#'
#' @export WorkflowSchedule
#'
#' @field name The name of the item.
#' @field modified The date the workflow schedule was modified (string).
#' @field scheduleName The name of the workflow schedule.
#' @field workflowName The name of the workflow associated with the schedule.
#' @field workspaceName The workspace in which the schedule is saved.
#' @field active This flag specifies whether the schedule is active.
#' @field processingEntitySetName The name of the processing entity set.
#' @field processingScopeName The name of the processing scope.
#' @field processingContextNames A list of strings holding the names of the
#'   processing contexts.
#' @field absoluteDate The date at which the schedule will be executed (string).
#' @field executedWorkflowName The name of the executed workflow.
#' @field executedWorkflowScheduleName The name of the executed workflow
#'  schedule.
#' @field scheduleStart The start date of the schedule (string).
#' @field scheduleEnd The end of the schedule. Must be an object of the class
#'  \code{ScheduleEnd}.
#' @field scheduleRecurrence Settings defining the schedule's recurrence.
#'   Must be an object of type \code{ScheduleRecurrence}.
#' @field isLocked This flag specifies whether the schedule is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the schedule belongs to.
#' @field isFavorite This flag specifies whether the schedule is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the schedule.
#' @examples
#' \dontrun{
#' WorkflowSchedule$new()
#'}
WorkflowSchedule <- R6Class("WorkflowSchedule",
  public = list(
    name = NULL,
    modified = NULL,
    scheduleName = NULL,
    workflowName = NULL,
    workspaceName = NULL,
    active = NULL,
    processingEntitySetName = NULL,
    processingScopeName = NULL,
    processingContextNames = NULL,
    absoluteDate = NULL,
    executedWorkflowName = NULL,
    executedWorkflowScheduleName = NULL,
    scheduleStart = NULL,
    scheduleEnd = NULL,
    scheduleRecurrence = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Workflow schedule instance.
    #'
    #' @param name The name of the item.
    #' @param modified The date the workflow schedule was modified (string).
    #' @param scheduleName The name of the workflow schedule.
    #' @param workflowName The name of the workflow associated with the schedule.
    #' @param workspaceName The workspace in which the schedule is saved.
    #' @param active This flag specifies whether the schedule is active.
    #' @param processingEntitySetName The name of the processing entity set.
    #' @param processingScopeName The name of the processing scope.
    #' @param processingContextNames A list of strings holding the names of the
    #'   processing contexts.
    #' @param absoluteDate The date at which the schedule will be executed (string).
    #' @param executedWorkflowName The name of the executed workflow.
    #' @param executedWorkflowScheduleName The name of the executed workflow
    #'  schedule.
    #' @param scheduleStart The start date of the schedule (string).
    #' @param scheduleEnd The end of the schedule. Must be an object of the class
    #'  \code{ScheduleEnd}.
    #' @param scheduleRecurrence Settings defining the schedule's recurrence.
    #'   Must be an object of type \code{ScheduleRecurrence}.
    #' @param isLocked This flag specifies whether the schedule is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the schedule belongs to.
    #' @param isFavorite This flag specifies whether the schedule is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module in
    #'   PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the schedule.
    initialize = function(name = NULL,
                          modified = NULL,
                          scheduleName = NULL,
                          workflowName = NULL,
                          workspaceName = NULL,
                          active = NULL,
                          processingEntitySetName = NULL,
                          processingScopeName = NULL,
                          processingContextNames = NULL,
                          absoluteDate = NULL,
                          executedWorkflowName = NULL,
                          executedWorkflowScheduleName = NULL,
                          scheduleStart = NULL,
                          scheduleEnd = NULL,
                          scheduleRecurrence = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$modified <- modified
      self$scheduleName <- scheduleName
      self$workflowName <- workflowName
      self$workspaceName <- workspaceName
      self$active <- active
      self$processingEntitySetName <- processingEntitySetName
      self$processingScopeName <- processingScopeName
      self$processingContextNames <- processingContextNames
      self$absoluteDate <- absoluteDate
      self$executedWorkflowName <- executedWorkflowName
      self$executedWorkflowScheduleName <- executedWorkflowScheduleName
      self$scheduleStart <- scheduleStart
      self$scheduleEnd <- scheduleEnd
      self$scheduleRecurrence <- scheduleRecurrence
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Modified = if(is.null(self$modified)) "" else self$modified,
        ScheduleName = if(is.null(self$scheduleName)) "" else self$scheduleName,
        WorkflowName = if(is.null(self$workflowName)) "" else self$workflowName,
        WorkspaceName = if(is.null(self$workspaceName)) {
          ""
        } else {
          self$workspaceName
        },
        Active = if(is.null(self$active)) "" else self$active,
        ProcessingEntitySet = if(is.null(self$processingEntitySetName)) {
          ""
        } else {
          self$processingEntitySetName
        },
        ProcessingScope = if(is.null(self$processingScopeName)) {
          ""
        } else {
          self$processingScopeName
        },
        ProcessingContexts = if(is.null(self$processingContextNames)) {
          ""
        } else {
          self$processingContextNames
        },
        AbsoluteDate = if(is.null(self$absoluteDate)) "" else self$absoluteDate,
        ExecutedWorkflowName = if(is.null(self$executedWorkflowName)) {
          ""
        } else {
          self$executedWorkflowName
        },
        ExecutedWorkflowScheduleName =
          if(is.null(self$executedWorkflowScheduleName)) {
            ""
          } else {
            self$executedWorkflowScheduleName
          },
        ScheduleStart = if(is.null(self$scheduleStart)) {
          ""
        } else {
          self$scheduleStart
        },
        ScheduleEnd = if(is.null(self$scheduleEnd)) {
          ""
        } else {
          self$scheduleEnd$toList()
        },
        ScheduleRecurrence = if(is.null(self$scheduleRecurrence)) {
          ""
        } else {
          self$scheduleRecurrence$toList()
        },
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)

#' @title ScheduleEnd
#'
#' @description Class representing a schedule end object.
#'
#' @export ScheduleEnd
#'
#' @field endType The type of the schedule end (integer).
#' @field repetitionsCount The number of times the schedule was executed.
#' @field totalRepetitions The number of times the schedule shall be executed.
#' @field endDate The date when the schedule shall end.
#' @examples
#' \dontrun{
#' ScheduleEnd$new()
#'}
ScheduleEnd <- R6Class("ScheduleEnd",
  public = list(
    endType = NULL,
    repetitionsCount = NULL,
    totalRepetitions = NULL,
    endDate = NULL,

    #' @description Create a new schedule end instance.
    #'
    #' @param endType The type of the schedule end (integer).
    #' @param repetitionsCount The number of times the schedule was executed.
    #' @param totalRepetitions The number of times the schedule shall be
    #'   executed.
    #' @param endDate The date when the schedule shall end.
    initialize = function(endType = NULL,
                          repetitionsCount = NULL,
                          totalRepetitions = NULL,
                          endDate = NULL){
      self$endType <- endType
      self$repetitionsCount <- repetitionsCount
      self$totalRepetitions <- totalRepetitions
      self$endDate <- endDate
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        EndType = if(is.null(self$endType)) "" else self$endType,
        RepetitionsCount = if(is.null(self$repetitionsCount)) {
          ""
        } else {
          self$repetitionsCount
        },
        TotalRepetitions = if(is.null(self$totalRepetitions)) {
          ""
        } else {
          self$totalRepetitions
        },
        EndDate = if(is.null(self$endDate)) "" else self$endDate
      )
      return(dl)
    }
  )
)

#' @title ScheduleRecurrence
#'
#' @description Class representing a schedule recurrence object.
#'
#' @export ScheduleRecurrence
#'
#' @field recurrenceType The type of the recurrence (string).
#' @field intervalsAmount Run the schedule every n days.
#' @field skipWeekend Flag specifying whether to skip execution on weekends.
#' @field repeatOnSpecificDayMonth Flag specifying whether to reapeat the
#'   schedule on a specific day of a month.
#' @field specificDayNumber The number of the day the schedule shall be repeated
#'   on if \code{repeatOnSpecificDayMonth} is \code{TRUE}.
#' @field orderNumberDay ??
#' @field typeDayMonthDay ??
#' @field runOnMonday Flag specifying if the schedule is run on Mondays.
#' @field runOnTuesday Flag specifying if the schedule is run on Tuesdays.
#' @field runOnWednesday Flag specifying if the schedule is run on Wednesdays.
#' @field runOnThursday Flag specifying if the schedule is run on Thursdays.
#' @field runOnFriday Flag specifying if the schedule is run on Fridays.
#' @field runOnSaturday Flag specifying if the schedule is run on Saturdays.
#' @field runOnSunday Flag specifying if the schedule is run on Sundays.
#' @examples
#' \dontrun{
#' ScheduleRecurrence$new()
#'}
ScheduleRecurrence <- R6Class("ScheduleRecurrence",
  public = list(
    recurrenceType = NULL,
    intervalsAmount = NULL,
    skipWeekend = NULL,
    repeatOnSpecificDayMonth = NULL,
    specificDayNumber = NULL,
    orderNumberDay = NULL,
    typeDayMonthDay = NULL,
    runOnMonday = NULL,
    runOnTuesday = NULL,
    runOnWednesday = NULL,
    runOnThursday = NULL,
    runOnFriday = NULL,
    runOnSaturday = NULL,
    runOnSunday = NULL,

    #' @description Create a new schedule recurrence instance.
    #'
    #' @param recurrenceType The type of the recurrence (string).
    #' @param intervalsAmount Run the schedule every n days.
    #' @param skipWeekend Flag specifying whether to skip execution on weekends.
    #' @param repeatOnSpecificDayMonth Flag specifying whether to reapeat the
    #'   schedule on a specific day of a month.
    #' @param specificDayNumber The number of the day the schedule shall be
    #'   repeated on if \code{repeatOnSpecificDayMonth} is \code{TRUE}.
    #' @param orderNumberDay ??
    #' @param typeDayMonthDay ??
    #' @param runOnMonday Flag specifying if the schedule is run on Mondays.
    #' @param runOnTuesday Flag specifying if the schedule is run on Tuesdays.
    #' @param runOnWednesday Flag specifying if the schedule is run on
    #'   Wednesdays.
    #' @param runOnThursday Flag specifying if the schedule is run on Thursdays.
    #' @param runOnFriday Flag specifying if the schedule is run on Fridays.
    #' @param runOnSaturday Flag specifying if the schedule is run on Saturdays.
    #' @param runOnSunday Flag specifying if the schedule is run on Sundays.
    initialize = function(recurrenceType = NULL,
                          intervalsAmount = NULL,
                          skipWeekend = NULL,
                          repeatOnSpecificDayMonth = NULL,
                          specificDayNumber = NULL,
                          orderNumberDay = NULL,
                          typeDayMonthDay = NULL,
                          runOnMonday = NULL,
                          runOnTuesday = NULL,
                          runOnWednesday = NULL,
                          runOnThursday = NULL,
                          runOnFriday = NULL,
                          runOnSaturday = NULL,
                          runOnSunday = NULL){
      self$recurrenceType <- recurrenceType
      self$intervalsAmount <- intervalsAmount
      self$skipWeekend <- skipWeekend
      self$repeatOnSpecificDayMonth <- repeatOnSpecificDayMonth
      self$specificDayNumber <- specificDayNumber
      self$orderNumberDay <- orderNumberDay
      self$typeDayMonthDay <- typeDayMonthDay
      self$runOnMonday <- runOnMonday
      self$runOnTuesday <- runOnTuesday
      self$runOnWednesday <- runOnWednesday
      self$runOnThursday <- runOnThursday
      self$runOnFriday <- runOnFriday
      self$runOnSaturday <- runOnSaturday
      self$runOnSunday <- runOnSunday
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        RecurrenceType = if(is.null(self$recurrenceType)) {
          ""
        } else {
          self$recurrenceType
        },
        IntervalsAmount = if(is.null(self$intervalsAmount)) {
          ""
        } else {
          self$intervalsAmount
        },
        SkipWeekend = if(is.null(self$skipWeekend)) "" else self$skipWeekend,
        RepeatOnSpecificDayMonth = if(is.null(self$repeatOnSpecificDayMonth)) {
          ""
        } else {
          self$repeatOnSpecificDayMonth
        },
        SpecificDayNumber = if(is.null(self$specificDayNumber)) {
          ""
        } else {
          self$specificDayNumber
        },
        OrderNumberDay = if(is.null(self$orderNumberDay)) {
          ""
        } else {
          self$orderNumberDay
        },
        TypeDayMonthDay = if(is.null(self$typeDayMonthDay)) {
          ""
        } else {
          self$typeDayMonthDay
        },
        RunOnMonday = if(is.null(self$runOnMonday)) {
          ""
        } else {
          self$runOnMonday
        },

        RunOnTuesday = if(is.null(self$runOnTuesday)) {
          ""
        } else {
          self$runOnTuesday
        },
        RunOnWednesday = if(is.null(self$runOnWednesday)) {
          ""
        } else {
          self$runOnWednesday
        },
        RunOnThursday = if(is.null(self$runOnThursday)) {
          ""
        } else {
          self$runOnThursday
        },
        RunOnFriday = if(is.null(self$runOnFriday)) {
          ""
        } else {
          self$runOnFriday
        },
        RunOnSaturday = if(is.null(self$runOnSaturday)) {
          ""
        } else {
          self$runOnSaturday
        },
        RunOnSunday = if(is.null(self$runOnSunday)) {
          ""
        } else {
          self$runOnSunday
        }
      )
      return(dl)
    }
  )
)
