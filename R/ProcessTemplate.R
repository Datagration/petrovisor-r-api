library("R6")

#' @title ProcessTemplate
#'
#' @description Class representing a PetroVisor process template object.
#'
#' @export ProcessTemplate
#'
#' @field name The name of the process template.
#' @field description The discription of the process template.
#' @field processTemplateGroup The group of the process template.
#' @field dueIntervals The interval unti a step is due.
#' @field priority The priority of the process.
#' @field severity The severity of the process.
#' @field steps A list containing the definition of the steps of the
#'   process template. The items of the list must be instances of the class
#'   \code{Step}.
#' @field customFields A list of custom fields of the process template.
#'   The items of the list must be instances of the class \code{CustomField}.
#'
#' @examples
#' \dontrun{
#' ProcessTemplate$new()
#'}
ProcessTemplate <- R6Class("ProcessTemplate",
  public = list(
    name = NULL,
    description = NULL,
    processTemplateGroup = NULL,
    dueIntervals = NULL,
    priority = NULL,
    severity = NULL,
    steps = NULL,
    customFields = NULL,

    #' @description Create a new ProcessTemplate instance.
    #'
    #' @param name The name of the process template.
    #' @param description The discription of the process template.
    #' @param processTemplateGroup The group of the process template.
    #' @param dueIntervals The interval unti a step is due.
    #' @param priority The priority of the process.
    #' @param severity The severity of the process.
    #' @param steps A list containing the definition of the steps of the
    #'   process template. The items of the list must be instances of the class
    #'   \code{Step}.
    #' @param customFields A list of custom fields of the process template.
    #'   The items of the list must be instances of the class \code{CustomField}.
    initialize = function(name = NULL,
                          description = NULL,
                          processTemplateGroup = NULL,
                          dueIntervals = NULL,
                          priority = NULL,
                          severity = NULL,
                          steps = NULL,
                          customFields = NULL){
      self$name <- name
      self$description <- description
      self$processTemplateGroup <- processTemplateGroup
      self$dueIntervals <- dueIntervals
      self$priority <- priority
      self$severity <- severity
      self$steps <- steps
      self$customFields <- customFields
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of steps
      stepsList <- list()
      if(!is.null(self$steps)){
        for (i in 1:length(self$steps)){
          stepsList[[i]] <- self$steps[[i]]$toList()
        }
      } else {
        stepsList[[1]] <- ""
      }

      # create list from list of custom fields
      customFieldsList <- list()
      if(!is.null(self$customFields)){
        for (i in 1:length(self$customFields)){
          customFieldsList[[i]] <- self$customFields[[i]]$toList()
        }
      } else {
        customFieldsList[[1]] <- ""
      }

      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Description = if(is.null(self$description)) "" else self$description,
        ProcessTemplateGroup = if(is.null(self$processTemplateGroup)) {
          ""
        } else {
          self$processTemplateGroup
        },
        DueIntervals = if(is.null(self$dueIntervals)) "" else self$dueIntervals,
        Priority = if(is.null(self$priority)) "" else self$priority,
        Severity = if(is.null(self$severity)) "" else self$severity,
        Steps = if(is.null(self$steps)) "" else stepsList,
        CustomFields = if(is.null(self$customFields)) "" else customFieldsList
      )
      return(dl)
    }
  )
)

#' @title Step
#'
#' @description Class representing a process template step.
#'
#' @export Step
#'
#' @field name The name of the step.
#' @field userGroupName The name of the user group assigned to the step.
#'
#' @examples
#' \dontrun{
#' Step$new(name = "DoThis", userGroupName = "Engineers")
#'}
Step <- R6Class("Step",
  public = list(
    name = NULL,
    userGroupName = NULL,

    #' @description Create a new Step instance.
    #'
    #' @param name The name of the step.
    #' @param userGroupName The name of the user group assigned to the step.
    initialize = function(name = NULL,
                          userGroupName = NULL){
      self$name <- name
      self$userGroupName <- userGroupName
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the Services to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        UserGroup = if(is.null(self$userGroupName)) "" else self$userGroupName)
      return(dl)
    }
  )
)

#' @title CustomField
#'
#' @description Class representing a process template custom field.
#'
#' @export CustomField
#'
#' @field name The name of the custom field.
#' @field evaluatedColumn The evaluated column. Must be an instance of the
#'   class \code{Column}.
#' @field evaluatedDate The date of the evaluation (string).
#' @field evaluatedValue The result of the evaluation (string).
#' @field fieldType The type of the custom field (integer).
#' @field scriptName The name of the script used by the custom field.
#' @field scriptColumnName The name of the column used by the custom field.
#' @field currentManualValue The current manually entered value.
#' @field initialValue The initial value of the custom field.
#' @field expressionTime The time of the expression.
#' @field activeStep The name of the step the custom field is active in.
#' @field settingName The name of the custom field's setting.
#'
#' @examples
#' \dontrun{
#' CustomField$new()
#'}
CustomField <- R6Class("CustomField",
  public = list(
    name = NULL,
    evaluatedColumn = NULL,
    evaluatedDate = NULL,
    evaluatedValue = NULL,
    fieldType = NULL,
    scriptName = NULL,
    scriptColumnName = NULL,
    currentManualValue = NULL,
    initialValue = NULL,
    expressionTime = NULL,
    activeStep = NULL,
    settingName = NULL,

    #' @description Create a new CustomField instance.
    #'
    #' @param name The name of the custom field.
    #' @param evaluatedColumn The evaluated column. Must be an instance of the
    #'   class \code{Column}.
    #' @param evaluatedDate The date of the evaluation (string).
    #' @param evaluatedValue The result of the evaluation (string).
    #' @param fieldType The type of the custom field (integer).
    #' @param scriptName The name of the script used by the custom field.
    #' @param scriptColumnName The name of the column used by the custom field.
    #' @param currentManualValue The current manually entered value.
    #' @param initialValue The initial value of the custom field.
    #' @param expressionTime The time of the expression.
    #' @param activeStep The name of the step the custom field is active in.
    #' @param settingName The name of the custom field's setting.
    initialize = function(name = NULL,
                          evaluatedColumn = NULL,
                          evaluatedDate = NULL,
                          evaluatedValue = NULL,
                          fieldType = NULL,
                          scriptName = NULL,
                          scriptColumnName = NULL,
                          currentManualValue = NULL,
                          initialValue = NULL,
                          expressionTime = NULL,
                          activeStep = NULL,
                          settingName = NULL){
      self$name <- name
      self$evaluatedColumn <- evaluatedColumn
      self$evaluatedDate <- evaluatedDate
      self$evaluatedValue <- evaluatedValue
      self$fieldType <- fieldType
      self$scriptName <- scriptName
      self$scriptColumnName <- scriptColumnName
      self$currentManualValue <- currentManualValue
      self$initialValue <- initialValue
      self$expressionTime <- expressionTime
      self$activeStep <- activeStep
      self$settingName <- settingName
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the Services to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Evaluated = list(
          Column = if(is.null(self$evaluatedColumn)) {
            ""
          } else {
            self$evaluatedColumn$toList()
          },
          Date = if(is.null(self$evaluatedDate)) "" else self$evaluatedDate,
          Value = if(is.null(self$evaluatedValue)) "" else self$evaluatedValue),
        FieldType = if(is.null(self$fieldType)) "" else self$fieldType,
        ScriptName = if(is.null(self$scriptName)) "" else self$scriptName,
        ScriptColumnName = if(is.null(self$scriptColumnName)) {
          ""
        }else {
          self$scriptColumnName
        },
        CurrentManualValue = if(is.null(self$currentManualValue)) {
          ""
        } else {
          self$currentManualValue
        },
        InitialValue = if(is.null(self$initialValue)) "" else self$initialValue,
        ExpressionTime = if(is.null(self$expressionTime)) {
          ""
        } else {
          self$expressionTime
        },
        ActiveStep = if(is.null(self$activeStep)) "" else self$activeStep,
        SettingName = if(is.null(self$settingName)) "" else self$settingName
        )
      return(dl)
    }
  )
)
