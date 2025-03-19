library("R6")

#' @title CleansingCalculation
#'
#' @description Class representing a PetroVisor cleansing calculation object.
#'
#' @export CleansingCalculation
#'
#' @field name The name of the cleansing calculation.
#' @field filters A list of filters of the cleansing calculation. Each item has
#'   to be an instance of the class \code{CleansingFilter}.
#' @field isLocked This flag specifies whether the cleansing calculation is
#'   locked. Defaults to \code{FALSE}.
#' @field user The user the cleansing calculation belongs to.
#' @field isFavorite This flag specifies whether the cleansing calculation is
#'   marked as favorite item, and thus shown in the favorites tab on the home
#'   module in PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the cleansing
#'   calculation.
#' @field formula The cleansing calculation's formula as string.
#' @examples
#' \dontrun{
#' CleansingCalculation$new()
#'}
CleansingCalculation <- R6Class("CleansingCalculation",
  public = list(
    name = NULL,
    filters = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,
    formula = NULL,

    #' @description Create a new CleansingCalculation instance.
    #'
    #' @param name The name of the cleansing calculation.
    #' @param filters A list of filters of the cleansing calculation. Each item has
    #'   to be an instance of the class \code{CleansingFilter}.
    #' @param isLocked This flag specifies whether the cleansing calculation is
    #'   locked. Defaults to \code{FALSE}.
    #' @param user The user the cleansing calculation belongs to.
    #' @param isFavorite This flag specifies whether the cleansing calculation is
    #'   marked as favorite item, and thus shown in the favorites tab on the home
    #'   module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the cleansing
    #'   calculation.
    #' @param formula The cleansing calculation's formula as string.
    initialize = function(name = NULL,
                          filters = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL,
                          formula = NULL){
      self$name <- name
      self$filters <- filters
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
      self$formula <- formula
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of filters
      filtersList <- list()
      if(!is.null(self$filters)){
        for (i in 1:length(self$filters)){
          filtersList[[i]] <- self$filters[[i]]$toList()
        }
      } else {
        filtersList[[1]] <- ""
      }


      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Filters = if(is.null(self$filters)) "" else filtersList,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels,
        Formula = if(is.null(self$formula)) "" else self$formula
      )
      return(dl)
    }
  )
)

#' @title CleansingFilter
#'
#' @description Class representing a cleansing filter object.
#'
#' @export CleansingFilter
#'
#' @field name The name of the cleansing filter.
#' @field formula The cleansing filter's formula.
#' @field unitName The cleansing filter's unit name.
#' @examples
#' \dontrun{
#' CleansingFilter$new()
#'}
CleansingFilter <- R6Class("CleansingFilter",
  public = list(
    name = NULL,
    formula = NULL,
    unitName = NULL,

    #' @description Create a new CleansingFilter instance.
    #'
    #' @param name The name of the cleansing filter.
    #' @param formula The cleansing filter's formula.
    #' @param unitName The cleansing filter's unit name.
    initialize = function(name = NULL,
                          formula = NULL,
                          unitName = NULL){
      self$name <- name
      self$formula <- formula
      self$unitName <- unitName
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Formula = if(is.null(self$formula)) "" else self$formula,
        UnitName = if(is.null(self$unitName)) "" else self$unitName
      )
      return(dl)
    }
  )
)

#' @title EventCalculation
#'
#' @description Class representing a PetroVisor event calculation object.
#'
#' @export EventCalculation
#'
#' @field name The name of the event calculation.
#' @field events A list of events of the event calculation. Each item has
#'   to be an instance of the class \code{Event}.
#' @field inputTableNames A list containing the names of all input tables for
#'   the event calculation.
#' @field isLocked This flag specifies whether the event calculation is
#'   locked. Defaults to \code{FALSE}.
#' @field user The user the event calculation belongs to.
#' @field isFavorite This flag specifies whether the event calculation is
#'   marked as favorite item, and thus shown in the favorites tab on the home
#'   module in PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the event
#'   calculation.
#' @field formula The event calculation's formula as string.
#' @examples
#' \dontrun{
#' EventCalculation$new()
#'}
EventCalculation <- R6Class("EventCalculation",
  public = list(
    name = NULL,
    events = NULL,
    inputTableNames = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,
    formula = NULL,

    #' @description Create a new EventCalculation instance.
    #'
    #' @param name The name of the event calculation.
    #' @param events A list of events of the event calculation. Each item has
    #'   to be an instance of the class \code{Event}.
    #' @param inputTableNames A list containing the names of all input tables for
    #'   the event calculation.
    #' @param isLocked This flag specifies whether the event calculation is
    #'   locked. Defaults to \code{FALSE}.
    #' @param user The user the event calculation belongs to.
    #' @param isFavorite This flag specifies whether the event calculation is
    #'   marked as favorite item, and thus shown in the favorites tab on the home
    #'   module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the event
    #'   calculation.
    #' @param formula The event calculation's formula as string.
    initialize = function(name = NULL,
                          events = NULL,
                          inputTableNames = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL,
                          formula = NULL){
      self$name <- name
      self$events <- events
      self$inputTableNames <- inputTableNames
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
      self$formula <- formula
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of events
      eventsList <- list()
      if(!is.null(self$events)){
        for (i in 1:length(self$events)){
          eventsList[[i]] <- self$events[[i]]$toList()
        }
      } else {
        eventsList[[1]] <- ""
      }


      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Events = if(is.null(self$events)) "" else eventsList,
        InputTableNames = if(is.null(self$inputTableNames)) {
          ""
        } else {
          self$inputTableNames
        },
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels,
        Formula = if(is.null(self$formula)) "" else self$formula
      )
      return(dl)
    }
  )
)

#' @title Event
#'
#' @description Class representing a event object.
#'
#' @export Event
#'
#' @field name The name of the event.
#' @field formula The event's formula.
#' @examples
#' \dontrun{
#' Event$new()
#'}
Event <- R6Class("Event",
  public = list(
    name = NULL,
    formula = NULL,

    #' @description Create a new Event instance.
    #'
    #' @param name The name of the cleansing filter.
    #' @param formula The cleansing filter's formula.
    initialize = function(name = NULL,
                          formula = NULL){
      self$name <- name
      self$formula <- formula
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Formula = if(is.null(self$formula)) "" else self$formula
      )
      return(dl)
    }
  )
)

#' @title TableCalculation
#'
#' @description Class representing a PetroVisor table calculation object.
#'
#' @export TableCalculation
#'
#' @field name The name of the table calculation.
#' @field columns A list of columns of the table calculation. Each item has
#'   to be an instance of the class \code{Column}.
#' @field inputTableNames A list containing the names of all input tables for
#'   the table calculation.
#' @field isLocked This flag specifies whether the table calculation is
#'   locked. Defaults to \code{FALSE}.
#' @field user The user the table calculation belongs to.
#' @field isFavorite This flag specifies whether the table calculation is
#'   marked as favorite item, and thus shown in the favorites tab on the home
#'   module in PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the table
#'   calculation.
#' @field formula The table calculation's formula as string.
#' @examples
#' \dontrun{
#' TableCalculation$new()
#'}
TableCalculation <- R6Class("TableCalculation",
  public = list(
    name = NULL,
    columns = NULL,
    inputTableNames = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,
    formula = NULL,

    #' @description Create a new EventCalculation instance.
    #'
    #' @param name The name of the table calculation.
    #' @param columns A list of columns of the table calculation. Each item has
    #'   to be an instance of the class \code{Column}.
    #' @param inputTableNames A list containing the names of all input tables for
    #'   the table calculation.
    #' @param isLocked This flag specifies whether the table calculation is
    #'   locked. Defaults to \code{FALSE}.
    #' @param user The user the table calculation belongs to.
    #' @param isFavorite This flag specifies whether the table calculation is
    #'   marked as favorite item, and thus shown in the favorites tab on the home
    #'   module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the table
    #'   calculation.
    #' @param formula The table calculation's formula as string.
    initialize = function(name = NULL,
                          columns = NULL,
                          inputTableNames = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL,
                          formula = NULL){
      self$name <- name
      self$columns <- columns
      self$inputTableNames <- inputTableNames
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
      self$formula <- formula
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of columns
      columnsList <- list()
      if(!is.null(self$columns)){
        for (i in 1:length(self$columns)){
          columnsList[[i]] <- self$columns[[i]]$toList()
        }
      } else {
        columnsList[[1]] <- ""
      }


      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Columns = if(is.null(self$columns)) "" else columnsList,
        InputTableNames = if(is.null(self$inputTableNames)) {
          ""
        } else {
          self$inputTableNames
        },
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels,
        Formula = if(is.null(self$formula)) "" else self$formula
      )
      return(dl)
    }
  )
)

#' @title Column
#'
#' @description Class representing a column object.
#'
#' @export Column
#'
#' @field name The name of the column.
#' @field unit The unit of the column. Has to be an object of the class
#'   \code{Unit}.
#' @field formula The columns's formula.
#' @field isStatic This flag specifies whether the column is static or
#'   time-dependent.
#' @field savingSignalName The name of the signal in the columns saving clause.
#' @field saveToParentEntity This flag specifies whether data shall be saved to
#'   the parent of the entity.
#' @examples
#' \dontrun{
#' Column$new()
#'}
Column <- R6Class("Column",
  public = list(
    name = NULL,
    unit = NULL,
    formula = NULL,
    isStatic = NULL,
    savingSignalName = NULL,
    saveToParentEntity = NULL,

    #' @description Create a new Column instance.
    #'
    #' @param name The name of the column.
    #' @param unit The unit of the column. Has to be an object of the class
    #'   \code{Unit}.
    #' @param formula The columns's formula.
    #' @param isStatic This flag specifies whether the column is static or
    #'   time-dependent.
    #' @param savingSignalName The name of the signal in the columns saving
    #'   clause.
    #' @param saveToParentEntity This flag specifies whether data shall be saved
    #'   to the parent of the entity.
    initialize = function(name = NULL,
                          unit = NULL,
                          formula = NULL,
                          isStatic = NULL,
                          savingSignalName = NULL,
                          saveToParentEntity = NULL){
      self$name <- name
      self$unit <- unit
      self$formula <- formula
      self$isStatic <- isStatic
      self$savingSignalName <- savingSignalName
      self$saveToParentEntity <- saveToParentEntity
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Unit = if(is.null(self$unit)) "" else self$unit$toList(),
        Formula = if(is.null(self$formula)) "" else self$formula,
        IsStatic = if(is.null(self$isStatic)) "" else self$isStatic,
        SavingSignalName = if(is.null(self$savingSignalName)) {
          ""
        } else {
          self$savingSignalName
        },
        SaveToParentEntity = if(is.null(self$saveToParentEntity)) {
          ""
        } else{
          self$saveToParentEntity
        }
      )
      return(dl)
    }
  )
)
