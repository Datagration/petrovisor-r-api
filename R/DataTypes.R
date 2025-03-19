library("R6")

#' @title StaticData
#'
#' @description Class representing static data.
#'
#' @export StaticData
#'
#' @field signalName The name of the signal.
#' @field entityName The name of the entity.
#' @field unitName The data's unit name.
#' @field data The data. In this case case a single numberic or string value.
#'
#' @examples
#' \dontrun{
#' StaticData$new(signalName = "surface x-coordinate",
#'                entityName = "Well01",
#'                unitName = "m",
#'                data = 10)
#'}
StaticData <- R6Class("StaticData",
  public = list(
    signalName = NULL,
    entityName = NULL,
    unitName = NULL,
    data = NULL,

    #' @description Create a new StaticData instance.
    #'
    #' @param signalName The name of the signal.
    #' @param entityName The name of the entity.
    #' @param unitName The data's unit name.
    #' @param data The data. In this case case a single numberic or string value.
    initialize = function(signalName = NULL,
                          entityName = NULL,
                          unitName = NULL,
                          data = NULL){
      self$signalName <- signalName
      self$entityName <- entityName
      self$unitName <- unitName
      self$data <- data
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Signal = if(is.null(self$signalName)) "" else self$signalName,
        Entity = if(is.null(self$entityName)) "" else self$entityName,
        Unit = if(is.null(self$unitName)) "" else self$unitName,
        Data = if(is.null(self$data)) "" else self$data)
      return(dl)
    }
  )
)

#' @title TimeData
#'
#' @description Class representing time-dependent data.
#'
#' @export TimeData
#'
#' @field signalName The name of the signal.
#' @field entityName The name of the entity.
#' @field unitName The data's unit name.
#' @field data The data. A list of date-value-pairs (objects of type DataPoint)
#'
#' @examples
#' \dontrun{
#' TimeData$new(singalName = "produced oil per time increment",
#'              entityName = "Well01",
#'              unitName = "m3",
#'              data = list(DataPoint$new(date = "2020-01-01T00:00:00.000Z",
#'                                        value = 20)))
#'}
TimeData <- R6Class("TimeData",
  public = list(
    signalName = NULL,
    entityName = NULL,
    unitName = NULL,
    data = NULL,

    #' @description Create a new StaticData instance.
    #'
    #' @param signalName The name of the signal.
    #' @param entityName The name of the entity.
    #' @param unitName The data's unit name.
    #' @param data The data. A list of date-value-pairs (objects of type
    #' DataPoint)
    initialize = function(signalName = NULL,
                          entityName = NULL,
                          unitName = NULL,
                          data = NULL){
      self$signalName <- signalName
      self$entityName <- entityName
      self$unitName <- unitName
      self$data <- data
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dataList <- list()
      if(!is.null(self$data)){
        for (i in 1:length(self$data)){
          dataList[[i]] <- self$data[[i]]$toList()
        }
      } else {
        dataList[[1]] <- ""
      }

      dl <- list(
        Signal = if(is.null(self$signalName)) "" else self$signalName,
        Entity = if(is.null(self$entityName)) "" else self$entityName,
        Unit = if(is.null(self$unitName)) "" else self$unitName,
        Data = if(is.null(self$data)) "" else dataList)
      return(dl)
    }
  )
)

#' @title DataPoint
#'
#' @description Class representing a time-dependent data point.
#'
#' @export DataPoint
#'
#' @field date The timestamp of the data point.
#' @field value The value at the specified timestamp.
#'
#' @examples
#' \dontrun{
#' DataPoint$new(date = "2020-01-01T00:00:00.000Z", value = 20)
#'}
DataPoint <- R6Class("DataPoint",
  public = list(
    date = NULL,
    value = NULL,

    #' @description Create a new DataPoint instance.
    #'
    #' @param date The timestamp of the data point.
    #' @param value The value at the specified timestamp.
    initialize = function(date = NULL,
                          value = NULL){
      self$date <- date
      self$value <- value
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Date = if(is.null(self$date)) "" else self$date,
        Value = if(is.null(self$value)) "" else self$value)
      return(dl)
    }
  )
)

#' @title Point
#'
#' @description Class representing a point with x- and y-value.
#'
#' @export Point
#'
#' @field x The x-value of the point.
#' @field y The y-value of the point.
#'
#' @examples
#' \dontrun{
#' Point$new(x = 10, y = 20)
#'}
Point <- R6Class("Point",
  public = list(
    x = NULL,
    y = NULL,

    #' @description Create a new Point instance.
    #'
    #' @param x The x-value of the point.
    #' @param y The y-value of the point.
    initialize = function(x = NULL,
                          y = NULL){
      self$x <- x
      self$y <- y
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the Services to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        X = if(is.null(self$x)) "" else self$x,
        Y = if(is.null(self$y)) "" else self$y)
      return(dl)
    }
  )
)

#' @title NamedPoint
#'
#' @description Class representing a point with name, tag, x- and y-value.
#'
#' @export NamedPoint
#'
#' @field name The name of the point.
#' @field tagName The name of the point's tag.
#' @field x The x-value of the point.
#' @field y The y-value of the point.
#'
#' @examples
#' \dontrun{
#' NamedPoint$new(name = "MyPoint", tagName = "MyTag", x = 10, y = 20)
#'}
NamedPoint <- R6Class("NamedPoint",
  public = list(
    name = NULL,
    tagName = NULL,
    x = NULL,
    y = NULL,

    #' @description Create a new NamedPoint instance.
    #'
    #' @param name The name of the point.
    #' @param tagName The name of the point's tag
    #' @param x The x-value of the point.
    #' @param y The y-value of the point.
    initialize = function(name = NULL,
                          tagName = NULL,
                          x = NULL,
                          y = NULL){
      self$name <- name
      self$tagName <- tagName
      self$x <- x
      self$y <- y
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the Services to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Tag = if(is.null(self$tagName)) "" else self$tagName,
        X = if(is.null(self$x)) "" else self$x,
        Y = if(is.null(self$y)) "" else self$y)
      return(dl)
    }
  )
)
