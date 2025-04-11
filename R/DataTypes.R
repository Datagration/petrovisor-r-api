library("R6")

#' @title StaticData
#'
#' @description Class representing static data.
#'
#' @export StaticData
#'
#' @field signal_name The name of the signal.
#' @field entity_name The name of the entity.
#' @field unit_name The data's unit name.
#' @field data The data. In this case case a single numberic or string value.
#' @field scenario The scenario the data is part of.
#'
#' @examples
#' \dontrun{
#' StaticData$new(signal_name = "surface x-coordinate",
#'                entity_name = "Well01",
#'                unit_name = "m",
#'                data = 10)
#'}
StaticData <- R6Class("StaticData",
  public = list(
    signal_name = NULL,
    entity_name = NULL,
    unit_name = NULL,
    data = NULL,
    scenario = NULL,

    #' @description Create a new StaticData instance.
    #'
    #' @param signal_name The name of the signal.
    #' @param entity_name The name of the entity.
    #' @param unit_name The data's unit name.
    #' @param data The data. In this case case a single number or string value.
    #' @param scenario The scenario the data is part of.
    initialize = function(signal_name = NULL,
                          entity_name = NULL,
                          unit_name = NULL,
                          data = NULL,
                          scenario = NULL) {
      self$signal_name <- signal_name
      self$entity_name <- entity_name
      self$unit_name <- unit_name
      self$data <- data
      self$scenario <- scenario
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    to_list = function() {
      dl <- list(
        Signal = if (is.null(self$signal_name)) "" else self$signal_name,
        Entity = if (is.null(self$entity_name)) "" else self$entity_name,
        Unit = if (is.null(self$unit_name)) "" else self$unit_name,
        Data = self$data,
        Scenario = if (is.null(self$scenario)) "" else self$scenario
      )
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
#' @field signal_name The name of the signal.
#' @field entity_name The name of the entity.
#' @field unit_name The data's unit name.
#' @field data The data. A list of date-value-pairs (named list).
#' @field scenario The scenario the data is part of.
#'
#' @examples
#' \dontrun{
#' TimeData$new(singal_name = "produced oil per time increment",
#'              entity_name = "Well01",
#'              unit_name = "m3",
#'              data = list(list(Date = "2020-01-01T00:00:00.000Z",
#'                               Value = 20)))
#'}
TimeData <- R6Class("TimeData",
  public = list(
    signal_name = NULL,
    entity_name = NULL,
    unit_name = NULL,
    data = NULL,
    scenario = NULL,

    #' @description Create a new TimeData instance.
    #'
    #' @param signal_name The name of the signal.
    #' @param entity_name The name of the entity.
    #' @param unit_name The data's unit name.
    #' @param data The data. A list of date-value-pairs (named list)
    #' @param scenario The scenario the data is part of.
    initialize = function(signal_name = NULL,
                          entity_name = NULL,
                          unit_name = NULL,
                          data = list(),
                          scenario = NULL) {
      self$signal_name <- signal_name
      self$entity_name <- entity_name
      self$unit_name <- unit_name
      self$data <- data
      self$scenario <- scenario
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    to_list = function() {
      dl <- list(
        Signal = if (is.null(self$signal_name)) "" else self$signal_name,
        Entity = if (is.null(self$entity_name)) "" else self$entity_name,
        Unit = if (is.null(self$unit_name)) "" else self$unit_name,
        Data = self$data,
        Scenario = if (is.null(self$scenario)) "" else self$scenario
      )
      return(dl)
    }
  )
)

#' @title DepthData
#'
#' @description Class representing depth-dependent data.
#'
#' @export DepthData
#'
#' @field signal_name The name of the signal.
#' @field entity_name The name of the entity.
#' @field unit_name The data's unit name.
#' @field data The data. A list of depth-value-pairs (named list).
#' @field scenario The scenario the data is part of.
#'
#' @examples
#' \dontrun{
#' TimeData$new(singal_name = "produced oil per depth increment",
#'              entity_name = "Well01",
#'              unit_name = "m3",
#'              data = list(list(Depth = 100,
#'                               Value = 20)))
#'}
DepthData <- R6Class("DepthData",
  public = list(
    signal_name = NULL,
    entity_name = NULL,
    unit_name = NULL,
    data = NULL,
    scenario = NULL,

    #' @description Create a new DepthData instance.
    #'
    #' @param signal_name The name of the signal.
    #' @param entity_name The name of the entity.
    #' @param unit_name The data's unit name.
    #' @param data The data. A list of date-value-pairs (named list)
    #' @param scenario The scenario the data is part of.
    initialize = function(signal_name = NULL,
                          entity_name = NULL,
                          unit_name = NULL,
                          data = list(),
                          scenario = NULL) {
      self$signal_name <- signal_name
      self$entity_name <- entity_name
      self$unit_name <- unit_name
      self$data <- data
      self$scenario <- scenario
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    to_list = function() {
      dl <- list(
        Signal = if (is.null(self$signal_name)) "" else self$signal_name,
        Entity = if (is.null(self$entity_name)) "" else self$entity_name,
        Unit = if (is.null(self$unit_name)) "" else self$unit_name,
        Data = self$data,
        Scenario = if (is.null(self$scenario)) "" else self$scenario
      )
      return(dl)
    }
  )
)

#' @title PVTData
#'
#' @description Class representing pvt data.
#'
#' @export PVTData
#'
#' @field signal_name The name of the signal.
#' @field entity_name The name of the entity.
#' @field unit_name The data's unit name.
#' @field data The data. A list of pressure-temperature-value
#'   triplets (named list).
#' @field scenario The scenario the data is part of.
#'
#' @examples
#' \dontrun{
#' PVTData$new(singal_name = "produced oil per time increment pvt",
#'             entity_name = "Well01",
#'             unit_name = "m3",
#'             data = list(list(Pressure = 300,
#'                              Temperature = 150,
#'                              Value = 20)))
#'}
PVTData <- R6Class("PVTData",
  public = list(
    signal_name = NULL,
    entity_name = NULL,
    unit_name = NULL,
    data = NULL,
    scenario = NULL,

    #' @description Create a new PVTData instance.
    #'
    #' @param signal_name The name of the signal.
    #' @param entity_name The name of the entity.
    #' @param unit_name The data's unit name.
    #' @param data The data. The data. A list of pressure-temperature-value
    #'   triplets (named list).
    #' @param scenario The scenario the data is part of.
    initialize = function(signal_name = NULL,
                          entity_name = NULL,
                          unit_name = NULL,
                          data = list(),
                          scenario = NULL) {
      self$signal_name <- signal_name
      self$entity_name <- entity_name
      self$unit_name <- unit_name
      self$data <- data
      self$scenario <- scenario
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    to_list = function() {
      dl <- list(
        Signal = if (is.null(self$signal_name)) "" else self$signal_name,
        Entity = if (is.null(self$entity_name)) "" else self$entity_name,
        Unit = if (is.null(self$unit_name)) "" else self$unit_name,
        Data = self$data,
        Scenario = if (is.null(self$scenario)) "" else self$scenario
      )
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
                          value = NULL) {
      self$date <- date
      self$value <- value
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the DataServices to convert the objects to lists and then
    #' call the web API.
    to_list = function() {
      dl <- list(
        Date = if (is.null(self$date)) "" else self$date,
        Value = self$value
      )
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
                          y = NULL) {
      self$x <- x
      self$y <- y
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the Services to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        X = if (is.null(self$x)) "" else self$x,
        Y = if (is.null(self$y)) "" else self$y
      )
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
                          y = NULL) {
      self$name <- name
      self$tagName <- tagName
      self$x <- x
      self$y <- y
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the Services to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        Tag = if (is.null(self$tagName)) "" else self$tagName,
        X = if (is.null(self$x)) "" else self$x,
        Y = if (is.null(self$y)) "" else self$y
      )
      return(dl)
    }
  )
)
