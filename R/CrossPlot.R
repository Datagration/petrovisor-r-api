library("R6")

#' @title CrossPlot
#'
#' @description Class representing a PetroVisor cross plot object.
#'
#' @export CrossPlot
#'
#' @field name The name of the cross plot.
#' @field dataSets A list of data sets in the cross plot. The items in the
#'   list must be instances of the class \code{CrossPlotDataSet}.
#' @field dataStart The start date of the data.
#' @field dataEnd The end date of the data.
#' @field dataStep The time increment of the data. One of:
#'   \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
#'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
#'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}.
#' @field hierarchyName The name of a hierarchy.
#' @field mode The mode of the cross plot.
#' @field xAxisSignalName The name of the signal on the x-axis.
#' @field xAxisUnitName The name of the unit on the x-axis.
#' @field xAxisEntityName The name of the entity on the x-axis.
#' @field yAxisSignalName The name of the signal on the y-axis.
#' @field yAxisUnitName The name of the unit on the y-axis.
#' @field yAxisEntityName The name of the entity on the y-axis.
#' @field skippedEntityNames A list of the names of skipped entities.
#' @field timeMin The minimum time.
#' @field timeMax The maximum time.
#' @field isLocked This flag specifies whether the cross plot is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the cross plot belongs to.
#' @field isFavorite This flag specifies whether the cross plot is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the cross plot.
#' @examples
#' \dontrun{
#' CrossPlot$new()
#'}
CrossPlot <- R6Class("CrossPlot",
  public = list(
    name = NULL,
    dataSets = NULL,
    dataStart = NULL,
    dataEnd = NULL,
    dataStep = NULL,
    hierarchyName = NULL,
    mode = NULL,
    xAxisSignalName = NULL,
    xAxisUnitName = NULL,
    xAxisEntityName = NULL,
    yAxisSignalName = NULL,
    yAxisUnitName = NULL,
    yAxisEntityName = NULL,
    skippedEntityNames = NULL,
    timeMin = NULL,
    timeMax = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new CrossPlot instance.
    #'
    #' @param name The name of the cross plot.
    #' @param dataSets A list of data sets in the cross plot. The items in the
    #'   list must be instances of the class \code{CrossPlotDataSet}.
    #' @param dataStart The start date of the data.
    #' @param dataEnd The end date of the data.
    #' @param dataStep The time increment of the data. One of:
    #'   \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
    #'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
    #'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}.
    #' @param hierarchyName The name of a hierarchy.
    #' @param mode The mode of the cross plot.
    #' @param xAxisSignalName The name of the signal on the x-axis.
    #' @param xAxisUnitName The name of the unit on the x-axis.
    #' @param xAxisEntityName The name of the entity on the x-axis.
    #' @param yAxisSignalName The name of the signal on the y-axis.
    #' @param yAxisUnitName The name of the unit on the y-axis.
    #' @param yAxisEntityName The name of the entity on the y-axis.
    #' @param skippedEntityNames A list of the names of skipped entities.
    #' @param timeMin The minimum time.
    #' @param timeMax The maximum time.
    #' @param isLocked This flag specifies whether the cross plot is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the cross plot belongs to.
    #' @param isFavorite This flag specifies whether the cross plot is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module in
    #'   PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the cross plot.
    initialize = function(name = NULL,
                          dataSets = NULL,
                          dataStart = NULL,
                          dataEnd = NULL,
                          dataStep = c("EveryMinute",
                                       "EveryFiveMinutes",
                                       "EveryFifteenMinutes",
                                       "Hourly",
                                       "Daily",
                                       "Monthly",
                                       "Quarterly",
                                       "Yearly"),
                          hierarchyName = NULL,
                          mode = NULL,
                          xAxisSignalName = NULL,
                          xAxisUnitName = NULL,
                          xAxisEntityName = NULL,
                          yAxisSignalName = NULL,
                          yAxisUnitName = NULL,
                          yAxisEntityName = NULL,
                          skippedEntityNames = NULL,
                          timeMin = NULL,
                          timeMax = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$dataSets <- dataSets
      self$dataStart <- dataStart
      self$dataEnd <- dataEnd
      self$dataStep <- match.arg(dataStep)
      self$hierarchyName <- hierarchyName
      self$mode <- mode
      self$xAxisSignalName <- xAxisSignalName
      self$xAxisUnitName <- xAxisUnitName
      self$xAxisEntityName <- xAxisEntityName
      self$yAxisSignalName <- yAxisSignalName
      self$yAxisUnitName <- yAxisUnitName
      self$yAxisEntityName <- yAxisEntityName
      self$skippedEntityNames <- skippedEntityNames
      self$timeMin <- timeMin
      self$timeMax <- timeMax
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of cross plot data sets
      dataSetsList <- list()
      if(!is.null(self$dataSets)){
        for (i in 1:length(self$dataSets)){
          dataSetsList[[i]] <- self$dataSets[[i]]$toList()
        }
      } else {
        dataSetsList[[1]] <- ""
      }

      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        DataSets = if(is.null(self$dataSets)) "" else dataSetsList,
        DataStart = if(is.null(self$dataStart)) "" else self$dataStart,
        DataEnd = if(is.null(self$dataEnd)) "" else self$dataEnd,
        DataStep = if(is.null(self$dataStep)) "" else self$dataStep,
        HierarchyName = if(is.null(self$hierarchyName)) {
          ""
        } else {
          self$hierarchyName
        },
        Mode = if(is.null(self$mode)) "" else self$mode,
        XAxis = list(
          Signal = if(is.null(self$xAxisSignalName)) {
            ""
          } else {
            self$xAxisSignalName
          },
          Unit = if(is.null(self$xAxisUnitName)) {
            ""
          } else {
            self$xAxisUnitName
          },
          Entity = if(is.null(self$xAxisEntityName)) {
            ""
          } else {
            self$xAxisEntityName
          }),
        YAxis = list(
          Signal = if(is.null(self$yAxisSignalName)) {
            ""
          } else {
            self$yAxisSignalName
          },
          Unit = if(is.null(self$yAxisUnitName)) {
            ""
          } else {
            self$yAxisUnitName
          },
          Entity = if(is.null(self$yAxisEntityName)) {
            ""
          } else {
            self$yAxisEntityName
          }),
        SkippedEntities = if(is.null(self$skippedEntityNames)) {
          ""
        } else {
          self$skippedEntityNames
        },
        TimeMin = if(is.null(self$timeMin)) "" else self$timeMin,
        TimeMax = if(is.null(self$timeMax)) "" else self$timeMax,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)

#' @title CrossPlotDataSet
#'
#' @description Class representing a cross plot data set object.
#'
#' @export CrossPlotDataSet
#'
#' @field signalName The name of the signal.
#' @field unitName The name of the unit.
#' @field entityName The name of the entity.
#' @field symbol A string giving the symbol to use for the data set.
#' @field color The color to use for the data set (string).
#' @field min The min value of the data set.
#' @field max The max value of the data set.
#' @field trendLine A string specifying whether to show a trend line.
#'   Either \code{"On"} or \code{"Off"}.
#'
#' @examples
#' \dontrun{
#' CrossPlotDataSet$new()
#'}
CrossPlotDataSet <- R6Class("CrossPlotDataSet",
  public = list(
    signalName = NULL,
    unitName = NULL,
    entityName = NULL,
    symbol = NULL,
    color = NULL,
    min = NULL,
    max = NULL,
    trendLine = NULL,

    #' @description Create a new CrossPlotDataSet instance.
    #'
    #' @param signalName The name of the signal.
    #' @param unitName The name of the unit.
    #' @param entityName The name of the entity.
    #' @param symbol A string giving the symbol to use for the data set.
    #' @param color The color to use for the data set (string).
    #' @param min The min value of the data set.
    #' @param max The max value of the data set.
    #' @param trendLine A string specifying whether to show a trend line.
    #'   Either \code{"On"} or \code{"Off"}.
    initialize = function(signalName = NULL,
                          unitName = NULL,
                          entityName = NULL,
                          symbol = NULL,
                          color = NULL,
                          min = NULL,
                          max = NULL,
                          trendLine = NULL){
      self$signalName <- signalName
      self$unitName <- unitName
      self$entityName <- entityName
      self$symbol <- symbol
      self$color <- color
      self$min <- min
      self$max <- max
      self$trendLine <- trendLine
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        SignalName = if(is.null(self$signalName)) "" else self$signalName,
        UnitName = if(is.null(self$unitName)) "" else self$unitName,
        EntityName = if(is.null(self$entityName)) "" else self$entityName,
        Symbol = if(is.null(self$symbol)) "" else self$symbol,
        Color = if(is.null(self$color)) "" else self$color,
        Min = if(is.null(self$min)) "" else self$min,
        Max = if(is.null(self$max)) "" else self$max,
        TrendLine = if(is.null(self$trendLine)) "" else self$trendLine
      )
      return(dl)
    }
  )
)
