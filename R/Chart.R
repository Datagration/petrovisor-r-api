library("R6")

#' @title Chart
#'
#' @description Class representing a PetroVisor chart object.
#'
#' @export Chart
#'
#' @field name The name of the chart.
#' @field dataType The datatype of the chart (string).
#' @field legendLocation A string specifying the location of the legend.
#' @field isLegendVisible A flag specifying whether the legend is visible.
#' @field isLegendOrientationHorizontal A flag specifying whether the legend is
#'   oriented horizontal.
#' @field isLegendPlacementInside A flag specifying whether the legend is placed
#'   inside or outside the chart.
#' @field chartType The type of the chart (string).
#' @field isStacked A flag specifying whether the chart is stacked.
#' @field isStacked100 A flag specifying whether the chart is stacked.
#' @field isGridLinesOnXAxis A flag specifying whether gridlines are visible on
#'   the x-axis.
#' @field isGridLinesOnYAxis A flag specifying whether gridlines are visible on
#'   the y-axis.
#' @field dataAxesMappings A list giving the axes mappings.
#' @field seriesSettings A list of series settings. The individual settings
#'   must be instances of the class \code{SeriesSetting}.
#' @field axes A list of axes. The individual axes must be instances of the
#'   class \code{Axis}.
#' @field showToolTipTitle A flag specifying whether the tooltip title is shown.
#' @field showToolTipX A flag specifying whether to show the x-value in the
#'   tooltip.
#' @field showToolTipY A flag specifying whether to show the y-value in the
#'   tooltip.
#' @field isLocked This flag specifies whether the chart is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the chart belongs to.
#' @field isFavorite This flag specifies whether the chart is marked as favorite
#'   item, and thus shown in the favorites tab on the home module in PetroVisor.
#'   Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the chart.
#' @examples
#' \dontrun{
#' Chart$new()
#'}
Chart <- R6Class("Chart",
  public = list(
    name = NULL,
    dataType = NULL,
    legendLocation = NULL,
    isLegendVisible = NULL,
    isLegendOrientationHorizontal = NULL,
    isLegendPlacementInside = NULL,
    chartType = NULL,
    isStacked = NULL,
    isStacked100 = NULL,
    isGridLinesOnXAxis = NULL,
    isGridLinesOnYAxis = NULL,
    dataAxesMappings = NULL,
    seriesSettings = NULL,
    axes = NULL,
    showToolTipTitle = NULL,
    showToolTipX = NULL,
    showToolTipY = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Scope instance.
    #'
    #' @param name The name of the chart.
    #' @param dataType The datatype of the chart (string).
    #' @param legendLocation A string specifying the location of the legend.
    #' @param isLegendVisible A flag specifying whether the legend is visible.
    #' @param isLegendOrientationHorizontal A flag specifying whether the legend is
    #'   oriented horizontal.
    #' @param isLegendPlacementInside A flag specifying whether the legend is placed
    #'   inside or outside the chart.
    #' @param chartType The type of the chart (string).
    #' @param isStacked A flag specifying whether the chart is stacked.
    #' @param isStacked100 A flag specifying whether the chart is stacked.
    #' @param isGridLinesOnXAxis A flag specifying whether gridlines are visible on
    #'   the x-axis.
    #' @param isGridLinesOnYAxis A flag specifying whether gridlines are visible on
    #'   the y-axis.
    #' @param dataAxesMappings A list giving the axes mappings.
    #' @param seriesSettings A list of series settings. The individual settings
    #'   must be instances of the class \code{SeriesSetting}.
    #' @param axes A list of axes. The individual axes must be instances of the
    #'   class \code{Axis}.
    #' @param showToolTipTitle A flag specifying whether the tooltip title is shown.
    #' @param showToolTipX A flag specifying whether to show the x-value in the
    #'   tooltip.
    #' @param showToolTipY A flag specifying whether to show the y-value in the
    #'   tooltip.
    #' @param isLocked This flag specifies whether the chart is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the chart belongs to.
    #' @param isFavorite This flag specifies whether the chart is marked as favorite
    #'   item, and thus shown in the favorites tab on the home module in PetroVisor.
    #'   Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the chart.
    initialize = function(name = NULL,
                          dataType = NULL,
                          legendLocation = NULL,
                          isLegendVisible = NULL,
                          isLegendOrientationHorizontal = NULL,
                          isLegendPlacementInside = NULL,
                          chartType = NULL,
                          isStacked = NULL,
                          isStacked100 = NULL,
                          isGridLinesOnXAxis = NULL,
                          isGridLinesOnYAxis = NULL,
                          dataAxesMappings = NULL,
                          seriesSettings = NULL,
                          axes = NULL,
                          showToolTipTitle = NULL,
                          showToolTipX = NULL,
                          showToolTipY = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$dataType <- dataType
      self$legendLocation <- legendLocation
      self$isLegendVisible <- isLegendVisible
      self$isLegendOrientationHorizontal <- isLegendOrientationHorizontal
      self$isLegendPlacementInside <- isLegendPlacementInside
      self$chartType <- chartType
      self$isStacked <- isStacked
      self$isStacked100 <- isStacked100
      self$isGridLinesOnXAxis <- isGridLinesOnXAxis
      self$isGridLinesOnYAxis <- isGridLinesOnYAxis
      self$dataAxesMappings <- dataAxesMappings
      self$seriesSettings <- seriesSettings
      self$axes <- axes
      self$showToolTipTitle <- showToolTipTitle
      self$showToolTipX <- showToolTipX
      self$showToolTipY <- showToolTipY
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of series settings
      settingsList <- list()
      if(!is.null(self$seriesSettings)){
        for (i in 1:length(self$seriesSettings)){
          settingsList[[i]] <- self$seriesSettings[[i]]$toList()
        }
      } else {
        settingsList[[1]] <- ""
      }


      # create list from list of axes
      axesList <- list()
      if(!is.null(self$axes)){
        for (i in 1:length(self$axes)){
          axesList[[i]] <- self$axes[[i]]$toList()
        }
      } else {
        axesList[[1]] <- ""
      }


      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,


        DataType = if(is.null(self$dataType)) "" else self$dataType,
        LegendLocation = if(is.null(self$legendLocation)) {
          ""
        } else {
          self$legendLocation
        },
        IsLegendVisible = if(is.null(self$isLegendVisible)) {
          ""
        } else {
          self$isLegendVisible
        },
        IsLegendOrientationHorizontal =
          if(is.null(self$isLegendOrientationHorizontal)) {
            ""
          } else {
            self$isLegendOrientationHorizontal
          },
        IsLegendPlacementInside = if(is.null(self$isLegendPlacementInside)) {
          ""
        } else {
          self$isLegendPlacementInside
        },
        ChartType = if(is.null(self$chartType)) "" else self$chartType,
        IsStacked = if(is.null(self$isStacked)) "" else self$isStacked,
        IsStacked100 = if(is.null(self$isStacked100)) "" else self$isStacked100,
        IsGridLinesOnXAxis = if(is.null(self$isGridLinesOnXAxis)) {
          ""
        } else {
          self$isGridLinesOnXAxis
        },
        IsGridLinesOnYAxis = if(is.null(self$isGridLinesOnYAxis)) {
          ""
        } else {
          self$isGridLinesOnYAxis
        },
        DataAxesMappings = if(is.null(self$dataAxesMappings)) {
          ""
        } else {
          self$dataAxesMappings
        },
        SeriesSettings = if(is.null(self$seriesSettings)) {
          ""
        } else {
          settingsList
        },
        Axes = if(is.null(self$axes)) "" else axesList,
        ShowToolTipTitle = if(is.null(self$showToolTipTitle)) {
          ""
        } else {
          self$showToolTipTitle
        },
        ShowToolTipX = if(is.null(self$showToolTipX)) "" else self$showToolTipX,
        ShowToolTipY = if(is.null(self$showToolTipY)) "" else self$showToolTipY,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)

#' @title Axis
#'
#' @description Class representing a chart axis object. Axes are used when
#'   creating a chart.
#'
#' @export Axis
#'
#' @field title The title of the axis.
#' @field isOpposed Flag specifying whether the axis is located on the opposing
#'   side of the chart (left or top).
#' @field isLog Flag specifying whether the axis is scaled logarithmically.
#' @field axisId The id of the axis.
#' @field manualMin Integer specifying the manually set minimum of the axis.
#' @field manualMax Integer specifying the manually set maximum of the axis.
#' @field isInversed Flag specifying whether the axis is inversed.
#' @field ticksPerInterval Integer specifying the number of ticks per interval.
#' @examples
#' \dontrun{
#' Axis$new(title = "Fruits eaten",
#'          isOpposed = false,
#'          isLog = false,
#'          axisId = "fruits",
#'          manualMin = NULL,
#'          manualMax = NULL,
#'          isInversed = false,
#'          ticksPerInterval = 10)
#'}
Axis <- R6Class("Axis",
  public = list(
    title = NULL,
    isOpposed = NULL,
    isLog = NULL,
    axisId = NULL,
    manualMin = NULL,
    manualMax = NULL,
    isInversed = NULL,
    ticksPerInterval = NULL,

    #' @description Create a new Axis instance.
    #'
    #' @param title The title of the axis.
    #' @param isOpposed Flag specifying whether the axis is located on the
    #'   opposing side of the chart (left or top).
    #' @param isLog Flag specifying whether the axis is scaled logarithmically.
    #' @param axisId The id of the axis.
    #' @param manualMin Integer specifying the manually set minimum of the axis.
    #' @param manualMax Integer specifying the manually set maximum of the axis.
    #' @param isInversed Flag specifying whether the axis is inversed.
    #' @param ticksPerInterval Integer specifying the number of ticks per
    #'   interval.
    initialize = function(title = NULL,
                          isOpposed = NULL,
                          isLog = NULL,
                          axisId = NULL,
                          manualMin = NULL,
                          manualMax = NULL,
                          isInversed = NULL,
                          ticksPerInterval = NULL){
      self$title <- title
      self$isOpposed <- isOpposed
      self$isLog <- isLog
      self$axisId <- axisId
      self$manualMin <- manualMin
      self$manualMax <- manualMax
      self$isInversed <- isInversed
      self$ticksPerInterval <- ticksPerInterval
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Title = if(is.null(self$title)) "" else self$title,
        IsOpposed = if(is.null(self$isOpposed)) "" else self$isOpposed,
        IsLog = if(is.null(self$isLog)) "" else self$isLog,
        AxisId = if(is.null(self$axisId)) "" else self$axisId,
        ManualMin = if(is.null(self$manualMin)) "" else self$manualMin,
        ManualMax = if(is.null(self$manualMax)) "" else self$manualMax,
        IsInversed = if(is.null(self$isInversed)) "" else self$isInversed,
        TicksPerInterval = if(is.null(self$ticksPerInterval)) {
          ""
        } else {
          self$ticksPerInterval
        }
      )
      return(dl)
    }
  )
)

#' @title SeriesSetting
#'
#' @description Class representing a series setting object. These settings are
#'   used when creating a chart.
#'
#' @export SeriesSetting
#'
#' @field scatterSize An integer giving the scatter size of the series.
#' @field lineThickness An integer specifiying the series' line thickness.
#' @field color A string specifying the series' color.
#' @examples
#' \dontrun{
#' SeriesSetting$new(scatterSize = 2,
#'           lineThickness = 3,
#'           color = "#123456")
#'}
SeriesSetting <- R6Class("SeriesSetting",
  public = list(
    scatterSize = NULL,
    lineThickness = NULL,
    color = NULL,

    #' @description Create a new SeriesSetting instance.
    #'
    #' @param scatterSize An integer giving the scatter size of the series.
    #' @param lineThickness An integer specifiying the series' line thickness.
    #' @param color A string specifying the series' color.
    initialize = function(scatterSize = NULL,
                          lineThickness = NULL,
                          color = NULL){
      self$scatterSize <- scatterSize
      self$lineThickness <- lineThickness
      self$color <- color
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        ScatterSize = if(is.null(self$scatterSize)) "" else self$scatterSize,
        LineThickness = if(is.null(self$lineThickness)) {
          ""
        } else {
          self$lineThickness
        },
        Color = if(is.null(self$color)) "" else self$color
      )
      return(dl)
    }
  )
)
