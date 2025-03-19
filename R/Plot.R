library("R6")

#' @title Plot
#'
#' @description Class representing a PetroVisor plot object.
#'
#' @export Plot
#'
#' @field name The name of the plot.
#' @field plotType The type of the plot.
#' @field inputTableNames A list giving the names of the input tables used
#'   to create the plot.
#' @field xAxisColumnName The name of the column for the x-axis.
#' @field xAxisUnitName The name of the unit of the x-axis.
#' @field yAxisColumnName The name of the column for the y-axis.
#' @field yAxisUnitName The name of the unit of the y-axis.
#' @field ryAxisColumnName The name of the column for the right y-axis.
#' @field ryAxisUnitName The name of the unit of the right y-axis.
#' @field sizeColumnName The name of the column for the size (if scatter-plot).
#' @field sizeUnitName The name of the unit of the size (if scatter-plot).
#' @field formula The plot's definition as string (P# syntax).
#' @field isLocked This flag specifies whether the plot is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the plot belongs to.
#' @field isFavorite This flag specifies whether the plot is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the plot.
#' @examples
#' \dontrun{
#' Plot$new( )
#'}
Plot <- R6Class("Plot",
  public = list(
    name = NULL,
    plotType = NULL,
    inputTableNames = NULL,
    xAxisColumnName = NULL,
    xAxisUnitName = NULL,
    yAxisColumnName = NULL,
    yAxisUnitName = NULL,
    ryAxisColumnName = NULL,
    ryAxisUnitName = NULL,
    sizeColumnName = NULL,
    sizeUnitName = NULL,
    formula = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Plot instance.
    #'
    #' @param name The name of the plot.
    #' @param plotType The type of the plot.
    #' @param inputTableNames A list giving the names of the input tables used
    #'   to create the plot.
    #' @param xAxisColumnName The name of the column for the x-axis.
    #' @param xAxisUnitName The name of the unit of the x-axis.
    #' @param yAxisColumnName The name of the column for the y-axis.
    #' @param yAxisUnitName The name of the unit of the y-axis.
    #' @param ryAxisColumnName The name of the column for the right y-axis.
    #' @param ryAxisUnitName The name of the unit of the right y-axis.
    #' @param sizeColumnName The name of the column for the size (if
    #'   scatter-plot).
    #' @param sizeUnitName The name of the unit of the size (if scatter-plot).
    #' @param formula The plot's definition as string (P# syntax).
    #' @param isLocked This flag specifies whether the plot is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the plot belongs to.
    #' @param isFavorite This flag specifies whether the plot is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the plot.
    initialize = function(name = NULL,
                          plotType = NULL,
                          inputTableNames = NULL,
                          xAxisColumnName = NULL,
                          xAxisUnitName = NULL,
                          yAxisColumnName = NULL,
                          yAxisUnitName = NULL,
                          ryAxisColumnName = NULL,
                          ryAxisUnitName = NULL,
                          sizeColumnName = NULL,
                          sizeUnitName = NULL,
                          formula = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$plotType <- plotType
      self$inputTableNames <- inputTableNames
      self$xAxisColumnName <- xAxisColumnName
      self$xAxisUnitName <- xAxisUnitName
      self$yAxisColumnName <- yAxisColumnName
      self$yAxisUnitName <- yAxisUnitName
      self$ryAxisColumnName <- ryAxisColumnName
      self$ryAxisUnitName <- ryAxisUnitName
      self$sizeColumnName <- sizeColumnName
      self$sizeUnitName <- sizeUnitName
      self$formula <- formula
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
        PlotType = if(is.null(self$plotType)) "" else self$plotType,
        InputTableNames = if(is.null(self$inputTableNames)) {
          ""
        } else {
          self$inputTableNames
        },

        XAxis = list(
          ColumnName = if(is.null(self$xAxisColumnName)) {
            ""
          } else {
            self$xAxisColumnName
          },
          UnitName = if(is.null(self$xAxisUnitName)) {
            ""
          } else {
            self$xAxisUnitName
          }),
        YAxis = list(
          ColumnName = if(is.null(self$yAxisColumnName)) {
            ""
          } else {
            self$yAxisColumnName
          },
          UnitName = if(is.null(self$yAxisUnitName)) {
            ""
          } else {
            self$yAxisUnitName
          }),
        RYAxis = list(
          ColumnName = if(is.null(self$ryAxisColumnName)) {
            ""
          } else {
            self$ryAxisColumnName
          },
          UnitName = if(is.null(self$ryAxisUnitName)) {
            ""
          } else {
            self$ryAxisUnitName
          }),
        Size = list(
          ColumnName = if(is.null(self$sizeColumnName)) {
            ""
          } else {
            self$sizeColumnName
          },
          UnitName = if(is.null(self$sizeUnitName)) {
            ""
          } else {
            self$sizeUnitName
          }),
        Formula = if(is.null(self$formula)) "" else self$formula,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)
