library("R6")

#' @title Signal
#'
#' @description Class representing a PetroVisor signal object.
#'
#' @export Signal
#'
#' @field name The name of the signal.
#' @field shortName The signal's short name.
#' @field measurementName The signal's measurement name.
#' @field storageUnitName The name of the signal's storage unit.
#' @field aggregationType The signal's aggregation type.
#' @field containerAggregationType The signal's container aggregation type.
#' @field signalType The signal's type.
#' @field defaultColor The signal's default color.
#' @field defaultLineType The signal's default line type.
#' @field settingName (Optional) The name of the enumeration linked to the
#'   signal.
#'
#' @examples
#' \dontrun{
#' Signal$new(name = "my signal",
#'            shortName = "my sig",
#'            measurementName = "Length",
#'            storageUnitName = "m",
#'            aggregationType = "Average",
#'            containerAggregationType = "Sum",
#'            signalType = "Time-dependent",
#'            defaultColor = 0,
#'            defaultLineType = "Solid",
#'            settingName = NULL)
#'}
Signal <- R6Class("Signal",
  public = list(
    name = NULL,
    shortName = NULL,
    measurementName = NULL,
    storageUnitName = NULL,
    aggregationType = NULL,
    containerAggregationType = NULL,
    signalType = NULL,
    defaultColor = NULL,
    defaultLineType = NULL,
    settingName = NULL,

    #' @description Create a new Signal instance.
    #'
    #' @param name The name of the signal.
    #' @param shortName The signal's short name.
    #' @param measurementName The signal's measurement name.
    #' @param storageUnitName The name of the signal's storage unit.
    #' @param aggregationType The signal's aggregation type.
    #' @param containerAggregationType The signal's container aggregation type.
    #' @param signalType The signal's type.
    #' @param defaultColor The signal's default color.
    #' @param defaultLineType The signal's default line type.
    #' @param settingName (Optional) The name of the enumeration linked to the
    #'   signal.
    initialize = function(name = NULL,
                          shortName = NULL,
                          measurementName = NULL,
                          storageUnitName = NULL,
                          aggregationType = c("Sum",
                                             "Average",
                                             "Max",
                                             "Min",
                                             "Count"),
                          containerAggregationType = c("Sum",
                                                       "Average",
                                                       "Max",
                                                       "Min",
                                                       "Count"),
                          signalType = c("Static",
                                         "Time-dependent",
                                         "Depth-dependent",
                                         "String",
                                         "PVT",
                                         "String time-dependent"),
                          defaultColor = 0,
                          defaultLineType = c("Solid", "Dash", "DashDot"),
                          settingName = NULL){
      self$name <- name
      self$shortName <- shortName
      self$measurementName <- measurementName
      self$storageUnitName <- storageUnitName
      self$aggregationType <- match.arg(aggregationType)
      self$containerAggregationType <- match.arg(containerAggregationType)
      self$signalType <- match.arg(signalType)
      self$defaultColor <- defaultColor
      self$defaultLineType <- match.arg(defaultLineType)
      self$settingName <- settingName
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        ShortName = if(is.null(self$shortName)) "" else self$shortName,
        MeasurementName = if(is.null(self$measurementName)) {
          ""
        } else {
          self$measurementName
        },
        StorageUnitName = if(is.null(self$storageUnitName)) {
          ""
        } else {
          self$storageUnitName
        },
        AggregationType = if(is.null(self$aggregationType)) {
          ""
        } else {
          self$aggregationType
        },
        ContainerAggregationType = if(is.null(self$containerAggregationType)) {
          ""
        } else {
          self$containerAggregationType
        },
        SignalType = if(is.null(self$signalType)) "" else self$signalType,
        DefaultColor = if(is.null(self$defaultColor)) "" else self$defaultColor,
        DefaultLineType = if(is.null(self$defaultLineType)) {
          ""
        } else {
          self$defaultLineType
        },
        SettingName = if(is.null(self$settingName)) "" else self$settingName
        )
      return(dl)
    }
  )
)
