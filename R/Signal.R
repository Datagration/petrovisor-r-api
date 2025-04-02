library("R6")

#' @title Signal
#'
#' @description Class representing a PetroVisor signal object.
#'
#' @export Signal
#'
#' @field name The name of the signal.
#' @field short_name The signal's short name.
#' @field measurement_name The signal's measurement name.
#' @field storage_unit_name The name of the signal's storage unit.
#' @field aggregation_type The signal's aggregation type.
#' @field container_aggregation_type The signal's container aggregation type.
#' @field signal_type The signal's type.
#' @field default_color The signal's default color.
#' @field default_line_type The signal's default line type.
#' @field setting_name (Optional) The name of the enumeration linked to the
#'   signal.
#' @field labels A list of strings holding the labels of the signal.
#' @field description The description of the signal.
#'
#' @examples
#' \dontrun{
#' Signal$new(name = "my signal",
#'            short_name = "my sig",
#'            measurement_name = "Length",
#'            storage_unit_name = "m",
#'            aggregation_type = "Average",
#'            container_aggregation_type = "Sum",
#'            signal_type = "Time-dependent",
#'            default_color = 0,
#'            default_line_type = "Solid",
#'            setting_name = NULL,
#'            labels = list(),
#'            description = NULL)
#'}
Signal <- R6Class("Signal",
  public = list(
    name = NULL,
    short_name = NULL,
    measurement_name = NULL,
    storage_unit_name = NULL,
    aggregation_type = NULL,
    container_aggregation_type = NULL,
    signal_type = NULL,
    default_color = NULL,
    default_line_type = NULL,
    setting_name = NULL,
    labels = NULL,
    description = NULL,

    #' @description Create a new Signal instance.
    #'
    #' @param name The name of the signal.
    #' @param short_name The signal's short name.
    #' @param measurement_name The signal's measurement name.
    #' @param storage_unit_name The name of the signal's storage unit.
    #' @param aggregation_type The signal's aggregation type.
    #' @param container_aggregation_type The signal's container aggregation
    #'   type.
    #' @param signal_type The signal's type.
    #' @param default_color The signal's default color.
    #' @param default_line_type The signal's default line type.
    #' @param setting_name (Optional) The name of the enumeration linked to the
    #'   signal.
    #' @param labels A list of strings holding the labels of the signal.
    #' @param description The description of the signal.
    initialize = function(name = NULL,
                          short_name = NULL,
                          measurement_name = NULL,
                          storage_unit_name = NULL,
                          aggregation_type = c("Sum",
                                               "Average",
                                               "Mode",
                                               "Max",
                                               "Min",
                                               "Count",
                                               "Count Distinct",
                                               "First",
                                               "Last",
                                               "None"),
                          container_aggregation_type = c("Sum",
                                                         "Average",
                                                         "Mode",
                                                         "Max",
                                                         "Min",
                                                         "Count",
                                                         "Count Distinct",
                                                         "First",
                                                         "Last",
                                                         "None"),
                          signal_type = c("Static",
                                          "TimeDependent",
                                          "DepthDependent",
                                          "String",
                                          "PVT",
                                          "StringTimeDependent",
                                          "StringDepthDependent"),
                          default_color = 0,
                          default_line_type = c("Solid", "Dash", "DashDot"),
                          setting_name = NULL,
                          labels = list(),
                          description = NULL) {
      self$name <- name
      self$short_name <- short_name
      self$measurement_name <- measurement_name
      self$storage_unit_name <- storage_unit_name
      self$aggregation_type <- match.arg(aggregation_type)
      self$container_aggregation_type <- match.arg(container_aggregation_type)
      self$signal_type <- match.arg(signal_type)
      self$default_color <- default_color
      self$default_line_type <- match.arg(default_line_type)
      self$setting_name <- setting_name
      self$labels <- labels
      self$description <- description
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        ShortName = if (is.null(self$short_name)) "" else self$short_name,
        MeasurementName =
          if (is.null(self$measurement_name)) {
            ""
          } else {
            self$measurement_name
          },
        StorageUnitName =
          if (is.null(self$storage_unit_name)) {
            ""
          } else {
            self$storage_unit_name
          },
        AggregationType =
          if (is.null(self$aggregation_type)) {
            ""
          } else {
            self$aggregation_type
          },
        ContainerAggregationType =
          if (is.null(self$container_aggregation_type)) {
            ""
          } else {
            self$container_aggregation_type
          },
        SignalType = if (is.null(self$signal_type)) "" else self$signal_type,
        DefaultColor =
          if (is.null(self$default_color)) {
            ""
          } else {
            self$default_color
          },
        DefaultLineType =
          if (is.null(self$default_line_type)) {
            ""
          } else {
            self$default_line_type
          },
        SettingName = if (is.null(self$setting_name)) "" else self$setting_name,
        Description = if (is.null(self$description)) "" else self$description,
        Labels = self$labels
      )
      return(dl)
    }
  )
)
