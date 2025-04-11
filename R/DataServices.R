library("R6")
library("httr")
#' @title DataServices
#'
#' @description Provides access to data related functionality provided through
#' the web API.
#'
#' @details A new instance of this class will be created by the ServiceProvider
#' automatically.
#'
#' @export DataServices
#'
#' @examples \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new(
#'   url = discovery_url,
#'   workspace = workspace,
#'   user = user,
#'   password = password
#' )
#'
#' # load data
#' data <- sp$dataServices$load_data(
#'   c("entity_one", "entity_two"),
#'   lapply(
#'     c("test time string signal one [ ]",
#'       "test time string signal two [ ]"),
#'     function(x) {
#'       sp$parse_signal(x)
#'     }
#'   ),
#'   time_increment = "Daily",
#'   time_start = "2025-01-01T00:00:00",
#'   time_end = "2025-01-02T00:00:00",
#'   reshape = TRUE
#' )
#'
#' # save data
#' time_numeric_data <- data.frame(
#'   c("", ""),
#'   c("2025-01-01T00:00:00", "2025-01-02T00:00:00"),
#'   c("entity_one", "entity_two"),
#'   c(23.45, 45.56),
#'   c(78.89, NaN)
#' )
#'
#' colnames(time_numeric_data) <- c("scenario",
#'                                  "date",
#'                                  "entity",
#'                                  time_numeric_signal_1_name,
#'                                  time_numeric_signal_2_name)
#'
#' result <- sp$dataServices$save_data(
#'   "TimeNumeric",
#'   time_numeric_data,
#'   signals = lapply(
#'     c("test time numeric signal one [m3]",
#'       "test time numeric signal two [m3]"),
#'     function(x) {
#'       sp$parse_signal(x)
#'     }
#'   )
#' )
#' }
DataServices <- R6Class("DataServices",
  public = list(

    #' @description Create a new DataServices instance. This is done by the
    #' ServiceProvider automatically.
    #'
    #' @param url the URL for the API calls.
    #' @param tokenType the type of the issued token.
    #' @param token the issued token.
    initialize = function(url, tokenType, token) {
      private$url <- url
      private$tokenType <- tokenType
      private$token <- token
    },

    #' @description Load data from PetroVisor.
    #' @param entities List of entities to retrieve data for. Either a list of
    #'  strings or a list of items of type Entity.
    #' @param signals List of parsed signals to retrieve data for.
    #' @param scenario_names List of scenario names to load data for.
    #' @param hierarchy_name Hierarchy used in the data retrieval process.
    #' @param top_records Number of records to return,
    #' @param include_workspace_data Whether workspace data shall be included in
    #'   the output (only applies if scenarios are used). Defaults to
    #'   \code{TRUE}.
    #' @param time_increment The time increment to load the data in.
    #' @param time_start The first time stamp data is loaded for.
    #' @param time_end The last time stamp data is loaded for.
    #' @param depth_increment The depth increment to load the data in.
    #' @param depth_start The first depth data is loaded for.
    #' @param depth_end The last depth data is loaded for.
    #' @param with_gaps Whether gaps shall be returned. Defaults to \code{TRUE}.
    #' @param gap_numeric_value Replacement value for gaps in numeric data.
    #' @param gap_string_value Replacement value for gaps in string data.
    #' @param depth_unit The depth unit for retrieving depth data.
    #' @param pressure_unit The pressure unit used when retrieving PVT data.
    #' @param temperature_unit The temperature unit used when retrieving PVT
    #'   data.
    #' @param aggregation Aggregation type applied to the data.
    #' @param reshape Whether to return the raw output of the api call or
    #'   reshape the data into a more user-friendly format. Defaults to
    #'   \code{TRUE}.
    load_data = function(entities,
                         signals,
                         scenario_names = NULL,
                         hierarchy_name = NULL,
                         top_records = NULL,
                         include_workspace_data = TRUE,
                         time_increment = NULL,
                         time_start = NULL,
                         time_end = NULL,
                         depth_increment = NULL,
                         depth_start = NULL,
                         depth_end = NULL,
                         with_gaps = TRUE,
                         gap_numeric_value = NULL,
                         gap_string_value = NULL,
                         depth_unit = NULL,
                         pressure_unit = NULL,
                         temperature_unit = NULL,
                         aggregation = NULL,
                         reshape = TRUE) {

      # Get entity names from input
      entity_names <- lapply(
        entities,
        function(x) {
          if ("Entity" %in% as.list(class(x))) {
            x$name
          } else {
            x
          }
        }
      )

      # Build request
      request <- list()
      request$Combinations <- list(Entities = entity_names, Signals = signals)
      if (!is.null(top_records))
        request$TopRecords <- top_records

      if (!is.null(hierarchy_name))
        request$Hierarchy <- hierarchy_name

      if (!is.null(scenario_names))
        request$Scenarios <- scenario_names

      request$IncludeWorkspaceData <- include_workspace_data
      if (!is.null(time_increment))
        request$TimeIncrement <- time_increment

      if (!is.null(time_start)) {
        request$TimeStart <- time_start
      }
      if (!is.null(time_end))
        request$TimeEnd <- time_end

      if (!is.null(depth_increment))
        request$DepthIncrement <- depth_increment

      if (!is.null(depth_start))
        request$DepthStart <- depth_start

      if (!is.null(depth_end))
        request$DepthEnd <- depth_end

      request$Options <- list(
        WithGaps = with_gaps,
        GapValue = if (!is.null(gap_numeric_value)) {
          gap_numeric_value
        } else {
          "NaN"
        },
        GapStringValue = if (!is.null(gap_string_value)) {
          gap_string_value
        } else {
          ""
        }
      )

      if (!is.null(depth_unit))
        request$DepthUnit <- depth_unit

      if (!is.null(pressure_unit))
        request$PressureUnit <- pressure_unit

      if (!is.null(temperature_unit))
        request$TemperatureUnit <- temperature_unit

      if (!is.null(aggregation))
        request$Aggregation <- aggregation


      # Retrieve data (returns named list)
      data <- private$retrieve_data(request, "Data/Retrieve")

      # Return as is, if reshape is FALSE
      if (!reshape)
        return(data)

      # Reshape data if reshape is TRUE
      data_out <- list()
      # Reshape static numeric data
      if (length(data$StaticNumericData) > 0) {
        # Reshape data -> signal data in columns, drop units
        reshaped_data <- reshape(
          subset(data$StaticNumericData, select = -Unit),
          direction = "wide",
          idvar = c("Entity", "Scenario"),
          timevar = "Signal"
        )

        colnames(reshaped_data) <- lapply(
          colnames(reshaped_data),
          function(x) {
            tolower(gsub("Data.", "", x))
          }
        )

        data_out$StaticNumericData <- reshaped_data
      }

      # Reshape static string data
      if (length(data$StaticStringData) > 0) {
        # Reshape data -> signal data in columns, drop units
        reshaped_data <- reshape(
          subset(data$StaticStringData, select = -Unit),
          direction = "wide",
          idvar = c("Entity", "Scenario"),
          timevar = "Signal"
        )

        colnames(reshaped_data) <- lapply(
          colnames(reshaped_data),
          function(x) {
            tolower(gsub("Data.", "", x))
          }
        )

        data_out$StaticStringData <- reshaped_data
      }

      # Reshape time numeric data
      if (length(data$TimeNumericData) > 0) {
        # Reshape data -> signal data in columns, drop units
        data_out$TimeNumericData <-
          private$unnest_and_reshape(data$TimeNumericData)
      }

      # Reshape time string data
      if (length(data$TimeStringData) > 0) {
        # Reshape data -> signal data in columns, drop units
        data_out$TimeStringData <-
          private$unnest_and_reshape(data$TimeStringData)
      }

      # Reshape depth numeric data
      if (length(data$DepthNumericData) > 0) {
        # Reshape data -> signal data in columns, drop units
        data_out$DepthNumericData <-
          private$unnest_and_reshape(data$DepthNumericData)
      }

      # Reshape depth string data
      if (length(data$DepthStringData) > 0) {
        # Reshape data -> signal data in columns, drop units
        data_out$DepthStringData <-
          private$unnest_and_reshape(data$DepthStringData)
      }

      # Reshape PVT numeric data
      if (length(data$PVTNumericData) > 0) {
        # Reshape data -> signal data in columns, drop units
        data_out$PVTNumericData <-
          private$unnest_and_reshape(data$PVTNumericData)
      }

      return(data_out)
    },

    #' @description Save data to PetroVisor.
    #' @param data_type The type of the data. One of:
    #'  \code{StaticNumeric}, \code{StaticString}, \code{TimeNumeric},
    #'  \code{TimeString}, \code{DepthNumeric}, \code{DepthString},
    #'  \code{PVTNumeric}.
    #' @param data The data as data frame.
    #' @param signals List of parsed signals to save data for.
    #' @param generate_logs Whether to generate log entries. Defaults to
    #'  \code{TRUE}.
    #' @param no_range_delete Whether to skip deleting all data in the saving
    #'  scope before storing the new values. Defaults to \code{TRUE}.
    #' @param values_time_increment The time increment of the time-dependent
    #'  values.
    #' @param values_depth_increment The depth increment of the depth-dependent
    #'  values.
    #' @param pressure_unit The pressure unit of the PVT values.
    #' @param temperature_unit The temperature unit of the PVT values.
    save_data = function(data_type = c("StaticNumeric",
                                       "StaticString",
                                       "TimeNumeric",
                                       "TimeString",
                                       "DepthNumeric",
                                       "DepthString",
                                       "PVTNumeric"),
                         data,
                         signals,
                         generate_logs = TRUE,
                         no_range_delete = TRUE,
                         values_time_increment = NULL,
                         values_depth_increment = NULL,
                         pressure_unit = NULL,
                         temperature_unit = NULL) {
      # Input checks
      data_type <- match.arg(data_type)

      # create unit lookup
      unit_lookup <- data.frame(do.call(rbind, signals))

      # Build body of request
      requests <- list()

      requests$GenerateLogs <- generate_logs
      requests$NoRangeDelete <- no_range_delete

      if (!is.null(values_time_increment))
        requests$ValuesTimeIncrement <- values_time_increment

      if (!is.null(values_depth_increment))
        requests$ValuesDepthIncrement <- values_depth_increment

      if (!is.null(pressure_unit))
        requests$PressureUnit <- pressure_unit

      if (!is.null(temperature_unit))
        requests$TemperatureUnit <- temperature_unit

      # Reshape static numeric data
      if (data_type == "StaticNumeric") {
        # Pivot data into data.frame with columns scenario, entity, signal, data
        pivot <- tidyr::pivot_longer(
          data,
          cols = colnames(data)[! colnames(data) %in% c("entity", "scenario")],
          names_to = "signal",
          values_to = "data"
        )

        # Add unit column
        pivot$unit <- apply(
          pivot,
          1,
          function(x) {
            unlist(unit_lookup[unit_lookup$Signal == x["signal"], "Unit"])
          }
        )

        # Create StaticData objects (make sure to have data as numeric values)
        pivot$objects <- apply(
          pivot,
          1,
          function(x) {
            StaticData$new(
              entity_name = x[["entity"]],
              signal_name = x[["signal"]],
              unit_name = x[["unit"]],
              scenario = x[["scenario"]],
              data = if (is.na(x[["data"]])) NaN else as.numeric(x[["data"]])
            )
          }
        )

        # Add to requests
        requests$StaticNumericData <- lapply(
          pivot$objects,
          function(x) {
            x$to_list()
          }
        )
      }

      # Reshape static string data
      if (data_type == "StaticString") {
        # Pivot data into data.frame with columns scenario, entity, signal, data
        pivot <- tidyr::pivot_longer(
          data,
          cols = colnames(data)[! colnames(data) %in% c("entity", "scenario")],
          names_to = "signal",
          values_to = "data"
        )

        # Add unit column
        pivot$unit <- apply(
          pivot,
          1,
          function(x) {
            unlist(unit_lookup[unit_lookup$Signal == x["signal"], "Unit"])
          }
        )

        # Create StaticData objects (make sure to have data as character values)
        pivot$objects <- apply(
          pivot,
          1,
          function(x) {
            StaticData$new(
              entity_name = x[["entity"]],
              signal_name = x[["signal"]],
              unit_name = x[["unit"]],
              scenario = x[["scenario"]],
              data = as.character(x[["data"]])
            )
          }
        )

        # Add to requests
        requests$StaticStringData <- lapply(
          pivot$objects,
          function(x) {
            x$to_list()
          }
        )
      }

      # Reshape time numeric data
      if (data_type == "TimeNumeric") {
        # Pivot data into data.frame with columns:
        #  scenario, date, entity, signal, value
        pivot <- tidyr::pivot_longer(
          data,
          cols = colnames(data)[! colnames(data) %in% c("entity",
                                                        "scenario",
                                                        "date")],
          names_to = "signal",
          values_to = "value"
        )

        # Create named list for each data point
        #  (make sure to have data as numeric values)
        pivot$data <- apply(
          pivot,
          1,
          function(x) {
            list(Date = x[["date"]], Value = if (is.na(x[["value"]])) {
              NaN
            } else {
              as.numeric(x[["value"]])
            })
          }
        )

        # Nest date and value
        nested <- tidyr::nest(
          subset(pivot, select = c(scenario, entity, signal, data)),
          .by = c("scenario", "entity", "signal")
        )

        # Add unit column
        nested$unit <- apply(
          nested,
          1,
          function(x) {
            unlist(unit_lookup[unit_lookup$Signal == x[["signal"]], "Unit"])
          }
        )

        # Create TimeData objects
        nested$objects <- apply(
          nested,
          1,
          function(x) {
            TimeData$new(
              entity_name = x[["entity"]],
              signal_name = x[["signal"]],
              unit_name = x[["unit"]],
              scenario = x[["scenario"]],
              data = as.list(x[["data"]][[1]])
            )
          }
        )

        requests$TimeNumericData <- lapply(
          nested$objects,
          function(x) {
            x$to_list()
          }
        )
      }

      # Reshape time string data
      if (data_type == "TimeString") {
        # Pivot data into data.frame with columns:
        #  scenario, date, entity, signal, value
        pivot <- tidyr::pivot_longer(
          data,
          cols = colnames(data)[! colnames(data) %in% c("entity",
                                                        "scenario",
                                                        "date")],
          names_to = "signal",
          values_to = "value"
        )

        # Create named list for each data point
        #  (make sure to have data as character values)
        pivot$data <- apply(
          pivot,
          1,
          function(x) {
            list(Date = x[["date"]], Value = as.character(x[["value"]]))
          }
        )

        # Nest date and value
        nested <- tidyr::nest(
          subset(pivot, select = c(scenario, entity, signal, data)),
          .by = c("scenario", "entity", "signal")
        )

        # Add unit column
        nested$unit <- apply(
          nested,
          1,
          function(x) {
            unlist(unit_lookup[unit_lookup$Signal == x[["signal"]], "Unit"])
          }
        )

        # Create TimeData objects
        nested$objects <- apply(
          nested,
          1,
          function(x) {
            TimeData$new(
              entity_name = x[["entity"]],
              signal_name = x[["signal"]],
              unit_name = x[["unit"]],
              scenario = x[["scenario"]],
              data = as.list(x[["data"]][[1]])
            )
          }
        )

        requests$TimeStringData <- lapply(
          nested$objects,
          function(x) {
            x$to_list()
          }
        )
      }

      # Reshape depth numeric data
      if (data_type == "DepthNumeric") {
        # Pivot data into data.frame with columns:
        #  scenario, depth, entity, signal, value
        pivot <- tidyr::pivot_longer(
          data,
          cols = colnames(data)[! colnames(data) %in% c("entity",
                                                        "scenario",
                                                        "depth")],
          names_to = "signal",
          values_to = "value"
        )

        # Create named list for each data point
        #  (make sure to have data as numeric values)
        pivot$data <- apply(
          pivot,
          1,
          function(x) {
            list(Depth = x[["depth"]], Value = if (is.na(x[["value"]])) {
              NaN
            } else {
              as.numeric(x[["value"]])
            })
          }
        )

        # Nest depth and value
        nested <- tidyr::nest(
          subset(pivot, select = c(scenario, entity, signal, data)),
          .by = c("scenario", "entity", "signal")
        )

        # Add unit column
        nested$unit <- apply(
          nested,
          1,
          function(x) {
            unlist(unit_lookup[unit_lookup$Signal == x[["signal"]], "Unit"])
          }
        )

        # Create DepthData objects
        nested$objects <- apply(
          nested,
          1,
          function(x) {
            DepthData$new(
              entity_name = x[["entity"]],
              signal_name = x[["signal"]],
              unit_name = x[["unit"]],
              scenario = x[["scenario"]],
              data = as.list(x[["data"]][[1]])
            )
          }
        )

        requests$DepthNumericData <- lapply(
          nested$objects,
          function(x) {
            x$to_list()
          }
        )
      }

      # Reshape depth string data
      if (data_type == "DepthString") {
        # Pivot data into data.frame with columns:
        #  scenario, depth, entity, signal, value
        pivot <- tidyr::pivot_longer(
          data,
          cols = colnames(data)[! colnames(data) %in% c("entity",
                                                        "scenario",
                                                        "depth")],
          names_to = "signal",
          values_to = "value"
        )

        # Create named list for each data point
        #  (make sure to have data as character values)
        pivot$data <- apply(
          pivot,
          1,
          function(x) {
            list(Depth = x[["depth"]], Value = as.character(x[["value"]]))
          }
        )

        # Nest date and value
        nested <- tidyr::nest(
          subset(pivot, select = c(scenario, entity, signal, data)),
          .by = c("scenario", "entity", "signal")
        )

        # Add unit column
        nested$unit <- apply(
          nested,
          1,
          function(x) {
            unlist(unit_lookup[unit_lookup$Signal == x[["signal"]], "Unit"])
          }
        )

        # Create DepthData objects
        nested$objects <- apply(
          nested,
          1,
          function(x) {
            DepthData$new(
              entity_name = x[["entity"]],
              signal_name = x[["signal"]],
              unit_name = x[["unit"]],
              scenario = x[["scenario"]],
              data = as.list(x[["data"]][[1]])
            )
          }
        )

        requests$DepthStringData <- lapply(
          nested$objects,
          function(x) {
            x$to_list()
          }
        )
      }

      # Reshape PVT data
      if (data_type == "PVTNumeric") {
        # Pivot data into data.frame with columns:
        #  scenario, pressure, temperature, entity, signal, value
        pivot <- tidyr::pivot_longer(
          data,
          cols = colnames(data)[! colnames(data) %in% c("entity",
                                                        "scenario",
                                                        "pressure",
                                                        "temperature")],
          names_to = "signal",
          values_to = "value"
        )

        # Create named list for each data point
        #  (make sure to have data as numeric values)
        pivot$data <- apply(
          pivot,
          1,
          function(x) {
            list(
              Pressure = x[["pressure"]],
              Temperature = x[["temperature"]],
              Value = if (is.na(x[["value"]])) {
                NaN
              } else {
                as.numeric(x[["value"]])
              }
            )
          }
        )

        # Nest pressure, temperature and value
        nested <- tidyr::nest(
          subset(pivot, select = c(scenario, entity, signal, data)),
          .by = c("scenario", "entity", "signal")
        )

        # Add unit column
        nested$unit <- apply(
          nested,
          1,
          function(x) {
            unlist(unit_lookup[unit_lookup$Signal == x[["signal"]], "Unit"])
          }
        )

        # Create PVTData objects
        nested$objects <- apply(
          nested,
          1,
          function(x) {
            PVTData$new(
              entity_name = x[["entity"]],
              signal_name = x[["signal"]],
              unit_name = x[["unit"]],
              scenario = x[["scenario"]],
              data = as.list(x[["data"]][[1]])
            )
          }
        )

        requests$PVTNumericData <- lapply(
          nested$objects,
          function(x) {
            x$to_list()
          }
        )
      }

      # Save data
      return(private$post(requests, "Data/Save"))
    },

    #' @description Remove data from PetroVisor.
    #' @param entities List of entities to delete data for. Either a list of
    #'  strings or a list of items of type Entity.
    #' @param signal_names List of signal names to delete data for.
    #' @param scenario_names List of scenario names to delete data from.
    #' @param include_workspace_data Whether workspace data shall be deleted as
    #'   well (only applies if scenarios are used). Defaults to \code{TRUE}.
    #' @param time_start The first time stamp data is deleted for.
    #' @param time_end The last time stamp data is deleted for.
    #' @param depth_start The first depth data is deleted for.
    #' @param depth_end The last depth data is deleted for.
    delete_data = function(entities,
                           signal_names,
                           scenario_names = NULL,
                           include_workspace_data = TRUE,
                           time_start = NULL,
                           time_end = NULL,
                           depth_start = NULL,
                           depth_end = NULL) {

      # Get entity names from input
      entity_names <- lapply(
        entities,
        function(x) {
          if ("Entity" %in% as.list(class(x))) {
            x$name
          } else {
            x
          }
        }
      )

      # Build request
      request <- list()
      request$Combinations <- list(
        Entities = entity_names,
        Signals = signal_names
      )

      if (!is.null(scenario_names))
        request$Scenarios <- scenario_names

      request$IncludeWorkspaceData <- include_workspace_data

      if (!is.null(time_start))
        request$TimeStart <- time_start

      if (!is.null(time_end))
        request$TimeEnd <- time_end

      if (!is.null(depth_start))
        request$DepthStart <- depth_start

      if (!is.null(depth_end))
        request$DepthEnd <- depth_end

      return(private$post(request, "Data/Delete"))
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL,

    retrieve_data = function(request, url_extension) {
      # get return data
      ret <- httr::POST(
        paste0(
          private$url, url_extension
        ),
        body = jsonlite::toJSON(request, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(
          Authorization = paste(private$tokenType, private$token)
        )
      )

      httr::stop_for_status(ret)
      cont <- httr::content(ret, as = "text")
      return(jsonlite::fromJSON(cont))
    },

    post = function(request, url_extension) {
      ret <- httr::POST(
        paste0(
          private$url, url_extension
        ),
        body = jsonlite::toJSON(request, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(
          Authorization = paste(private$tokenType, private$token)
        )
      )

      httr::stop_for_status(ret)
    },

    unnest_and_reshape = function(data) {
      unnested <- tidyr::unnest(data, cols = names(data), keep_empty = TRUE)
      reshaped_data <- tidyr::pivot_wider(
        unnested,
        names_from = Signal,
        values_from = Value
      )

      colnames(reshaped_data) <- lapply(
        colnames(reshaped_data),
        function(x) {
          tolower(gsub("Data.", "", x))
        }
      )

      return(subset(reshaped_data, select = -unit))
    }
  )
)
