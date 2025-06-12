library("R6")

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
#' data <- sp$data$load(
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
#' result <- sp$data$save(
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
DataServices <- R6Class(
  "DataServices",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(

    #' @description Create a new DataServices instance. This is done by the
    #' ServiceProvider automatically.
    #'
    #' @param url the URL for the API calls.
    #' @param token_type the type of the issued token.
    #' @param token the issued token.
    #' @param sp instance of the service provider
    initialize = function(url, token_type, token, sp) {
      private$url <- url
      private$token_type <- token_type
      private$token <- token
      private$sp <- sp
    },

    #' @description Load data from PetroVisor. This function is deprecated.
    #'  Use \code{load_signals()} or \code{load_reference_table()} instead.
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
    # ToDo: remove in version 3.2.0
    load = function(entities,
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
      lifecycle::deprecate_warn("3.1.0", "load()", "load_signals()")

      return(self$load_signals(entities,
                               signals,
                               scenario_names,
                               hierarchy_name,
                               top_records,
                               include_workspace_data,
                               time_increment,
                               time_start,
                               time_end,
                               depth_increment,
                               depth_start,
                               depth_end,
                               with_gaps,
                               gap_numeric_value,
                               gap_string_value,
                               depth_unit,
                               pressure_unit,
                               temperature_unit,
                               aggregation,
                               reshape))
    },

    #' @description Load signal data from PetroVisor.
    #' @param entities List of entities to retrieve data for. Either a list of
    #'  strings or a list of items of type Entity.
    #' @param signals List of parsed signals to retrieve data for.
    #' @param scenario_names List of scenario names to load data for.
    #' @param hierarchy_name Hierarchy used in the data retrieval process.
    #' @param top_records Number of records to return.
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
    load_signals = function(entities,
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
      entity_names <- private$get_entity_names(entities)

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
      data <- private$post(request,
                           private$url,
                           "Data/Retrieve",
                           private$token_type,
                           private$token,
                           expect_data = TRUE)

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

    #' @description Load reference table data from PetroVisor.
    #'
    #' @param table The name of the reference table to load from.
    #' @param entities List of entities to retrieve data for. Either a list of
    #'  strings or a list of items of type Entity.
    #' @param time_start The first time stamp data is loaded for.
    #' @param time_end The last time stamp data is loaded for.
    #' @param top_records Number of records to return.
    #' @param key_unit_name The unit in which to retrieve the key values.
    #' @param columns List of strings defining the columns and units to be
    #'   retrieved. E.g. list("column name 1 [unit]", "column name 2 [unit]").
    #'   When \code{specified_columns_only = TRUE}, only columns given in this
    #'   argument will be returned. Otherwise, this argument will be used to
    #'   define the units in which the column data will be returned.
    #' @param specified_columns_only Whether to return the columns specified in
    #'   \code{columns} only. Defaults to \code{FALSE}.
    #' @param where WHERE-like clause to filter the reference table data.
    load_reference_table = function(table,
                                    entities = NULL,
                                    time_start = NULL,
                                    time_end = NULL,
                                    top_records = NULL,
                                    key_unit_name = NULL,
                                    columns = NULL,
                                    specified_columns_only = FALSE,
                                    where = NULL) {

      # Get entity names from input
      entity_names <- private$get_entity_names(entities)

      # Build request
      request <- list()
      request$Entities <- entity_names

      if (!is.null(time_start))
        request$StartTimestamp <- time_start

      if (!is.null(time_end))
        request$EndTimestamp <- time_end

      if (!is.null(top_records))
        request$TopRows <- top_records

      if (!is.null(key_unit_name))
        request$KeyUnitName <- key_unit_name

      if (!is.null(columns)) {
        column_units <- list()
        for (i in seq_along(columns)) {
          parsed <- private$sp$parse_signal(columns[[i]])
          column_units[[parsed$Signal]] <- parsed$Unit
        }
        request$ValuesUnitNames <- column_units
      }

      request$ReturnOnlySpecifiedValuesUnitNames <- specified_columns_only
      request$WhereExpression <- where

      # Retrieve data (data does not contain the column names)
      data <- private$post(request,
                           private$url,
                           paste0("RefTables/", table, "/Data"),
                           private$token_type,
                           private$token,
                           expect_data = TRUE)

      if (length(data) == 0)
        return(data.frame())

      # add column names (needs the table definition)
      table_definition <- private$sp$items$load("ReferenceTable", table)

      # these are always there
      column_names <- c("Entity", "Timestamp", table_definition$key$name)

      # depending on the input, either append the selected column names only,
      # or get all column names from the table definition
      if (specified_columns_only) {
        # handle instances where specified_columns_only is TRUE, but no columns
        # are given
        if (is.null(columns)) {
          column_names <- c(
            column_names,
            lapply(
              table_definition$values,
              function(x) {
                x$name
              }
            )
          )
        } else {
          column_names <- c(column_names, names(request$ValuesUnitNames))
        }
      } else {
        column_names <- c(
          column_names,
          lapply(
            table_definition$values,
            function(x) {
              x$name
            }
          )
        )
      }

      colnames(data) <- column_names
      data <- as.data.frame(data)

      # convert strings to data type mentioned in column definitions
      # position 4 in the column_names variable is the first value column
      # the key column is handled separately
      for (name in column_names[4:length(column_names)]) {
        # get column definition
        column_definition <-
          table_definition$values[sapply(
            table_definition$values, function(x) {
              x$name == name
            }
          )][[1]]

        switch(column_definition$column_type,
          Numeric = data[, name] <- as.double(data[, name]),
          Boolean = data[, name] <- as.logical(data[, name])
        )
      }

      # handle conversion of the key column
      switch(table_definition$key$column_type,
        Numeric = data[, table_definition$key$name] <-
          as.double(data[, table_definition$key$name]),
        Boolean = data[, table_definition$key$name] <-
          as.logical(data[, table_definition$key$name])
      )

      return(data)
    },

    #' @description Load pivot table data from PetroVisor.
    #'
    #' @param table The name of the pivot table to load from.
    #' @param top_records Number of records to return.
    load_pivot_table = function(table, top_records = NULL) {
      data <- private$get(private$url,
                          paste0("PivotTables/", table, "/Saved"),
                          private$token_type,
                          private$token,
                          query = list(RowCount = top_records))
      # restructure data
      # promote first row to colnames
      df <- data.frame(data[-1, ])
      colnames(df) <- data[1,]

      return(df)
    },

    #' @description Save data to PetroVisor. This function is deprecated.
    #'  Use \code{save_signals()} or \code{save_reference_table()} instead.
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
    # ToDo: remove in version 3.2.0
    save = function(data_type = c("StaticNumeric",
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
      lifecycle::deprecate_warn("3.1.0", "save()", "save_signals()")

      return(self$save_signals(data_type,
                               data, signals,
                               generate_logs,
                               no_range_delete,
                               values_time_increment,
                               values_depth_increment,
                               pressure_unit,
                               temperature_unit))
    },

    #' @description Save signal data to PetroVisor.
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
    save_signals = function(data_type = c("StaticNumeric",
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
      return(private$post(requests,
                          private$url,
                          "Data/Save",
                          private$token_type,
                          private$token))
    },

    #' @description Save reference table data to PetroVisor.
    #' @param table The name of the reference table to save to.
    #' @param data The data to save as data frame.
    #' @param skip_existing Whether to skip existing rows (defined by key!) or
    #'   update all rows. Defaults to \code{FALSE}, which means all rows will be
    #'   updated.
    save_reference_table = function(table, data, skip_existing = FALSE) {
      data <- as.matrix(data)
      body <- split(data, seq_len(nrow(data)))
      names(body) <- NULL
      return(private$put(body,
                         private$url,
                         paste0("RefTables/", table, "/Data/String"),
                         private$token_type,
                         private$token,
                         query = list(SkipExistingData = skip_existing)))
    },

    #' @description Generate and save the data of the specified pivot table.
    #' @param table The name of the pivot table to generate and save.
    save_pivot_table = function(table) {
      return(private$get(private$url,
                         paste0("PivotTables/", table, "/Save"),
                         private$token_type,
                         private$token,
                         parse_json = FALSE))
    },

    #' @description Remove data from PetroVisor. This function is deprecated.
    #'  Use \code{delete_signals()} or \code{delete_reference_table()} instead.
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
    # ToDo: remove in version 3.2.0
    delete = function(entities,
                      signal_names,
                      scenario_names = NULL,
                      include_workspace_data = TRUE,
                      time_start = NULL,
                      time_end = NULL,
                      depth_start = NULL,
                      depth_end = NULL) {

      lifecycle::deprecate_warn("3.1.0", "delete()", "delete_signals()")

      return(self$delete_signals(entities,
                                 signal_names,
                                 scenario_names,
                                 include_workspace_data,
                                 time_start,
                                 time_end,
                                 depth_start,
                                 depth_end))
    },

    #' @description Remove signal data from PetroVisor.
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
    delete_signals = function(entities,
                              signal_names,
                              scenario_names = NULL,
                              include_workspace_data = TRUE,
                              time_start = NULL,
                              time_end = NULL,
                              depth_start = NULL,
                              depth_end = NULL) {

      # Get entity names from input
      entity_names <- private$get_entity_names(entities)

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

      return(private$post(request,
                          private$url,
                          "Data/Delete",
                          private$token_type,
                          private$token))
    },

    #' @description Remove reference table data from PetroVisor.
    #' @param table The name of the reference table to delete from.
    #' @param where WHERE-like clause to filter the reference table data to
    #'   delete. If \code{NULL} (default), all data will be deleted.
    delete_reference_table = function(table, where = NULL) {
      # set name NULL, because it is appended to the route in ApiRequests.R
      return(private$delete(name = NULL,
                            private$url,
                            paste0("RefTables/", table, "/Data"),
                            private$token_type,
                            private$token,
                            query = list(WhereExpression = where)))
    },

    #' @description Remove pivot table data from PetroVisor.
    #' @param table The name of the pivot table to delete the data for.
    delete_pivot_table = function(table) {
      return(private$get(private$url,
                         paste0("PivotTables/", table, "/Delete"),
                         private$token_type,
                         private$token,
                         parse_json = FALSE))
    }
  ),
  private = list(
    url = NULL,
    token_type = NULL,
    token = NULL,
    sp = NULL,

    get_entity_names = function(entities) {
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

      return(entity_names)
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
