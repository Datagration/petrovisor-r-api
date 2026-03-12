# DataServices

Provides access to data related functionality provided through the web
API.

## Details

A new instance of this class will be created by the ServiceProvider
automatically.

## Note

When using time-based data loading, ensure time_start and time_end are
in ISO 8601 format (e.g., "2025-01-01T00:00:00").

Large data requests may take significant time to process. Consider using
top_records parameter to limit result size.

Reference table must exist in PetroVisor before loading data. Use
`sp$items$load_names("ReferenceTable")` to list available tables.

Pivot table data is read-only. Use `save_pivot_table()` to regenerate
the data if source data has changed.

Data frame must contain required columns: entity, scenario, and
additional columns depending on data_type (date for time data, depth for
depth data, etc.).

When no_range_delete = FALSE, existing data in the scope will be deleted
before saving new values.

## Methods

- Data Loading:

  load_signals(), load_reference_table(), load_pivot_table()

- Data Saving:

  save_signals(), save_reference_table(), save_pivot_table()

- Data Deletion:

  delete_signals(), delete_reference_table(), delete_pivot_table()

## Data Types Supported

- Static Data:

  Entity-level data that doesn't change over time or depth.

- Time Series Data:

  Data that varies over time (daily, monthly, etc.)

- Depth Data:

  Data that varies with depth or measured depth.

- PVT Data:

  Pressure-Volume-Temperature dependent data.

- Reference Tables:

  Structured tabular data with defined schemas.

- Pivot Tables:

  Pre-aggregated summary tables.

## See also

Related classes:

- [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
  for creating a service provider instance

- [RepositoryService](https://datagration.github.io/petrovisor-r-api/reference/RepositoryService.md)
  for managing entities and signals

Data type classes:

- [StaticData](https://datagration.github.io/petrovisor-r-api/reference/StaticData.md)
  for static data structure

- [TimeData](https://datagration.github.io/petrovisor-r-api/reference/TimeData.md)
  for time series data structure

- [DepthData](https://datagration.github.io/petrovisor-r-api/reference/DepthData.md)
  for depth data structure

- [PVTData](https://datagration.github.io/petrovisor-r-api/reference/PVTData.md)
  for PVT data structure

- [ReferenceTable](https://datagration.github.io/petrovisor-r-api/reference/ReferenceTable.md)
  for reference table structure

- [PivotTable](https://datagration.github.io/petrovisor-r-api/reference/PivotTable.md)
  for pivot table structure

Related objects:

- [Signal](https://datagration.github.io/petrovisor-r-api/reference/Signal.md)
  for signal definitions

- [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
  for entity definitions

- [Unit](https://datagration.github.io/petrovisor-r-api/reference/Unit.md)
  for unit definitions

Vignettes:

- [`vignette("working-with-data")`](https://datagration.github.io/petrovisor-r-api/articles/working-with-data.md)
  for comprehensive data operations guide

- [`vignette("getting-started")`](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)
  for basic usage

## Super class

[`Myrconn.PetroVisor.Client::ApiRequests`](https://datagration.github.io/petrovisor-r-api/reference/ApiRequests.md)
-\> `DataServices`

## Methods

### Public methods

- [`DataServices$new()`](#method-DataServices-new)

- [`DataServices$load_signals()`](#method-DataServices-load_signals)

- [`DataServices$load_reference_table()`](#method-DataServices-load_reference_table)

- [`DataServices$load_pivot_table()`](#method-DataServices-load_pivot_table)

- [`DataServices$save_signals()`](#method-DataServices-save_signals)

- [`DataServices$save_reference_table()`](#method-DataServices-save_reference_table)

- [`DataServices$save_pivot_table()`](#method-DataServices-save_pivot_table)

- [`DataServices$delete_signals()`](#method-DataServices-delete_signals)

- [`DataServices$delete_reference_table()`](#method-DataServices-delete_reference_table)

- [`DataServices$delete_pivot_table()`](#method-DataServices-delete_pivot_table)

- [`DataServices$clone()`](#method-DataServices-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DataServices instance. This is done by the
[ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)
automatically.

#### Usage

    DataServices$new(sp)

#### Arguments

- `sp`:

  Instance of the
  [ServiceProvider](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md)

------------------------------------------------------------------------

### Method `load_signals()`

Load signal data from PetroVisor.

#### Usage

    DataServices$load_signals(
      entities,
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
      reshape = TRUE
    )

#### Arguments

- `entities`:

  List of entities to retrieve data for. Can be:

  - Character vector: `c("Well_001", "Well_002")`

  - List of
    [Entity](https://datagration.github.io/petrovisor-r-api/reference/Entity.md)
    objects: `list(entity1, entity2)`

- `signals`:

  List of parsed
  [Signal](https://datagration.github.io/petrovisor-r-api/reference/Signal.md)
  objects created using `sp$parse_signal()`. Each signal should specify
  name and unit.

- `scenario_names`:

  List of scenario names to load data for.

- `hierarchy_name`:

  [Hierarchy](https://datagration.github.io/petrovisor-r-api/reference/Hierarchy.md)
  used in the data retrieval process.

- `top_records`:

  Number of records to return.

- `include_workspace_data`:

  Whether workspace data shall be included in the output (only applies
  if scenarios are used). Defaults to `TRUE`.

- `time_increment`:

  The time increment to load the data in.

- `time_start`:

  The first time stamp data is loaded for.

- `time_end`:

  The last time stamp data is loaded for.

- `depth_increment`:

  The depth increment to load the data in.

- `depth_start`:

  The first depth data is loaded for.

- `depth_end`:

  The last depth data is loaded for.

- `with_gaps`:

  Whether gaps shall be returned. Defaults to `TRUE`.

- `gap_numeric_value`:

  Replacement value for gaps in numeric data.

- `gap_string_value`:

  Replacement value for gaps in string data.

- `depth_unit`:

  The depth unit for retrieving depth data.

- `pressure_unit`:

  The pressure unit used when retrieving PVT data.

- `temperature_unit`:

  The temperature unit used when retrieving PVT data.

- `aggregation`:

  Aggregation type applied to the data.

- `reshape`:

  Whether to return the raw output of the api call or reshape the data
  into a more user-friendly format. Defaults to `TRUE`.

#### Details

This method supports loading multiple data types simultaneously and
automatically handles data reshaping for user convenience. The method
supports various filtering options:

- **Entity Filtering**: Specify entities as strings or Entity objects

- **Time Filtering**: Use ISO 8601 format for time_start/time_end

- **Depth Filtering**: Specify depth ranges with depth_start/depth_end

- **Scenario Filtering**: Load specific scenarios or workspace data

When `reshape = TRUE` (default), the returned data structure contains:

- `StaticNumericData/StaticStringData`: Wide format with entities as
  rows, signals as columns

- `TimeNumericData/TimeStringData`: Long format with Date, Entity, and
  signal columns

- `DepthNumericData/DepthStringData`: Long format with Depth, Entity,
  and signal columns

- `PVTNumericData`: Long format with Pressure, Temperature, Entity, and
  signal columns

#### Returns

A list containing the loaded data, structured by data type
(StaticNumericData, TimeNumericData, etc.) when reshape = TRUE, or raw
API response when reshape = FALSE.

#### Examples

    \dontrun{
      # Time-series data with specific time range
      time_data <- sp$data$load(
        entities = c("Well_A"),
        signals = lapply(
          c("oil production [bbl/d]", "gas production [MSCF/d]"),
          function(x) { sp$parse_signal(x) }
        ),
        time_increment = "Daily",
        time_start = "2024-01-01T00:00:00",
        time_end = "2024-12-31T23:59:59",
        reshape = TRUE
      )
    }

------------------------------------------------------------------------

### Method `load_reference_table()`

Load reference table data from PetroVisor.

#### Usage

    DataServices$load_reference_table(
      table,
      entities = NULL,
      time_start = NULL,
      time_end = NULL,
      top_records = NULL,
      key_unit_name = NULL,
      columns = NULL,
      specified_columns_only = FALSE,
      where = NULL
    )

#### Arguments

- `table`:

  The name of the reference table to load from.

- `entities`:

  List of entities to retrieve data for. Either a list of strings or a
  list of items of type Entity.

- `time_start`:

  The first time stamp data is loaded for.

- `time_end`:

  The last time stamp data is loaded for.

- `top_records`:

  Number of records to return.

- `key_unit_name`:

  The unit in which to retrieve the key values.

- `columns`:

  List of strings defining the columns and units to be retrieved. E.g.
  list("column name 1 unit", "column name 2 unit"). When
  `specified_columns_only = TRUE`, only columns given in this argument
  will be returned. Otherwise, this argument will be used to define the
  units in which the column data will be returned.

- `specified_columns_only`:

  Whether to return the columns specified in `columns` only. Defaults to
  `FALSE`.

- `where`:

  WHERE-like clause to filter the reference table data.

#### Returns

A data frame containing the loaded reference table data.

#### Examples

    \dontrun{
      # Basic reference table loading
      ref_data <- sp$data$load_reference_table(
        table = "Well Properties"
      )

      # With entity filtering
      ref_data <- sp$data$load_reference_table(
        table = "Production History",
        entities = c("Well_A", "Well_B")
      )

      # With time filtering
      ref_data <- sp$data$load_reference_table(
        table = "Monthly Reports",
        entities = c("Field_1"),
        time_start = "2024-01-01T00:00:00",
        time_end = "2024-12-31T23:59:59"
      )

      # With specific columns and units
      ref_data <- sp$data$load_reference_table(
        table = "Reservoir Properties",
        entities = c("Reservoir_A"),
        columns = list(
          "porosity [fraction]",
          "permeability [mD]",
          "thickness [ft]"
        ),
        specified_columns_only = TRUE,
        key_unit_name = "ft"
      )

      # With WHERE clause and top records
      ref_data <- sp$data$load_reference_table(
        table = "Well Completion Data",
        where = "[WellType] = 'Horizontal' AND [TVD] > 5000",
        top_records = 50
      )
    }

------------------------------------------------------------------------

### Method `load_pivot_table()`

Load pivot table data from PetroVisor.

#### Usage

    DataServices$load_pivot_table(table, top_records = NULL)

#### Arguments

- `table`:

  The name of the pivot table to load from.

- `top_records`:

  Number of records to return.

#### Returns

A data frame containing the loaded pivot table data.

#### Examples

    \dontrun{
    # Basic pivot table loading
      pivot_data <- sp$data$load_pivot_table(
        table = "Production Summary"
      )

      # With record limit
      pivot_data <- sp$data$load_pivot_table(
        table = "Monthly Production Report",
        top_records = 100
      )
    }

------------------------------------------------------------------------

### Method `save_signals()`

Save signal data to PetroVisor.

#### Usage

    DataServices$save_signals(
      data_type = c("StaticNumeric", "StaticString", "TimeNumeric", "TimeString",
        "DepthNumeric", "DepthString", "PVTNumeric"),
      data,
      signals,
      generate_logs = TRUE,
      no_range_delete = TRUE,
      values_time_increment = NULL,
      values_depth_increment = NULL,
      pressure_unit = NULL,
      temperature_unit = NULL
    )

#### Arguments

- `data_type`:

  The type of the data. One of: `StaticNumeric`, `StaticString`,
  `TimeNumeric`, `TimeString`, `DepthNumeric`, `DepthString`,
  `PVTNumeric`.

- `data`:

  The data as data frame.

- `signals`:

  List of parsed signals to save data for.

- `generate_logs`:

  Whether to generate log entries. Defaults to `TRUE`.

- `no_range_delete`:

  Whether to skip deleting all data in the saving scope before storing
  the new values. Defaults to `TRUE`.

- `values_time_increment`:

  The time increment of the time-dependent values.

- `values_depth_increment`:

  The depth increment of the depth-dependent values.

- `pressure_unit`:

  The pressure unit of the PVT values.

- `temperature_unit`:

  The temperature unit of the PVT values.

#### Returns

A response object from the API indicating success or failure of the save
operation.

#### Examples

    \dontrun{
      # Static numeric data
      static_numeric_data <- data.frame(
        scenario = c("", ""),
        entity = c("Well_A", "Well_B"),
        `initial oil reserves` = c(1000000, 750000),
        `initial gas reserves` = c(2000000, 1500000)
      )

      result <- sp$data$save_signals(
        data_type = "StaticNumeric",
        data = static_numeric_data,
        signals = lapply(
          c("initial oil reserves [bbl]", "initial gas reserves [MSCF]"),
          function(x) { sp$parse_signal(x) }
        ),
        generate_logs = TRUE,
        no_range_delete = FALSE
      )

      # Static string data
      static_string_data <- data.frame(
         scenario = c(""),
        entity = c("Well_A"),
        `well type` = c("Horizontal"),
        `completion type` = c("Multi-stage frac")
      )

      result <- sp$data$save_signals(
        data_type = "StaticString",
        data = static_string_data,
        signals = lapply(
          c("well type [ ]", "completion type [ ]"),
          function(x) { sp$parse_signal(x) }
        )
      )

      # Time numeric data
      time_numeric_data <- data.frame(
        scenario = c("", "", ""),
        date = c("2024-01-01T00:00:00",
                 "2024-01-02T00:00:00",
                 "2024-01-03T00:00:00"),
        entity = c("Well_A", "Well_A", "Well_A"),
        `oil rate` = c(1000, 950, 900),
        `gas rate` = c(2000, 1900, 1800)
      )

      result <- sp$data$save_signals(
        data_type = "TimeNumeric",
        data = time_numeric_data,
        signals = lapply(
          c("oil rate [bbl/d]", "gas rate [MSCF/d]"),
          function(x) { sp$parse_signal(x) }
        ),
        values_time_increment = "Daily"
      )

      # Time string data
      time_string_data <- data.frame(
        scenario = c("", ""),
        date = c("2024-01-01T00:00:00", "2024-01-02T00:00:00"),
        entity = c("Well_A", "Well_A"),
        `operation status` = c("Producing", "Shut-in"),
        `maintenance notes` = c("Normal operation", "Scheduled maintenance")
      )

      result <- sp$data$save_signals(
        data_type = "TimeString",
        data = time_string_data,
        signals = lapply(
          c("operation status [ ]", "maintenance notes [ ]"),
          function(x) { sp$parse_signal(x) }
        )
      )

      # Depth numeric data
      depth_numeric_data <- data.frame(
        scenario = c("", ""),
        depth = c(1000, 1010),
        entity = c("Well_A", "Well_A"),
        porosity = c(0.15, 0.18),
        permeability = c(100, 150)
      )

      result <- sp$data$save_signals(
        data_type = "DepthNumeric",
        data = depth_numeric_data,
        signals = lapply(
          c("porosity [fraction]", "permeability [mD]"),
          function(x) { sp$parse_signal(x) }
        ),
        values_depth_increment = "Meter"
      )

      # Depth string data
      depth_string_data <- data.frame(
        scenario = c("", ""),
        depth = c(1000, 1010),
        entity = c("Well_A", "Well_A"),
        `rock type` = c("Sandstone", "Shale"),
        `formation name` = c("Formation A", "Formation B")
      )

      result <- sp$data$save_signals(
        data_type = "DepthString",
        data = depth_string_data,
        signals = lapply(
          c("rock type [ ]", "formation name [ ]"),
          function(x) { sp$parse_signal(x) }
        )
      )

      # PVT numeric data
      pvt_numeric_data <- data.frame(
        scenario = c("", ""),
        temperature = c(200, 250),
        pressure = c(3000, 4000),
        entity = c("Fluid_Sample_1", "Fluid_Sample_1"),
        density = c(800, 820),
        viscosity = c(1.2, 1.5)
      )

      result <- sp$data$save_signals(
        data_type = "PVTNumeric",
        data = pvt_numeric_data,
        signals = lapply(
          c("density [kg/m3]", "viscosity [cP]"),
          function(x) { sp$parse_signal(x) }
        ),
        pressure_unit = "psi",
        temperature_unit = "degF"
      )
    }

------------------------------------------------------------------------

### Method `save_reference_table()`

Save reference table data to PetroVisor.

#### Usage

    DataServices$save_reference_table(table, data, skip_existing = FALSE)

#### Arguments

- `table`:

  The name of the reference table to save to.

- `data`:

  The data to save as data frame.

- `skip_existing`:

  Whether to skip existing rows (defined by key!) or update all rows.
  Defaults to `FALSE`, which means all rows will be updated.

#### Returns

A response object from the API indicating success or failure of the save
operation.

#### Examples

    \dontrun{
      # Basic reference table data saving
      ref_table_data <- data.frame(
        Entity = c("Well_A", "Well_B", "Well_C"),
        Timestamp = c("2024-01-01T00:00:00",
                      "2024-01-01T00:00:00",
                      "2024-01-01T00:00:00"),
        KeyColumn = c(1, 2, 3),
        WellName = c("Alpha-1", "Beta-2", "Gamma-3"),
        TVD = c(5000, 5500, 6000),
        Status = c("Active", "Shut-in", "Active")
      )

      result <- sp$data$save_reference_table(
        table = "Well Master Data",
        data = ref_table_data,
        skip_existing = FALSE
      )

      # Skip existing records
      result <- sp$data$save_reference_table(
        table = "Well Master Data",
        data = ref_table_data,
        skip_existing = TRUE
      )
    }

------------------------------------------------------------------------

### Method `save_pivot_table()`

Generate and save the data of the specified pivot table.

#### Usage

    DataServices$save_pivot_table(table)

#### Arguments

- `table`:

  The name of the pivot table to generate and save.

#### Returns

A response object from the API indicating success or failure of the save
operation.

#### Examples

    \dontrun{
      # Generate and save pivot table data
      result <- sp$data$save_pivot_table(
        table = "Monthly Production Summary"
      )
    }

------------------------------------------------------------------------

### Method `delete_signals()`

Remove signal data from PetroVisor.

#### Usage

    DataServices$delete_signals(
      entities,
      signal_names,
      scenario_names = NULL,
      include_workspace_data = TRUE,
      time_start = NULL,
      time_end = NULL,
      depth_start = NULL,
      depth_end = NULL
    )

#### Arguments

- `entities`:

  List of entities to delete data for. Either a list of strings or a
  list of items of type Entity.

- `signal_names`:

  List of signal names to delete data for.

- `scenario_names`:

  List of scenario names to delete data from.

- `include_workspace_data`:

  Whether workspace data shall be deleted as well (only applies if
  scenarios are used). Defaults to `TRUE`.

- `time_start`:

  The first time stamp data is deleted for.

- `time_end`:

  The last time stamp data is deleted for.

- `depth_start`:

  The first depth data is deleted for.

- `depth_end`:

  The last depth data is deleted for.

#### Returns

A response object from the API indicating success or failure of the
delete operation.

#### Examples

    \dontrun{
      # Delete all signal data for specific entities
      result <- sp$data$delete_signals(
        entities = c("Well_A", "Well_B"),
        signal_names = c("oil production", "gas production")
      )

      # Delete time-range specific data
      result <- sp$data$delete_signals(
        entities = c("Well_C"),
        signal_names = c("daily production"),
        time_start = "2024-01-01T00:00:00",
        time_end = "2024-01-31T23:59:59"
      )

      # Delete depth-range specific data
      result <- sp$data$delete_signals(
        entities = c("Well_D"),
        signal_names = c("porosity", "permeability"),
        depth_start = 1000,
        depth_end = 2000
      )

      # Delete scenario-specific data
      result <- sp$data$delete_signals(
        entities = c("Well_E"),
        signal_names = c("forecast production"),
        scenario_names = c("Pessimistic Case"),
        include_workspace_data = FALSE
      )
    }

------------------------------------------------------------------------

### Method `delete_reference_table()`

Remove reference table data from PetroVisor.

#### Usage

    DataServices$delete_reference_table(table, where = NULL)

#### Arguments

- `table`:

  The name of the reference table to delete from.

- `where`:

  WHERE-like clause to filter the reference table data to delete. If
  `NULL` (default), all data will be deleted.

#### Returns

A response object from the API indicating success or failure of the
delete operation.

#### Examples

    \dontrun{
      # Delete all data from reference table
      result <- sp$data$delete_reference_table(
        table = "Temporary Data"
      )

      # Delete with WHERE clause
      result <- sp$data$delete_reference_table(
        table = "Well Completion Data",
        where = "[Status] = 'Abandoned'"
      )

      # Delete specific date range
      result <- sp$data$delete_reference_table(
        table = "Monthly Reports",
        where = "[ReportDate] < '2023-01-01'"
      )
    }

------------------------------------------------------------------------

### Method `delete_pivot_table()`

Remove pivot table data from PetroVisor.

#### Usage

    DataServices$delete_pivot_table(table)

#### Arguments

- `table`:

  The name of the pivot table to delete the data for.

#### Returns

A response object from the API indicating success or failure of the
delete operation.

#### Examples

    \dontrun{
      # Delete pivot table data
      result <- sp$data$delete_pivot_table(
        table = "Temporary Analysis"
      )
    }

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataServices$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# create a new instance of the service provider
sp <- ServiceProvider$new(
  url = discovery_url,
  workspace = workspace,
  user = user,
  password = password
)

# load data
data <- sp$data$load(
  c("entity_one", "entity_two"),
  lapply(
    c("test time string signal one [ ]",
      "test time string signal two [ ]"),
    function(x) {
      sp$parse_signal(x)
    }
  ),
  time_increment = "Daily",
  time_start = "2025-01-01T00:00:00",
  time_end = "2025-01-02T00:00:00",
  reshape = TRUE
)

# save data
time_numeric_data <- data.frame(
  c("", ""),
  c("2025-01-01T00:00:00", "2025-01-02T00:00:00"),
  c("entity_one", "entity_two"),
  c(23.45, 45.56),
  c(78.89, NaN)
)

colnames(time_numeric_data) <- c("scenario",
                                 "date",
                                 "entity",
                                 time_numeric_signal_1_name,
                                 time_numeric_signal_2_name)

result <- sp$data$save(
  "TimeNumeric",
  time_numeric_data,
  signals = lapply(
    c("test time numeric signal one [m3]",
      "test time numeric signal two [m3]"),
    function(x) {
      sp$parse_signal(x)
    }
  )
)
} # }

## ------------------------------------------------
## Method `DataServices$load_signals`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Time-series data with specific time range
  time_data <- sp$data$load(
    entities = c("Well_A"),
    signals = lapply(
      c("oil production [bbl/d]", "gas production [MSCF/d]"),
      function(x) { sp$parse_signal(x) }
    ),
    time_increment = "Daily",
    time_start = "2024-01-01T00:00:00",
    time_end = "2024-12-31T23:59:59",
    reshape = TRUE
  )
} # }

## ------------------------------------------------
## Method `DataServices$load_reference_table`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Basic reference table loading
  ref_data <- sp$data$load_reference_table(
    table = "Well Properties"
  )

  # With entity filtering
  ref_data <- sp$data$load_reference_table(
    table = "Production History",
    entities = c("Well_A", "Well_B")
  )

  # With time filtering
  ref_data <- sp$data$load_reference_table(
    table = "Monthly Reports",
    entities = c("Field_1"),
    time_start = "2024-01-01T00:00:00",
    time_end = "2024-12-31T23:59:59"
  )

  # With specific columns and units
  ref_data <- sp$data$load_reference_table(
    table = "Reservoir Properties",
    entities = c("Reservoir_A"),
    columns = list(
      "porosity [fraction]",
      "permeability [mD]",
      "thickness [ft]"
    ),
    specified_columns_only = TRUE,
    key_unit_name = "ft"
  )

  # With WHERE clause and top records
  ref_data <- sp$data$load_reference_table(
    table = "Well Completion Data",
    where = "[WellType] = 'Horizontal' AND [TVD] > 5000",
    top_records = 50
  )
} # }

## ------------------------------------------------
## Method `DataServices$load_pivot_table`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Basic pivot table loading
  pivot_data <- sp$data$load_pivot_table(
    table = "Production Summary"
  )

  # With record limit
  pivot_data <- sp$data$load_pivot_table(
    table = "Monthly Production Report",
    top_records = 100
  )
} # }

## ------------------------------------------------
## Method `DataServices$save_signals`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Static numeric data
  static_numeric_data <- data.frame(
    scenario = c("", ""),
    entity = c("Well_A", "Well_B"),
    `initial oil reserves` = c(1000000, 750000),
    `initial gas reserves` = c(2000000, 1500000)
  )

  result <- sp$data$save_signals(
    data_type = "StaticNumeric",
    data = static_numeric_data,
    signals = lapply(
      c("initial oil reserves [bbl]", "initial gas reserves [MSCF]"),
      function(x) { sp$parse_signal(x) }
    ),
    generate_logs = TRUE,
    no_range_delete = FALSE
  )

  # Static string data
  static_string_data <- data.frame(
     scenario = c(""),
    entity = c("Well_A"),
    `well type` = c("Horizontal"),
    `completion type` = c("Multi-stage frac")
  )

  result <- sp$data$save_signals(
    data_type = "StaticString",
    data = static_string_data,
    signals = lapply(
      c("well type [ ]", "completion type [ ]"),
      function(x) { sp$parse_signal(x) }
    )
  )

  # Time numeric data
  time_numeric_data <- data.frame(
    scenario = c("", "", ""),
    date = c("2024-01-01T00:00:00",
             "2024-01-02T00:00:00",
             "2024-01-03T00:00:00"),
    entity = c("Well_A", "Well_A", "Well_A"),
    `oil rate` = c(1000, 950, 900),
    `gas rate` = c(2000, 1900, 1800)
  )

  result <- sp$data$save_signals(
    data_type = "TimeNumeric",
    data = time_numeric_data,
    signals = lapply(
      c("oil rate [bbl/d]", "gas rate [MSCF/d]"),
      function(x) { sp$parse_signal(x) }
    ),
    values_time_increment = "Daily"
  )

  # Time string data
  time_string_data <- data.frame(
    scenario = c("", ""),
    date = c("2024-01-01T00:00:00", "2024-01-02T00:00:00"),
    entity = c("Well_A", "Well_A"),
    `operation status` = c("Producing", "Shut-in"),
    `maintenance notes` = c("Normal operation", "Scheduled maintenance")
  )

  result <- sp$data$save_signals(
    data_type = "TimeString",
    data = time_string_data,
    signals = lapply(
      c("operation status [ ]", "maintenance notes [ ]"),
      function(x) { sp$parse_signal(x) }
    )
  )

  # Depth numeric data
  depth_numeric_data <- data.frame(
    scenario = c("", ""),
    depth = c(1000, 1010),
    entity = c("Well_A", "Well_A"),
    porosity = c(0.15, 0.18),
    permeability = c(100, 150)
  )

  result <- sp$data$save_signals(
    data_type = "DepthNumeric",
    data = depth_numeric_data,
    signals = lapply(
      c("porosity [fraction]", "permeability [mD]"),
      function(x) { sp$parse_signal(x) }
    ),
    values_depth_increment = "Meter"
  )

  # Depth string data
  depth_string_data <- data.frame(
    scenario = c("", ""),
    depth = c(1000, 1010),
    entity = c("Well_A", "Well_A"),
    `rock type` = c("Sandstone", "Shale"),
    `formation name` = c("Formation A", "Formation B")
  )

  result <- sp$data$save_signals(
    data_type = "DepthString",
    data = depth_string_data,
    signals = lapply(
      c("rock type [ ]", "formation name [ ]"),
      function(x) { sp$parse_signal(x) }
    )
  )

  # PVT numeric data
  pvt_numeric_data <- data.frame(
    scenario = c("", ""),
    temperature = c(200, 250),
    pressure = c(3000, 4000),
    entity = c("Fluid_Sample_1", "Fluid_Sample_1"),
    density = c(800, 820),
    viscosity = c(1.2, 1.5)
  )

  result <- sp$data$save_signals(
    data_type = "PVTNumeric",
    data = pvt_numeric_data,
    signals = lapply(
      c("density [kg/m3]", "viscosity [cP]"),
      function(x) { sp$parse_signal(x) }
    ),
    pressure_unit = "psi",
    temperature_unit = "degF"
  )
} # }

## ------------------------------------------------
## Method `DataServices$save_reference_table`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Basic reference table data saving
  ref_table_data <- data.frame(
    Entity = c("Well_A", "Well_B", "Well_C"),
    Timestamp = c("2024-01-01T00:00:00",
                  "2024-01-01T00:00:00",
                  "2024-01-01T00:00:00"),
    KeyColumn = c(1, 2, 3),
    WellName = c("Alpha-1", "Beta-2", "Gamma-3"),
    TVD = c(5000, 5500, 6000),
    Status = c("Active", "Shut-in", "Active")
  )

  result <- sp$data$save_reference_table(
    table = "Well Master Data",
    data = ref_table_data,
    skip_existing = FALSE
  )

  # Skip existing records
  result <- sp$data$save_reference_table(
    table = "Well Master Data",
    data = ref_table_data,
    skip_existing = TRUE
  )
} # }

## ------------------------------------------------
## Method `DataServices$save_pivot_table`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Generate and save pivot table data
  result <- sp$data$save_pivot_table(
    table = "Monthly Production Summary"
  )
} # }

## ------------------------------------------------
## Method `DataServices$delete_signals`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Delete all signal data for specific entities
  result <- sp$data$delete_signals(
    entities = c("Well_A", "Well_B"),
    signal_names = c("oil production", "gas production")
  )

  # Delete time-range specific data
  result <- sp$data$delete_signals(
    entities = c("Well_C"),
    signal_names = c("daily production"),
    time_start = "2024-01-01T00:00:00",
    time_end = "2024-01-31T23:59:59"
  )

  # Delete depth-range specific data
  result <- sp$data$delete_signals(
    entities = c("Well_D"),
    signal_names = c("porosity", "permeability"),
    depth_start = 1000,
    depth_end = 2000
  )

  # Delete scenario-specific data
  result <- sp$data$delete_signals(
    entities = c("Well_E"),
    signal_names = c("forecast production"),
    scenario_names = c("Pessimistic Case"),
    include_workspace_data = FALSE
  )
} # }

## ------------------------------------------------
## Method `DataServices$delete_reference_table`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Delete all data from reference table
  result <- sp$data$delete_reference_table(
    table = "Temporary Data"
  )

  # Delete with WHERE clause
  result <- sp$data$delete_reference_table(
    table = "Well Completion Data",
    where = "[Status] = 'Abandoned'"
  )

  # Delete specific date range
  result <- sp$data$delete_reference_table(
    table = "Monthly Reports",
    where = "[ReportDate] < '2023-01-01'"
  )
} # }

## ------------------------------------------------
## Method `DataServices$delete_pivot_table`
## ------------------------------------------------

if (FALSE) { # \dontrun{
  # Delete pivot table data
  result <- sp$data$delete_pivot_table(
    table = "Temporary Analysis"
  )
} # }
```
