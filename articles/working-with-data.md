# Working with Data Services

``` r
library(Myrconn.PetroVisor.Client)
```

## Overview

The `DataServices` class provides comprehensive functionality for
loading and saving various types of data in PetroVisor. This guide
covers all data types, operations, and best practices based on tested
examples.

## Setup

First, create a service provider instance:

``` r
sp <- ServiceProvider$new(
  url = "https://identity.us1.petrovisor.com",
  workspace = "YourWorkspace",
  user = "your_username",
  password = "your_password"
)

# The data service is accessible via sp$data
```

## Data Types in PetroVisor

PetroVisor supports several data types:

1.  **Static Data**: Entity-level data that doesn’t vary with time or
    depth
2.  **Time Series Data**: Data that varies over time (TimeNumeric,
    TimeString)
3.  **Depth Data**: Data that varies with depth (DepthNumeric,
    DepthString)
4.  **PVT Data**: Pressure-Volume-Temperature dependent data
5.  **Reference Tables**: Structured tabular data with defined schemas
6.  **Pivot Tables**: Pre-aggregated summary tables

## Loading Data with `load_signals()`

The main method for loading data is `sp$data$load_signals()`. This
method handles all data types.

### Basic Loading Pattern

``` r
data <- sp$data$load_signals(
  entities = c("Entity1", "Entity2"),     # Entity names
  signals = list(signal1, signal2),        # Parsed signals
  time_increment = "Daily",                # Time resolution
  time_start = "2024-01-01T00:00:00",     # Start date/time
  time_end = "2024-12-31T23:59:59",       # End date/time
  reshape = TRUE                           # Reshape to wide format
)
```

### Loading Time Series Data

#### Numeric Time Series

``` r
# Parse signal strings
oil_rate <- sp$parse_signal("Oil Rate [bbl/d]")
gas_rate <- sp$parse_signal("Gas Rate [Mscf/d]")
water_rate <- sp$parse_signal("Water Rate [bbl/d]")

# Load daily production data
production <- sp$data$load_signals(
  entities = c("Well-001", "Well-002", "Well-003"),
  signals = list(oil_rate, gas_rate, water_rate),
  time_increment = "Daily",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-12-31T23:59:59",
  reshape = TRUE
)

# Result structure (when reshape = TRUE):
# production$TimeNumericData is a data frame with columns:
# - scenario
# - date
# - entity
# - Oil Rate
# - Gas Rate
# - Water Rate

head(production$TimeNumericData)
```

#### String Time Series

``` r
# Load well status over time
status_signal <- sp$parse_signal("Well Status [ ]")

well_status <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(status_signal),
  time_increment = "Daily",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-12-31T23:59:59",
  reshape = TRUE
)

# Result structure:
# well_status$TimeStringData with columns:
# - scenario, date, entity, Well Status
```

#### Time Increments

Available time increments: - `"Daily"` - Daily data points -
`"Monthly"` - Monthly aggregates - `"Yearly"` - Yearly aggregates

``` r
# Monthly production
monthly_prod <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(oil_rate),
  time_increment = "Monthly",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-12-31T23:59:59",
  reshape = TRUE
)
```

### Loading Static Data

Static data doesn’t require time parameters:

``` r
# Parse static signals
latitude <- sp$parse_signal("Latitude [deg]")
longitude <- sp$parse_signal("Longitude [deg]")

# Load static data (no time parameters needed)
static_data <- sp$data$load_signals(
  entities = c("Well-001", "Well-002", "Well-003"),
  signals = list(latitude, longitude),
  reshape = TRUE
)

# Result structure:
# static_data$StaticNumericData with columns:
# - scenario, entity, Latitude, Longitude
```

### Loading Depth Data

Depth data varies with measured depth:

``` r
# Parse depth signals
porosity <- sp$parse_signal("Porosity [frac]")
permeability <- sp$parse_signal("Permeability [mD]")

# Load depth data
depth_data <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(porosity, permeability),
  depth_increment = "Meter",
  depth_start = 5000,
  depth_end = 7000,
  reshape = TRUE
)

# Result structure:
# depth_data$DepthNumericData with columns:
# - scenario, depth, entity, Porosity, Permeability
```

### Loading PVT Data

PVT data depends on pressure and temperature:

``` r
# PVT signals
oil_fvf <- sp$parse_signal("Oil FVF [rb/stb]")
gas_fvf <- sp$parse_signal("Gas FVF [rcf/scf]")

# Load PVT data
pvt_data <- sp$data$load_signals(
  entities = c("Reservoir-A"),
  signals = list(oil_fvf, gas_fvf),
  pressure_unit = "psi",
  temperature_unit = "degC",
  reshape = TRUE
)

# Result structure:
# pvt_data$PVTNumericData with columns:
# - scenario, pressure, temperature, entity, Oil FVF, Gas FVF
```

### Loading Reference Tables

Reference tables are loaded separately:

``` r
# Load a reference table by name
decline_curve <- sp$data$load_reference_table("Decline Curve Parameters")

# Result is a data frame with the table structure
head(decline_curve)
```

### Loading Pivot Tables

``` r
# Load a pivot table
summary <- sp$data$load_pivot_table("Monthly Production Summary")

# Optionally limit number of rows
summary_limited <- sp$data$load_pivot_table("Monthly Production Summary", 100)
```

### Data Reshaping

The `reshape` parameter controls output format:

``` r
# reshape = FALSE (long format - raw API response)
long_data <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(oil_rate, gas_rate),
  time_increment = "Daily",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-01-31T23:59:59",
  reshape = FALSE
)
# Structure: list with raw API response

# reshape = TRUE (wide format - user-friendly)
wide_data <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(oil_rate, gas_rate),
  time_increment = "Daily",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-01-31T23:59:59",
  reshape = TRUE
)
# Structure: list with reshaped data frames
# wide_data$TimeNumericData has columns:
# - scenario
# - date
# - entity
# - Oil Rate
# - Gas Rate
```

## Saving Data with `save_signals()`

The main method for saving data is `sp$data$save_signals()`.

### Saving Time Series Data

#### Numeric Time Series

``` r
# Prepare data frame with correct column names
time_numeric_data <- data.frame(
  scenario = c("", ""),
  date = c("2024-01-01T00:00:00", "2024-01-02T00:00:00"),
  entity = c("Well-001", "Well-001"),
  oil_rate = c(500, 520),
  gas_rate = c(1500, 1560)
)

# Column names must match signal names (without units)
colnames(time_numeric_data) <- c(
  "scenario", "date", "entity",
  "Oil Rate", "Gas Rate"
)

# Parse signals (with units)
oil_signal <- sp$parse_signal("Oil Rate [bbl/d]")
gas_signal <- sp$parse_signal("Gas Rate [Mscf/d]")

# Save data using correct data type name
result <- sp$data$save_signals(
  "TimeNumeric",  # Data type name
  time_numeric_data,
  signals = list(oil_signal, gas_signal)
)

# Check result
if (result$status_code == 200) {
  cat("Data saved successfully!\n")
}
```

#### String Time Series

``` r
# Prepare string data
time_string_data <- data.frame(
  scenario = c(""),
  date = c("2024-01-01T00:00:00"),
  entity = c("Well-001"),
  status = c("Producing")
)

colnames(time_string_data) <- c(
  "scenario", "date", "entity",
  "Well Status"
)

# Save string data
status_signal <- sp$parse_signal("Well Status [ ]")
result <- sp$data$save_signals(
  "TimeString",
  time_string_data,
  signals = list(status_signal)
)
```

### Saving Static Data

``` r
# Prepare static data
static_data <- data.frame(
  scenario = c("", ""),
  entity = c("Well-001", "Well-002"),
  latitude = c(45.123, 45.234),
  longitude = c(-110.456, -110.567)
)

colnames(static_data) <- c(
  "scenario", "entity",
  "Latitude", "Longitude"
)

# Parse signals
lat_signal <- sp$parse_signal("Latitude [deg]")
lon_signal <- sp$parse_signal("Longitude [deg]")

# Save static data
result <- sp$data$save_signals(
  "StaticNumeric",
  static_data,
  signals = list(lat_signal, lon_signal)
)
```

### Saving Depth Data

``` r
# Prepare depth data
depth_data <- data.frame(
  scenario = c("", ""),
  entity = c("Well-001", "Well-001"),
  depth = c(5000, 5001),
  porosity = c(0.15, 0.16),
  permeability = c(50, 55)
)

colnames(depth_data) <- c(
  "scenario", "entity", "depth",
  "Porosity", "Permeability"
)

# Save depth data
por_signal <- sp$parse_signal("Porosity [frac]")
perm_signal <- sp$parse_signal("Permeability [mD]")

result <- sp$data$save_signals(
  "DepthNumeric",
  depth_data,
  signals = list(por_signal, perm_signal)
)
```

### Saving PVT Data

``` r
# Prepare PVT data
pvt_data <- data.frame(
  scenario = c("", ""),
  temperature = c(100, 100),
  pressure = c(150, 200),
  entity = c("Reservoir-A", "Reservoir-A"),
  oil_fvf = c(1.15, 1.20),
  gas_fvf = c(0.005, 0.006)
)

colnames(pvt_data) <- c(
  "scenario", "temperature", "pressure", "entity",
  "Oil FVF", "Gas FVF"
)

# Save PVT data
oil_fvf_signal <- sp$parse_signal("Oil FVF [rb/stb]")
gas_fvf_signal <- sp$parse_signal("Gas FVF [rcf/scf]")

result <- sp$data$save_signals(
  "PVTNumeric",
  pvt_data,
  signals = list(oil_fvf_signal, gas_fvf_signal),
  pressure_unit = "psi",
  temperature_unit = "degC"
)
```

### Saving Reference Tables

``` r
# Prepare reference table data
ref_table_data <- data.frame(
  Entity = c(NA, "Well-001", NA, "Well-001"),
  Timestamp = c(NA, "2024-01-01T00:00:00", NA, NA),
  ID = c(1, 2, 3, 4),
  Name = c("Param1", "Param2", "Param3", "Param4"),
  Value = c(100, 200, 300, 400)
)

# Save reference table data
result <- sp$data$save_reference_table("Parameters", ref_table_data)
```

### Saving Pivot Tables

``` r
# Pivot tables are generated/saved using their definitions
# No direct data saving - data is generated by the pivot table formula
result <- sp$data$save_pivot_table("Monthly Production Summary")
```

### Handling Missing Values

Use `NA` or `NaN` for missing numeric values:

``` r
# Data with missing values
data_with_na <- data.frame(
  scenario = c("", "", ""),
  date = c("2024-01-01T00:00:00", "2024-01-02T00:00:00", "2024-01-03T00:00:00"),
  entity = c("Well-001", "Well-001", "Well-001"),
  oil_rate = c(500, NA, 520),  # Missing value on Jan 2
  gas_rate = c(1500, 1550, NaN)  # NaN on Jan 3
)

colnames(data_with_na) <- c(
  "scenario", "date", "entity",
  "Oil Rate", "Gas Rate"
)

# PetroVisor handles NA/NaN appropriately
result <- sp$data$save_signals("TimeNumeric", data_with_na, signals)
```

## Deleting Data

### Delete Signal Data

``` r
# Delete specific signal data for entities
result <- sp$data$delete_signals(
  entities = c("Well-001", "Well-002"),
  signals = c("Oil Rate", "Gas Rate"),  # Signal names without units
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-01-31T23:59:59"
)
```

### Delete Reference Table Data

``` r
# Delete all data
result <- sp$data$delete_reference_table("Parameters")

# Delete with WHERE clause
result <- sp$data$delete_reference_table(
  "Parameters",
  where = "[Value] > 200"
)
```

### Delete Pivot Table Data

``` r
result <- sp$data$delete_pivot_table("Monthly Production Summary")
```

## Working with Signals and Units

### Signal Parsing

PetroVisor signals are strings that combine name and unit:

``` r
# Parse a signal string (format: "Name [unit]")
signal <- sp$parse_signal("Oil Production Rate [bbl/d]")

# Access components
signal$Signal  # "Oil Production Rate"
signal$Unit    # "bbl/d"

# Use in data operations
data <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(signal),
  time_increment = "Daily",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-01-31T23:59:59",
  reshape = TRUE
)
```

### Unit Conversion

Convert data between units:

``` r
# Load data in bbl/d
oil_data <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(sp$parse_signal("Oil Rate [bbl/d]")),
  time_increment = "Daily",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-01-31T23:59:59",
  reshape = TRUE
)

# Convert oil rate column from bbl/d to m3/d
oil_data$TimeNumericData$`Oil Rate` <- sp$convert_unit(
  x = oil_data$TimeNumericData$`Oil Rate`,
  source_unit = "bbl/d",
  target_unit = "m3/d"
)

# Now oil_data has rates in m3/d
```

### Special Unit Characters

Some units require special handling:

``` r
# Space as unit (dimensionless)
dimensionless <- sp$parse_signal("Recovery Factor [ ]")

# Percent
percentage <- sp$parse_signal("Water Cut [%]")

# Converting percent to fraction
fraction <- sp$convert_unit(10, "%", " ")  # Result: 0.1

# Converting fraction to percent
percent <- sp$convert_unit(0.1, " ", "%")  # Result: 10

# Units with slashes are handled automatically
rate <- sp$parse_signal("Production Rate [bbl/d]")
pressure_rate <- sp$parse_signal("Pressure Gradient [psi/ft]")
```

## Advanced Data Operations

### Batch Loading Multiple Entities

``` r
# Get list of entities
all_wells <- sp$items$load_names("Entity")

# Filter to wells of interest
production_wells <- all_wells[grepl("^PROD-", all_wells)]

# Load data for all production wells
all_production <- sp$data$load_signals(
  entities = production_wells,
  signals = list(
    sp$parse_signal("Oil Rate [bbl/d]"),
    sp$parse_signal("Gas Rate [Mscf/d]"),
    sp$parse_signal("Water Rate [bbl/d]")
  ),
  time_increment = "Monthly",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-12-31T23:59:59",
  reshape = TRUE
)
```

### Limiting Results with `top_records`

``` r
# Load only the first 10 records
limited_data <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(sp$parse_signal("Oil Rate [bbl/d]")),
  time_increment = "Daily",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-12-31T23:59:59",
  top_records = 10,
  reshape = TRUE
)
```

### Working with Scenarios

``` r
# Load data from specific scenarios
scenario_data <- sp$data$load_signals(
  entities = c("Well-001"),
  signals = list(sp$parse_signal("Oil Rate [bbl/d]")),
  scenario_names = c("Base Case", "Optimistic"),
  time_increment = "Monthly",
  time_start = "2024-01-01T00:00:00",
  time_end = "2024-12-31T23:59:59",
  include_workspace_data = TRUE,
  reshape = TRUE
)
```

## Data Type Names Reference

When using `save_signals()`, use these exact data type names:

- **Static data**: `"StaticNumeric"`, `"StaticString"`
- **Time series**: `"TimeNumeric"`, `"TimeString"`
- **Depth data**: `"DepthNumeric"`, `"DepthString"`
- **PVT data**: `"PVTNumeric"`

## Best Practices

### 1. Always Parse Signals

``` r
# GOOD - Parse signal strings
signal <- sp$parse_signal("Oil Rate [bbl/d]")
data <- sp$data$load_signals(entities = wells, signals = list(signal), ...)

# Avoid manually constructing signal objects
```

### 2. Use Correct Date Format

``` r
# GOOD - ISO 8601 format
time_start <- "2024-01-01T00:00:00"
time_end <- "2024-12-31T23:59:59"

# BAD - Other formats will fail
# time_start <- "2024-01-01"
# time_start <- "01/01/2024"
```

### 3. Match Column Names to Signals

``` r
# Signal name: "Oil Rate [bbl/d]"
# Column name in data frame: "Oil Rate" (without unit)

data <- data.frame(
  scenario = c(""),
  date = c("2024-01-01T00:00:00"),
  entity = c("Well-001"),
  oil_rate = c(500)
)

# Set column name to match signal name (without unit)
colnames(data)[4] <- "Oil Rate"

# Now save with signal including unit
result <- sp$data$save_signals(
  "TimeNumeric",
  data,
  signals = list(sp$parse_signal("Oil Rate [bbl/d]"))
)
```

### 4. Handle Errors Gracefully

``` r
# Wrap data operations in tryCatch
result <- tryCatch({
  sp$data$save_signals(
    "TimeNumeric",
    my_data,
    signals = my_signals
  )
}, error = function(e) {
  cat("Error saving data:", e$message, "\n")
  return(list(status_code = 500))
})

if (result$status_code == 200) {
  cat("Data saved successfully\n")
}
```

### 5. Check Data Structure Before Saving

``` r
# Validate data structure
str(data_to_save)
head(data_to_save)

# Check column names match signals (without units)
expected_cols <- c("scenario", "date", "entity", "Oil Rate", "Gas Rate")
actual_cols <- colnames(data_to_save)

if (!all(expected_cols %in% actual_cols)) {
  stop("Column names don't match expected signal names")
}

# Check for missing values
cat("Missing values per column:\n")
print(colSums(is.na(data_to_save)))

# Then save
result <- sp$data$save_signals(...)
```

## Common Issues and Solutions

### Issue: “Column not found” error

**Cause**: Column names in data frame don’t match signal names

**Solution**: Ensure column names exactly match signal names (without
units)

``` r
# If signal is "Oil Rate [bbl/d]", column must be "Oil Rate"
colnames(my_data)[which(colnames(my_data) == "oil_rate")] <- "Oil Rate"
```

### Issue: “Invalid date format” error

**Cause**: Date not in ISO 8601 format

**Solution**: Use `"YYYY-MM-DDTHH:MM:SS"` format

``` r
# Convert dates to correct format
my_data$date <- format(as.POSIXct(my_data$date), "%Y-%m-%dT%H:%M:%S")
```

### Issue: Data not appearing after save

**Cause**: Wrong data type name used

**Solution**: Use correct data type name (e.g., “TimeNumeric” not
“Time”)

``` r
# CORRECT
result <- sp$data$save_signals("TimeNumeric", data, signals)

# INCORRECT
# result <- sp$data$save_signals("Time", data, signals)
```

## Next Steps

- **[Repository
  Service](https://datagration.github.io/petrovisor-r-api/articles/repository-service.md)**:
  Learn about creating signals and entities
- **[Machine
  Learning](https://datagration.github.io/petrovisor-r-api/articles/machine-learning.md)**:
  Use your data for ML models
- **[Getting
  Started](https://datagration.github.io/petrovisor-r-api/articles/getting-started.md)**:
  Package overview

## See Also

- [`?DataServices`](https://datagration.github.io/petrovisor-r-api/reference/DataServices.md):
  Complete data services documentation
- [`?ServiceProvider`](https://datagration.github.io/petrovisor-r-api/reference/ServiceProvider.md):
  Service provider documentation
- Test files in `tests/testthat/test_data_services.R` for more examples
