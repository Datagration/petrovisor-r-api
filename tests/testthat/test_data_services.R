context("DataService Tests")

# Setup
# Create test entity
entity <- Entity$new(name = "TestName",
                     entity_type_name = "Well",
                     alias = "TestAlias",
                     is_opportunity = FALSE)

sp$items$save("Entity", entity)

# Create test signals
# Static numeric signals
static_numeric_signal_1 <- Signal$new(name = "test static numeric signal one",
                                      short_name = "tsns1",
                                      measurement_name = "Length",
                                      storage_unit_name = "m",
                                      aggregation_type = "Sum",
                                      container_aggregation_type = "Sum",
                                      signal_type = "Static",
                                      default_color = 3711337,
                                      default_line_type = "Dash",
                                      setting_name = NULL,
                                      labels = list(),
                                      description = "My description")

result <- sp$items$save("Signal", static_numeric_signal_1)

static_numeric_signal_2 <- Signal$new(name = "test static numeric signal two",
                                      short_name = "tsns2",
                                      measurement_name = "Length",
                                      storage_unit_name = "m",
                                      aggregation_type = "Sum",
                                      container_aggregation_type = "Sum",
                                      signal_type = "Static",
                                      default_color = 3711337,
                                      default_line_type = "Dash",
                                      setting_name = NULL,
                                      labels = list(),
                                      description = "My description")

result <- sp$items$save("Signal", static_numeric_signal_2)

# Static string signals
static_string_signal_1 <- Signal$new(name = "test static string signal one",
                                     short_name = "tsss1",
                                     measurement_name = "Dimensionless",
                                     storage_unit_name = " ",
                                     aggregation_type = "Sum",
                                     container_aggregation_type = "Sum",
                                     signal_type = "String",
                                     default_color = 3711337,
                                     default_line_type = "Dash",
                                     setting_name = NULL,
                                     labels = list(),
                                     description = "My description")

result <- sp$items$save("Signal", static_string_signal_1)

static_string_signal_2 <- Signal$new(name = "test static string signal two",
                                     short_name = "tsss2",
                                     measurement_name = "Dimensionless",
                                     storage_unit_name = " ",
                                     aggregation_type = "Sum",
                                     container_aggregation_type = "Sum",
                                     signal_type = "String",
                                     default_color = 3711337,
                                     default_line_type = "Dash",
                                     setting_name = NULL,
                                     labels = list(),
                                     description = "My description")

result <- sp$items$save("Signal", static_string_signal_2)

# Time numeric signals
time_numeric_signal_1 <- Signal$new(name = "test time numeric signal one",
                                    short_name = "ttns1",
                                    measurement_name = "Volume",
                                    storage_unit_name = "m3",
                                    aggregation_type = "Sum",
                                    container_aggregation_type = "Sum",
                                    signal_type = "TimeDependent",
                                    default_color = 3711337,
                                    default_line_type = "Dash",
                                    setting_name = NULL,
                                    labels = list(),
                                    description = "My description")

result <- sp$items$save("Signal", time_numeric_signal_1)

time_numeric_signal_2 <- Signal$new(name = "test time numeric signal two",
                                    short_name = "tts2",
                                    measurement_name = "Volume",
                                    storage_unit_name = "m3",
                                    aggregation_type = "Sum",
                                    container_aggregation_type = "Sum",
                                    signal_type = "TimeDependent",
                                    default_color = 3711337,
                                    default_line_type = "Dash",
                                    setting_name = NULL,
                                    labels = list(),
                                    description = "My description")

result <- sp$items$save("Signal", time_numeric_signal_2)

# Time string signals
time_string_signal_1 <- Signal$new(name = "test time string signal one",
                                   short_name = "ttss1",
                                   measurement_name = "Dimensionless",
                                   storage_unit_name = " ",
                                   aggregation_type = "Sum",
                                   container_aggregation_type = "Sum",
                                   signal_type = "StringTimeDependent",
                                   default_color = 3711337,
                                   default_line_type = "Dash",
                                   setting_name = NULL,
                                   labels = list(),
                                   description = "My description")

result <- sp$items$save("Signal", time_string_signal_1)

time_string_signal_2 <- Signal$new(name = "test time string signal two",
                                   short_name = "ttss2",
                                   measurement_name = "Dimensionless",
                                   storage_unit_name = " ",
                                   aggregation_type = "Sum",
                                   container_aggregation_type = "Sum",
                                   signal_type = "StringTimeDependent",
                                   default_color = 3711337,
                                   default_line_type = "Dash",
                                   setting_name = NULL,
                                   labels = list(),
                                   description = "My description")

result <- sp$items$save("Signal", time_string_signal_2)

# Depth numeric signals
depth_numeric_signal_1 <- Signal$new(name = "test depth numeric signal one",
                                     short_name = "tdns1",
                                     measurement_name = "Volume",
                                     storage_unit_name = "m3",
                                     aggregation_type = "Sum",
                                     container_aggregation_type = "Sum",
                                     signal_type = "DepthDependent",
                                     default_color = 3711337,
                                     default_line_type = "Dash",
                                     setting_name = NULL,
                                     labels = list(),
                                     description = "My description")

result <- sp$items$save("Signal", depth_numeric_signal_1)

depth_numeric_signal_2 <- Signal$new(name = "test depth numeric signal two",
                                     short_name = "tdns2",
                                     measurement_name = "Volume",
                                     storage_unit_name = "m3",
                                     aggregation_type = "Sum",
                                     container_aggregation_type = "Sum",
                                     signal_type = "DepthDependent",
                                     default_color = 3711337,
                                     default_line_type = "Dash",
                                     setting_name = NULL,
                                     labels = list(),
                                     description = "My description")

result <- sp$items$save("Signal", depth_numeric_signal_2)

# Depth string signals
depth_string_signal_1 <- Signal$new(name = "test depth string signal one",
                                    short_name = "tdss1",
                                    measurement_name = "Dimensionless",
                                    storage_unit_name = " ",
                                    aggregation_type = "Sum",
                                    container_aggregation_type = "Sum",
                                    signal_type = "StringDepthDependent",
                                    default_color = 3711337,
                                    default_line_type = "Dash",
                                    setting_name = NULL,
                                    labels = list(),
                                    description = "My description")

result <- sp$items$save("Signal", depth_string_signal_1)

depth_string_signal_2 <- Signal$new(name = "test depth string signal two",
                                    short_name = "tdss2",
                                    measurement_name = "Dimensionless",
                                    storage_unit_name = " ",
                                    aggregation_type = "Sum",
                                    container_aggregation_type = "Sum",
                                    signal_type = "StringDepthDependent",
                                    default_color = 3711337,
                                    default_line_type = "Dash",
                                    setting_name = NULL,
                                    labels = list(),
                                    description = "My description")

result <- sp$items$save("Signal", depth_string_signal_2)

# PVT numeric signals
pvt_numeric_signal_1 <- Signal$new(name = "test pvt numeric signal one",
                                   short_name = "tpns1",
                                   measurement_name = "MassPerVolume",
                                   storage_unit_name = "kg/m3",
                                   signal_type = "PVT",
                                   default_color = 3711337,
                                   default_line_type = "Dash",
                                   setting_name = NULL,
                                   labels = list(),
                                   description = "My description")

result <- sp$items$save("Signal", pvt_numeric_signal_1)

pvt_numeric_signal_2 <- Signal$new(name = "test pvt numeric signal two",
                                   short_name = "tpns2",
                                   measurement_name = "MassPerVolume",
                                   storage_unit_name = "kg/m3",
                                   signal_type = "PVT",
                                   default_color = 3711337,
                                   default_line_type = "Dash",
                                   setting_name = NULL,
                                   labels = list(),
                                   description = "My description")

result <- sp$items$save("Signal", pvt_numeric_signal_2)

# Create test data
# Static numeric data
static_numeric_data <- data.frame(
  c(""),
  c(entity$name),
  c(23.45),
  c(NaN)
)

colnames(static_numeric_data) <- c("scenario",
                                   "entity",
                                   static_numeric_signal_1$name,
                                   static_numeric_signal_2$name)

#Static string data
static_string_data <- data.frame(
  c(""),
  c(entity$name),
  c("test string 1"),
  c("test string 2")
)

colnames(static_string_data) <- c("scenario",
                                  "entity",
                                  static_string_signal_1$name,
                                  static_string_signal_2$name)
# Time numeric data
time_numeric_data <- data.frame(
  c("", ""),
  c("2025-01-01T00:00:00", "2025-01-02T00:00:00"),
  c(entity$name, entity$name),
  c(23.45, 45.56),
  c(78.89, NaN)
)

colnames(time_numeric_data) <- c("scenario",
                                 "date",
                                 "entity",
                                 time_numeric_signal_1$name,
                                 time_numeric_signal_2$name)

# Time string data
time_string_data <- data.frame(
  c("", ""),
  c("2025-01-01T00:00:00", "2025-01-02T00:00:00"),
  c(entity$name, entity$name),
  c("test string 1", "test string 2"),
  c("test string 3", "test string 4")
)

colnames(time_string_data) <- c("scenario",
                                "date",
                                "entity",
                                time_string_signal_1$name,
                                time_string_signal_2$name)

# Depth numeric data
depth_numeric_data <- data.frame(
  c("", ""),
  c(10, 11),
  c(entity$name, entity$name),
  c(23.45, 45.56),
  c(78.89, NaN)
)

colnames(depth_numeric_data) <- c("scenario",
                                  "depth",
                                  "entity",
                                  depth_numeric_signal_1$name,
                                  depth_numeric_signal_2$name)




# Depth string data
depth_string_data <- data.frame(
  c("", ""),
  c(10, 11),
  c(entity$name, entity$name),
  c("test string 1", "test string 2"),
  c("test string 3", "test string 4")
)

colnames(depth_string_data) <- c("scenario",
                                 "depth",
                                 "entity",
                                 depth_string_signal_1$name,
                                 depth_string_signal_2$name)

# PVT numeric data
pvt_numeric_data <- data.frame(
  c("", ""),
  c(100, 100), # Temperature
  c(150, 200), # Pressure
  c(entity$name, entity$name),
  c(23.45, 45.56),
  c(78.89, NaN)
)

colnames(pvt_numeric_data) <- c("scenario",
                                "temperature",
                                "pressure",
                                "entity",
                                pvt_numeric_signal_1$name,
                                pvt_numeric_signal_2$name)

# Create test reference table
key_column <- ReferenceTableColumn$new(name = "KeyColumn",
                                       column_type = "Numeric",
                                       unit_name = " ")

name_column <- ReferenceTableColumn$new(name = "PersonName",
                                        column_type = "String",
                                        unit_name = " ")

age_column <- ReferenceTableColumn$new(name = "PersonAge",
                                       column_type = "Numeric",
                                       unit_name = " ")

reference_table <- ReferenceTable$new(name = "Test R Table",
                                      description = "Test Description",
                                      key = key_column,
                                      values = list(name_column, age_column))

result <- sp$items$save("ReferenceTable", reference_table)

# Create test reference table data
reference_table_data <- data.frame(
  Entity = c(NA, "TestName", NA, "TestName"),
  Timestamp = c(NA, "2025-05-20T00:00:00", NA, NA),
  KeyColumn = c(1, 2, 3, 4),
  PersonName = c("Franz", "Herbert", "Josef", "Hans"),
  PersonAge = c(12, 34.4, 56.9, 20.3)
)

# Create test pivot table
pivot_table_definition <- PivotTable$new(
  name = "Test R PivotTable",
  add_entity_alias_column = TRUE,
  add_entity_type_column = TRUE,
  scope_formula = "Scope \"At Date\"\r\n\tBetween #01/01/2025#\r\n\tAnd #02/01/2025#\r\n\tStep Daily\r\nEnd Scope",
  entity_set_formula = "Entity Set \"Test\"\r\n\t\"TestName\"\r\nEnd Set",
  table_formula = "Table \"Test\"\r\n\tColumn \"Oil\" in \"m3\"\r\n\t\tRandom()\r\n\tEnd Column\r\nEnd Table",
  skip_empty_rows = TRUE,
  add_is_opportunity_column = TRUE,
  append_data = TRUE,
  description = "Test R Description")

result <- sp$items$save("PivotTable", pivot_table_definition)

# Perform tests
# Static numeric data tests
test_that("Static numeric data can be saved", {
  result <- sp$data$save_signals(
    "StaticNumeric",
    static_numeric_data,
    signals = lapply(
      c("test static numeric signal one [m]",
        "test static numeric signal two [m]"),
      function(x) {
        sp$parse_signal(x)
      }
    )
  )

  expect_equal(result$status_code, 200)
})

test_that("Static numeric data can be retrieved", {
  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test static numeric signal one [m]",
        "test static numeric signal two [m]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$StaticNumericData$scenario,
    static_numeric_data$scenario
  )
  expect_equal(
    retrieved_data$StaticNumericData$entity,
    static_numeric_data$entity
  )
  expect_equal(
    retrieved_data$StaticNumericData$`test static numeric signal one`,
    static_numeric_data$`test static numeric signal one`
  )
  expect_equal(
    retrieved_data$StaticNumericData$`test static numeric signal two`,
    static_numeric_data$`test static numeric signal two`
  )
})

test_that("Static numeric data can be retrieved (top_records)", {
  retrieved_data <- sp$data$load_signals(
    c(entity$name),
    lapply(
      c("test static numeric signal one [m]",
        "test static numeric signal two [m]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    top_records = 1,
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$StaticNumericData$scenario,
    static_numeric_data$scenario[1]
  )
  expect_equal(
    retrieved_data$StaticNumericData$entity,
    static_numeric_data$entity[1]
  )
  expect_equal(
    retrieved_data$StaticNumericData$`test static numeric signal one`,
    static_numeric_data$`test static numeric signal one`[1]
  )
  expect_equal(
    retrieved_data$StaticNumericData$`test static numeric signal two`,
    static_numeric_data$`test static numeric signal two`[1]
  )
})

test_that("Static numeric data can be deleted", {
  result <- sp$data$delete_signals(
    c(entity),
    c("test static numeric signal one", "test static numeric signal two")
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test static numeric signal one [m]",
        "test static numeric signal two [m]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$StaticNumericData$scenario,
    static_numeric_data$scenario
  )
  expect_equal(
    retrieved_data$StaticNumericData$entity,
    static_numeric_data$entity
  )
  expect_equal(
    retrieved_data$StaticNumericData$`test static numeric signal one`,
    c(NaN)
  )
  expect_equal(
    retrieved_data$StaticNumericData$`test static numeric signal two`,
    c(NaN)
  )
})

# Static string data tests
test_that("Static string data can be saved", {
  result <- sp$data$save_signals(
    "StaticString",
    static_string_data,
    signals = lapply(
      c("test static string signal one [ ]",
        "test static string signal two [ ]"),
      function(x) {
        sp$parse_signal(x)
      }
    )
  )

  expect_equal(result$status_code, 200)
})

test_that("Static string data can be retrieved", {
  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test static string signal one [ ]",
        "test static string signal two [ ]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$StaticStringData$scenario,
    static_string_data$scenario
  )
  expect_equal(
    retrieved_data$StaticStringData$entity,
    static_string_data$entity
  )
  expect_equal(
    retrieved_data$StaticStringData$`test static string signal one`,
    static_string_data$`test static string signal one`
  )
  expect_equal(
    retrieved_data$StaticStringData$`test static string signal two`,
    static_string_data$`test static string signal two`
  )
})

test_that("Static string data can be deleted", {
  result <- sp$data$delete_signals(
    c(entity),
    c("test static string signal one", "test static string signal two")
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test static string signal one [ ]",
        "test static string signal two [ ]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$StaticStringData$scenario,
    static_string_data$scenario
  )
  expect_equal(
    retrieved_data$StaticStringData$entity,
    static_string_data$entity
  )
  expect_equal(
    retrieved_data$StaticStringData$`test static string signal one`,
    c("")
  )
  expect_equal(
    retrieved_data$StaticStringData$`test static string signal two`,
    c("")
  )
})

# Time numeric data tests
test_that("Time numeric data can be saved", {
  result <- sp$data$save_signals(
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

  expect_equal(result$status_code, 200)
})

test_that("Time numeric data can be retrieved", {
  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test time numeric signal one [m3]",
        "test time numeric signal two [m3]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    time_increment = "Daily",
    time_start = "2025-01-01T00:00:00",
    time_end = "2025-01-02T00:00:00",
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$TimeNumericData$scenario,
    time_numeric_data$scenario
  )
  expect_equal(retrieved_data$TimeNumericData$entity, time_numeric_data$entity)
  expect_equal(retrieved_data$TimeNumericData$date, time_numeric_data$date)
  expect_equal(
    retrieved_data$TimeNumericData$`test time numeric signal one`,
    time_numeric_data$`test time numeric signal one`
  )
  expect_equal(
    retrieved_data$TimeNumericData$`test time numeric signal two`,
    time_numeric_data$`test time numeric signal two`
  )
})

test_that("Time numeric data can be deleted", {
  result <- sp$data$delete_signals(
    c(entity),
    c("test time numeric signal one", "test time numeric signal two"),
    time_start = "2025-01-01T00:00:00",
    time_end = "2025-01-02T00:00:00"
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test time numeric signal one [m3]",
        "test time numeric signal two [m3]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    time_increment = "Daily",
    time_start = "2025-01-01T00:00:00",
    time_end = "2025-01-02T00:00:00",
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$TimeNumericData$scenario,
    time_numeric_data$scenario
  )
  expect_equal(retrieved_data$TimeNumericData$entity, time_numeric_data$entity)
  expect_equal(retrieved_data$TimeNumericData$date, time_numeric_data$date)
  expect_equal(
    retrieved_data$TimeNumericData$`test time numeric signal one`,
    c(NaN, NaN)
  )
  expect_equal(
    retrieved_data$TimeNumericData$`test time numeric signal two`,
    c(NaN, NaN)
  )
})

# Time string data tests
test_that("Time string data can be saved", {
  result <- sp$data$save_signals(
    "TimeString",
    time_string_data,
    signals = lapply(
      c("test time string signal one [ ]",
        "test time string signal two [ ]"),
      function(x) {
        sp$parse_signal(x)
      }
    )
  )

  expect_equal(result$status_code, 200)
})

test_that("Time string data can be retrieved", {
  retrieved_data <- sp$data$load_signals(
    c(entity),
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

  expect_equal(
    retrieved_data$TimeStringData$scenario,
    time_string_data$scenario
  )
  expect_equal(retrieved_data$TimeStringData$entity, time_string_data$entity)
  expect_equal(retrieved_data$TimeStringData$date, time_string_data$date)
  expect_equal(
    retrieved_data$TimeStringData$`test time string signal one`,
    time_string_data$`test time string signal one`
  )
  expect_equal(
    retrieved_data$TimeStringData$`test time string signal two`,
    time_string_data$`test time string signal two`
  )
})

test_that("Time string data can be deleted", {
  result <- sp$data$delete_signals(
    c(entity),
    c("test time string signal one", "test time string signal two"),
    time_start = "2025-01-01T00:00:00",
    time_end = "2025-01-02T00:00:00"
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load_signals(
    c(entity),
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

  expect_equal(
    retrieved_data$TimeStringData$scenario,
    time_string_data$scenario
  )
  expect_equal(retrieved_data$TimeStringData$entity, time_string_data$entity)
  expect_equal(retrieved_data$TimeStringData$date, time_string_data$date)
  expect_equal(
    retrieved_data$TimeStringData$`test time string signal one`,
    c("", "")
  )
  expect_equal(
    retrieved_data$TimeStringData$`test time string signal two`,
    c("", "")
  )
})

# Depth numeric data tests
test_that("Depth numeric data can be saved", {
  result <- sp$data$save_signals(
    "DepthNumeric",
    depth_numeric_data,
    signals = lapply(
      c("test depth numeric signal one [m3]",
        "test depth numeric signal two [m3]"),
      function(x) {
        sp$parse_signal(x)
      }
    )
  )

  expect_equal(result$status_code, 200)
})

test_that("Depth numeric data can be retrieved", {
  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test depth numeric signal one [m3]",
        "test depth numeric signal two [m3]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    depth_increment = "Meter",
    depth_start = 10,
    depth_end = 11,
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$DepthNumericData$scenario,
    depth_numeric_data$scenario
  )
  expect_equal(
    retrieved_data$DepthNumericData$entity,
    depth_numeric_data$entity
  )
  expect_equal(
    retrieved_data$DepthNumericData$depth,
    depth_numeric_data$depth
  )
  expect_equal(
    retrieved_data$DepthNumericData$`test depth numeric signal one`,
    depth_numeric_data$`test depth numeric signal one`
  )
  expect_equal(
    retrieved_data$DepthNumericData$`test depth numeric signal two`,
    depth_numeric_data$`test depth numeric signal two`
  )
})

test_that("Depth numeric data can be deleted", {
  result <- sp$data$delete_signals(
    c(entity),
    c("test depth numeric signal one", "test depth numeric signal two"),
    depth_start = 0,
    depth_end = 100
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test depth numeric signal one [m3]",
        "test depth numeric signal two [m3]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    depth_increment = "Meter",
    depth_start = 10,
    depth_end = 11,
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$DepthNumericData$scenario,
    depth_numeric_data$scenario
  )
  expect_equal(
    retrieved_data$DepthNumericData$entity,
    depth_numeric_data$entity
  )
  expect_equal(retrieved_data$DepthNumericData$depth, depth_numeric_data$depth)
  expect_equal(
    retrieved_data$DepthNumericData$`test depth numeric signal one`,
    c(NaN, NaN)
  )
  expect_equal(
    retrieved_data$DepthNumericData$`test depth numeric signal two`,
    c(NaN, NaN)
  )
})

# Depth string data tests
test_that("Depth string data can be saved", {
  result <- sp$data$save_signals(
    "DepthString",
    depth_string_data,
    signals = lapply(
      c("test depth string signal one [ ]",
        "test depth string signal two [ ]"),
      function(x) {
        sp$parse_signal(x)
      }
    )
  )

  expect_equal(result$status_code, 200)
})

test_that("Depth string data can be retrieved", {
  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test depth string signal one [ ]",
        "test depth string signal two [ ]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    depth_increment = "Meter",
    depth_start = 10,
    depth_end = 11,
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$DepthStringData$scenario,
    depth_string_data$scenario
  )
  expect_equal(retrieved_data$DepthStringData$entity, depth_string_data$entity)
  expect_equal(retrieved_data$DepthStringData$depth, depth_string_data$depth)
  expect_equal(
    retrieved_data$DepthStringData$`test depth string signal one`,
    depth_string_data$`test depth string signal one`
  )
  expect_equal(
    retrieved_data$DepthStringData$`test depth string signal two`,
    depth_string_data$`test depth string signal two`
  )
})

test_that("Depth string data can be deleted", {
  result <- sp$data$delete_signals(
    c(entity),
    c("test depth string signal one", "test depth string signal two"),
    depth_start = 0,
    depth_end = 100
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test depth string signal one [ ]",
        "test depth string signal two [ ]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    depth_increment = "Meter",
    depth_start = 10,
    depth_end = 11,
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$DepthStringData$scenario,
    depth_string_data$scenario
  )
  expect_equal(retrieved_data$DepthStringData$entity, depth_string_data$entity)
  expect_equal(retrieved_data$DepthStringData$depth, depth_string_data$depth)
  expect_equal(
    retrieved_data$DepthStringData$`test depth string signal one`,
    c("", "")
  )
  expect_equal(
    retrieved_data$DepthStringData$`test depth string signal two`,
    c("", "")
  )
})

# PVT numeric data tests
test_that("PVT numeric data can be saved", {
  result <- sp$data$save_signals(
    "PVTNumeric",
    pvt_numeric_data,
    signals = lapply(
      c("test pvt numeric signal one [kg/m3]",
        "test pvt numeric signal two [kg/m3]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    pressure_unit = "psi",
    temperature_unit = "degC"
  )

  expect_equal(result$status_code, 200)
})

test_that("PVT numeric data can be retrieved", {
  retrieved_data <- sp$data$load_signals(
    c(entity),
    lapply(
      c("test pvt numeric signal one [kg/m3]",
        "test pvt numeric signal two [kg/m3]"),
      function(x) {
        sp$parse_signal(x)
      }
    ),
    pressure_unit = "psi",
    temperature_unit = "degC",
    reshape = TRUE
  )

  expect_equal(
    retrieved_data$PVTNumericData$scenario,
    pvt_numeric_data$scenario
  )
  expect_equal(retrieved_data$PVTNumericData$entity, pvt_numeric_data$entity)
  expect_equal(
    retrieved_data$PVTNumericData$pressure,
    pvt_numeric_data$pressure
  )
  expect_equal(
    retrieved_data$PVTNumericData$temperature,
    pvt_numeric_data$temperature
  )
  expect_equal(
    retrieved_data$PVTNumericData$`test pvt numeric signal one`,
    pvt_numeric_data$`test pvt numeric signal one`
  )
  expect_equal(
    retrieved_data$PVTNumericData$`test pvt numeric signal two`,
    pvt_numeric_data$`test pvt numeric signal two`
  )
})

test_that("PVT numeric data can be deleted", {
  result <- sp$data$delete_signals(
    c(entity),
    c("test pvt numeric signal one", "test pvt numeric signal two")
  )

  expect_equal(result$status_code, 200)

  # retrieved_data <- sp$data$load(
  #   c(entity),
  #   lapply(
  #     c("test pvt numeric signal one [kg/m3]",
  #       "test pvt numeric signal two [kg/m3]"),
  #     function(x) {
  #       sp$parse_signal(x)
  #     }
  #   ),
  #   reshape = TRUE
  # )
  #
  # expect_equal(
  #   retrieved_data$PVTNumericData$scenario,
  #   pvt_numeric_data$scenario
  # )
  # expect_equal(retrieved_data$PVTNumericData$entity, pvt_numeric_data$entity)
  # expect_equal(
  #   retrieved_data$PVTNumericData$pressure,
  #   pvt_numeric_data$pressure
  # )
  # expect_equal(
  #   retrieved_data$PVTNumericData$temperature,
  #   pvt_numeric_data$temperature
  # )
  # expect_equal(
  #   retrieved_data$PVTNumericData$`test pvt numeric signal one`,
  #   c(NaN, NaN)
  # )
  # expect_equal(
  #   retrieved_data$PVTNumericData$`test pvt numeric signal two`,
  #   c(NaN, NaN)
  # )
})

# Reference table data tests
test_that("Reference table data can be saved", {
  result <- sp$data$save_reference_table(reference_table$name,
                                         reference_table_data)

  expect_equal(result$status_code, 200)
})

test_that("Reference table data can be retrieved", {
  data <- sp$data$load_reference_table(reference_table$name)

  expect_equal(data, reference_table_data)
})

test_that("Reference table data can be deleted (with WHERE clause)", {
  result <- sp$data$delete_reference_table(reference_table$name,
                                           where = "[PersonAge] = 20.3")
  expect_equal(result$status_code, 200)

  data <- sp$data$load_reference_table(reference_table$name)

  expect_equal(data, reference_table_data[1:3, ])
})

test_that("Reference table data can be deleted (no WHERE clause)", {
  result <- sp$data$delete_reference_table(reference_table$name)
  expect_equal(result$status_code, 200)

  data <- sp$data$load_reference_table(reference_table$name)

  expect_equal(data, data.frame())
})

# Pivot table data tests
test_that("Pivot table data can be generated/saved", {
  expect_no_error(sp$data$save_pivot_table(pivot_table_definition$name))
})

test_that("Pivot table data can be retrieved", {
  expect_no_error(sp$data$load_pivot_table(pivot_table_definition$name))

  expect_no_error(sp$data$load_pivot_table(pivot_table_definition$name, 100))
})

test_that("Pivot table data can be deleted", {
  expect_no_error(sp$data$delete_pivot_table(pivot_table_definition$name))
})

# Clean up
# Remove test entity
sp$items$delete("Entity", entity$name)
# Remove test signals
sp$items$delete("Signal", static_numeric_signal_1$name)
sp$items$delete("Signal", static_numeric_signal_2$name)
sp$items$delete("Signal", static_string_signal_1$name)
sp$items$delete("Signal", static_string_signal_2$name)
sp$items$delete("Signal", time_numeric_signal_1$name)
sp$items$delete("Signal", time_numeric_signal_2$name)
sp$items$delete("Signal", time_string_signal_1$name)
sp$items$delete("Signal", time_string_signal_2$name)
sp$items$delete("Signal", depth_numeric_signal_1$name)
sp$items$delete("Signal", depth_numeric_signal_2$name)
sp$items$delete("Signal", depth_string_signal_1$name)
sp$items$delete("Signal", depth_string_signal_2$name)
sp$items$delete("Signal", pvt_numeric_signal_1$name)
sp$items$delete("Signal", pvt_numeric_signal_2$name)
sp$items$delete("ReferenceTable", reference_table$name)
sp$items$delete("PivotTable", pivot_table_definition$name)
