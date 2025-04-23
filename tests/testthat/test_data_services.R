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

# Perform tests
# Static numeric data tests
test_that("Static numeric data can be saved", {
  result <- sp$data$save(
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
  retrieved_data <- sp$data$load(
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

test_that("Static numeric data can be deleted", {
  result <- sp$data$delete(
    c(entity),
    c("test static numeric signal one", "test static numeric signal two")
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load(
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
  result <- sp$data$save(
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
  retrieved_data <- sp$data$load(
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
  result <- sp$data$delete(
    c(entity),
    c("test static string signal one", "test static string signal two")
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load(
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

  expect_equal(result$status_code, 200)
})

test_that("Time numeric data can be retrieved", {
  retrieved_data <- sp$data$load(
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
  result <- sp$data$delete(
    c(entity),
    c("test time numeric signal one", "test time numeric signal two"),
    time_start = "2025-01-01T00:00:00",
    time_end = "2025-01-02T00:00:00"
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load(
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
  result <- sp$data$save(
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
  retrieved_data <- sp$data$load(
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
  result <- sp$data$delete(
    c(entity),
    c("test time string signal one", "test time string signal two"),
    time_start = "2025-01-01T00:00:00",
    time_end = "2025-01-02T00:00:00"
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load(
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
  result <- sp$data$save(
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
  retrieved_data <- sp$data$load(
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
  result <- sp$data$delete(
    c(entity),
    c("test depth numeric signal one", "test depth numeric signal two"),
    depth_start = 0,
    depth_end = 100
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load(
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
  result <- sp$data$save(
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
  retrieved_data <- sp$data$load(
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
  result <- sp$data$delete(
    c(entity),
    c("test depth string signal one", "test depth string signal two"),
    depth_start = 0,
    depth_end = 100
  )

  expect_equal(result$status_code, 200)

  retrieved_data <- sp$data$load(
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
  result <- sp$data$save(
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
  retrieved_data <- sp$data$load(
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
  result <- sp$data$delete(
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
