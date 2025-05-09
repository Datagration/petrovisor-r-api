context("Signal Tests")

test_that("Signal instantiation and conversion works", {
  signal <- Signal$new(name = "MySignal",
                       short_name = "MyShorName",
                       measurement_name = "Length",
                       storage_unit_name = "m",
                       aggregation_type = "Average",
                       container_aggregation_type = "Sum",
                       signal_type = "TimeDependent",
                       default_color = 0,
                       default_line_type = "Dash",
                       setting_name = "MySetting",
                       labels = list("label1", "label2"),
                       description = "My description")

  listed <- signal$toList()

  expect_equal(listed,
               list(Name = "MySignal",
                    ShortName = "MyShorName",
                    MeasurementName = "Length",
                    StorageUnitName = "m",
                    AggregationType = "Average",
                    ContainerAggregationType = "Sum",
                    SignalType = "TimeDependent",
                    DefaultColor = 0,
                    DefaultLineType = "Dash",
                    SettingName = "MySetting",
                    Description = "My description",
                    Labels = list("label1", "label2")))
})

test_that("Signal instantiation and conversion works (empty constructor)", {
  signal <- Signal$new()

  listed <- signal$toList()

  expect_equal(listed,
               list(Name = "",
                    ShortName = "",
                    MeasurementName = "",
                    StorageUnitName = "",
                    AggregationType = "Sum",
                    ContainerAggregationType = "Sum",
                    SignalType = "Static",
                    DefaultColor = 0,
                    DefaultLineType = "Solid",
                    SettingName = "",
                    Description = "",
                    Labels = list()))
})

test_that("Signal can be created", {
  signal <- Signal$new(name = "test r signal",
                       short_name = "trs",
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

  result <- sp$items$save("Signal", signal)

  expect_equal(result$status_code, 201)
})

test_that("Signal can be retrieved", {
  signal <- sp$items$load("Signal", "test r signal")

  expect_equal(signal, Signal$new(name = "test r signal",
                                  short_name = "trs",
                                  measurement_name = "Volume",
                                  storage_unit_name = "m3",
                                  aggregation_type = "Sum",
                                  container_aggregation_type = "Sum",
                                  signal_type = "TimeDependent",
                                  default_color = 3711337,
                                  default_line_type = "Dash",
                                  setting_name = NULL,
                                  labels = list(),
                                  description = "My description"))
})

test_that("Signal can be deleted", {
  result <- sp$items$delete("Signal", "test r signal")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("Signal", "test r signal"))
})
