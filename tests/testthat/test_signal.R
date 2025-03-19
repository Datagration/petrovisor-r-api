##### SIGNAL #####
context("Signal instanciation and conversion to list")

test_that("Signal instanciation and conversion works",{
  signal <- Signal$new(name = "MySignal",
                       shortName = "MyShorName",
                       measurementName = "Length",
                       storageUnitName = "m",
                       aggregationType = "Average",
                       containerAggregationType = "Sum",
                       signalType = "Time-dependent",
                       defaultColor = 0,
                       defaultLineType = "Dash",
                       settingName = "MySetting")

  listed <- signal$toList()

  expect_equal(listed,
               list(Name = "MySignal",
                    ShortName = "MyShorName",
                    MeasurementName = "Length",
                    StorageUnitName = "m",
                    AggregationType = "Average",
                    ContainerAggregationType = "Sum",
                    SignalType = "Time-dependent",
                    DefaultColor = 0,
                    DefaultLineType = "Dash",
                    SettingName = "MySetting"))
})

test_that("Signal instanciation and conversion works (empty constructor)",{
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
                    SettingName = ""))
})
