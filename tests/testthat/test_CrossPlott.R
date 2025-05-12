##### CROSS PLOT #####
context("Cross plot instantiation and conversion to list")

test_that("Cross plot instantiation and conversion works",{
  ds1 <- CrossPlotDataSet$new(signalName = "oil rate",
                              unitName = "m3/d",
                              entityName = "Well01",
                              symbol = "circle",
                              color = "#123456",
                              min = 10,
                              max = 100,
                              trendLine = "On")
  ds2 <- CrossPlotDataSet$new(signalName = "gas rate",
                              unitName = "m3/d",
                              entityName = "Well01",
                              symbol = "circle",
                              color = "#123456",
                              min = 10,
                              max = 100,
                              trendLine = "Off")

  cp <- CrossPlot$new(name = "MyCrossPlot",
                      dataSets = list(ds1, ds2),
                      dataStart = "2020-02-01T00:00:00.000Z",
                      dataEnd = "2020-03-01T00:00:00.000Z",
                      dataStep = "Daily",
                      hierarchyName = "MyHierarchy",
                      mode = "Time",
                      xAxisSignalName = "oil rate",
                      xAxisUnitName = "m3/d",
                      xAxisEntityName = "Well01",
                      yAxisSignalName = "gas rate",
                      yAxisUnitName = "m3/d",
                      yAxisEntityName = "Well01",
                      skippedEntityNames = list("Well02", "Well03"),
                      timeMin = "2020-02-01T00:00:00.000Z",
                      timeMax = "2020-03-01T00:00:00.000Z",
                      isLocked = TRUE,
                      user = "MyUser",
                      isFavorite = TRUE,
                      labels = list("label1", "label2"))

  listed <- cp$toList()

  expect_equal(listed,
               list(Name = "MyCrossPlot",
                    DataSets = list(ds1$toList(), ds2$toList()),
                    DataStart = "2020-02-01T00:00:00.000Z",
                    DataEnd = "2020-03-01T00:00:00.000Z",
                    DataStep = "Daily",
                    HierarchyName = "MyHierarchy",
                    Mode = "Time",
                    XAxis = list(Signal = "oil rate",
                                 Unit = "m3/d",
                                 Entity = "Well01"),
                    YAxis = list(Signal = "gas rate",
                                 Unit = "m3/d",
                                 Entity = "Well01"),
                    SkippedEntities = list("Well02", "Well03"),
                    TimeMin = "2020-02-01T00:00:00.000Z",
                    TimeMax = "2020-03-01T00:00:00.000Z",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Cross plot instantiation and conversion works
          (empty constructor)",{
  cp <- CrossPlot$new()

  listed <- cp$toList()

  expect_equal(listed,
               list(Name = "",
                    DataSets = "",
                    DataStart = "",
                    DataEnd = "",
                    DataStep = "EveryMinute",
                    HierarchyName = "",
                    Mode = "",
                    XAxis = list(Signal = "",
                                 Unit = "",
                                 Entity = ""),
                    YAxis = list(Signal = "",
                                 Unit = "",
                                 Entity = ""),
                    SkippedEntities = "",
                    TimeMin = "",
                    TimeMax = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})

##### CROSS PLOT DATA SET #####
context("Cross plot data set instantiation and conversion to list")

test_that("Cross plot data set instantiation and conversion works",{
  cpds <- CrossPlotDataSet$new(signalName = "oil rate",
                               unitName = "m3/d",
                               entityName = "Well01",
                               symbol = "circle",
                               color = "#123456",
                               min = 10,
                               max = 100,
                               trendLine = "On")

  listed <- cpds$toList()

  expect_equal(listed,
               list(SignalName = "oil rate",
                    UnitName = "m3/d",
                    EntityName = "Well01",
                    Symbol = "circle",
                    Color = "#123456",
                    Min = 10,
                    Max = 100,
                    TrendLine = "On"))
})

test_that("Cross plot data set instantiation and conversion works
          (empty constructor)",{
  cpds <- CrossPlotDataSet$new()

  listed <- cpds$toList()

  expect_equal(listed,
               list(SignalName = "",
                    UnitName = "",
                    EntityName = "",
                    Symbol = "",
                    Color = "",
                    Min = "",
                    Max = "",
                    TrendLine = ""))
})
