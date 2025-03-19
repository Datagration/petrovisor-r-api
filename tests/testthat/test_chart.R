context("Chart instanciation and conversion to list")

##### CHART #####
test_that("Chart instanciation and conversion works",{
  axis1 <- Axis$new(title = "Fruits eaten",
                   isOpposed = FALSE,
                   isLog = FALSE,
                   axisId = "fruits",
                   manualMin = 0,
                   manualMax = 100,
                   isInversed = FALSE,
                   ticksPerInterval = 10)

  axis2 <- Axis$new(title = "Fruits harvested",
                    isOpposed = TRUE,
                    isLog = FALSE,
                    axisId = "fruitsHarvested",
                    manualMin = 0,
                    manualMax = 100,
                    isInversed = FALSE,
                    ticksPerInterval = 20)

  setting1 <- SeriesSetting$new(scatterSize = 2,
                               lineThickness = 3,
                               color = "#123456")

  setting2 <- SeriesSetting$new(scatterSize = 4,
                                lineThickness = 6,
                                color = "#987654")

  chart <- Chart$new(name = "MyChart",
                     dataType = "MyDataType",
                     legendLocation = "top",
                     isLegendVisible = TRUE,
                     isLegendOrientationHorizontal = TRUE,
                     isLegendPlacementInside = FALSE,
                     chartType = "scatter",
                     isStacked = TRUE,
                     isStacked100 = TRUE,
                     isGridLinesOnXAxis = FALSE,
                     isGridLinesOnYAxis = TRUE,
                     dataAxesMappings = list("mapping1", "mapping2"),
                     seriesSettings = list(setting1, setting2),
                     axes = list(axis1, axis2),
                     showToolTipTitle = TRUE,
                     showToolTipX = TRUE,
                     showToolTipY = TRUE,
                     isLocked = FALSE,
                     user = "MyUser",
                     isFavorite = FALSE,
                     labels = list("label1", "label2"))
  listed <- chart$toList()

  expect_equal(listed, list(Name = "MyChart",
                          DataType = "MyDataType",
                          LegendLocation = "top",
                          IsLegendVisible = TRUE,
                          IsLegendOrientationHorizontal = TRUE,
                          IsLegendPlacementInside = FALSE,
                          ChartType = "scatter",
                          IsStacked = TRUE,
                          IsStacked100 = TRUE,
                          IsGridLinesOnXAxis = FALSE,
                          IsGridLinesOnYAxis = TRUE,
                          DataAxesMappings = list("mapping1", "mapping2"),
                          SeriesSettings = list(setting1$toList(),
                                                setting2$toList()),
                          Axes = list(axis1$toList(),
                                      axis2$toList()),
                          ShowToolTipTitle = TRUE,
                          ShowToolTipX = TRUE,
                          ShowToolTipY = TRUE,
                          IsLocked = FALSE,
                          User = "MyUser",
                          IsFavorite = FALSE,
                          Labels = list("label1", "label2")))
})

test_that("Chart instanciation and conversion works (empty constructor)",{
  myChart <- Chart$new()
  listed <- myChart$toList()

  expect_equal(listed, list(Name = "",
                          DataType = "",
                          LegendLocation = "",
                          IsLegendVisible = "",
                          IsLegendOrientationHorizontal = "",
                          IsLegendPlacementInside = "",
                          ChartType = "",
                          IsStacked = "",
                          IsStacked100 = "",
                          IsGridLinesOnXAxis = "",
                          IsGridLinesOnYAxis = "",
                          DataAxesMappings = "",
                          SeriesSettings = "",
                          Axes = "",
                          ShowToolTipTitle = "",
                          ShowToolTipX = "",
                          ShowToolTipY = "",
                          IsLocked = FALSE,
                          User = "",
                          IsFavorite = FALSE,
                          Labels = ""))
})

##### AXIS #####
test_that("Axis instanciation and conversion works",{
  axis <- Axis$new(title = "Fruits eaten",
                   isOpposed = FALSE,
                   isLog = FALSE,
                   axisId = "fruits",
                   manualMin = 0,
                   manualMax = 100,
                   isInversed = FALSE,
                   ticksPerInterval = 10)
  axisList <- axis$toList()

  expect_equal(axisList, list(Title = "Fruits eaten",
                              IsOpposed = FALSE,
                              IsLog = FALSE,
                              AxisId = "fruits",
                              ManualMin = 0,
                              ManualMax = 100,
                              IsInversed = FALSE,
                              TicksPerInterval = 10))
})

test_that("Axis instanciation and conversion works (empty constructor)",{
  axis <- Axis$new()
  axisList <- axis$toList()

  expect_equal(axisList, list(Title = "",
                              IsOpposed = "",
                              IsLog = "",
                              AxisId = "",
                              ManualMin = "",
                              ManualMax = "",
                              IsInversed = "",
                              TicksPerInterval = ""))
})

##### SERIES SETTING #####
test_that("SeriesSetting instanciation and conversion works",{
  setting <- SeriesSetting$new(scatterSize = 2,
                           lineThickness = 3,
                           color = "#123456")
  settingList <- setting$toList()

  expect_equal(settingList, list(ScatterSize = 2,
                                 LineThickness = 3,
                                 Color = "#123456"))
})

test_that("SeriesSetting instanciation and conversion works
          (empty constructor)",{
  setting <- SeriesSetting$new()
  settingList <- setting$toList()

  expect_equal(settingList, list(ScatterSize = "",
                                 LineThickness = "",
                                 Color = ""))
})
