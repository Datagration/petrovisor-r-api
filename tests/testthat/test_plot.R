##### PLOT #####
context("plot instantiation and conversion to list")

test_that("plot instantiation and conversion works",{
  p <- Plot$new(name = "MyPlot",
                plotType = "scatter",
                inputTableNames = list("Table1", "Table2"),
                xAxisColumnName = "x-coordinate",
                xAxisUnitName = "m",
                yAxisColumnName = "y-coordinate",
                yAxisUnitName = "m",
                ryAxisColumnName = "surface y-coordinate",
                ryAxisUnitName = "m",
                sizeColumnName = "oil rate",
                sizeUnitName = "m3/d",
                formula = "MyFormula",
                isLocked = TRUE,
                user = "MyUser",
                isFavorite = TRUE,
                labels = list("label1", "label2"))

  listed <- p$toList()

  expect_equal(listed,
               list(Name = "MyPlot",
                    PlotType = "scatter",
                    InputTableNames = list("Table1", "Table2"),
                    XAxis = list(ColumnName = "x-coordinate",
                                 UnitName = "m"),
                    YAxis = list(ColumnName = "y-coordinate",
                                 UnitName = "m"),
                    RYAxis = list(ColumnName = "surface y-coordinate",
                                  UnitName = "m"),
                    Size = list(ColumnName = "oil rate",
                                UnitName = "m3/d"),
                    Formula = "MyFormula",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Plot instantiation and conversion works (empty constructor)",{
  p <- Plot$new()

  listed <- p$toList()

  expect_equal(listed,
               list(Name = "",
                    PlotType = "",
                    InputTableNames = "",
                    XAxis = list(ColumnName = "",
                                 UnitName = ""),
                    YAxis = list(ColumnName = "",
                                 UnitName = ""),
                    RYAxis = list(ColumnName = "",
                                  UnitName = ""),
                    Size = list(ColumnName = "",
                                UnitName = ""),
                    Formula = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
