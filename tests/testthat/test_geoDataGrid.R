##### GEO DATA GRID #####
context("Geo data grid instanciation and conversion to list")

test_that("Geo data grid instanciation and conversion works",{
  geoGrid <- GeoDataGrid$new(name = "MyGrid",
                             gridData = list(1,2,3,4,5,6,7,8),
                             rows = 4,
                             columns = 2,
                             minX = 1,
                             minY = 1,
                             maxX = 1,
                             maxY = 1,
                             description = "MyDescription",
                             isLocked = TRUE,
                             user = "MyUser",
                             isFavorite = TRUE,
                             labels = list("label1", "label2"))

  listed <- geoGrid$toList()

  expect_equal(listed,
               list(Name = "MyGrid",
                    GridData = list(1,2,3,4,5,6,7,8),
                    Rows = 4,
                    Columns = 2,
                    Min = list(X = 1,
                               Y = 1),
                    Max = list(X = 1,
                               Y = 1),
                    Description = "MyDescription",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Geo data grid instanciation and conversion works (empty constructor)",{
  geoGrid <- GeoDataGrid$new()

  listed <- geoGrid$toList()

  expect_equal(listed,
               list(Name = "",
                    GridData = "",
                    Rows = "",
                    Columns = "",
                    Min = list(X = "", Y = ""),
                    Max = list(X = "", Y = ""),
                    Description = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
