##### STATIC DATA #####
context("Static data instanciation and conversion to list")

test_that("Static data instanciation and conversion works",{
  ds <- StaticData$new(signalName = "x-coordinate",
                       unitName = "m",
                       entityName = "Well01",
                       data = 10)

  listed <- ds$toList()

  expect_equal(listed,
               list(Signal = "x-coordinate",
                    Entity = "Well01",
                    Unit = "m",
                    Data = 10))
})

test_that("Static data instanciation and conversion works (empty constructor)",{
  ds <- StaticData$new()

  listed <- ds$toList()

  expect_equal(listed,
               list(Signal = "",
                    Entity = "",
                    Unit = "",
                    Data = ""))
})

##### TIME DATA #####
context("Time data instanciation and conversion to list")

test_that("Time data instanciation and conversion works",{
  dp1 <- DataPoint$new(date = "2020-01-01T00:00:00.000Z",
                       value = 10)
  dp2 <- DataPoint$new(date = "2020-02-01T00:00:00.000Z",
                       value = 20)

  dt <- TimeData$new(signalName = "oil rate",
                     unitName = "m3/d",
                     entityName = "Well01",
                     data = list(dp1, dp2))

  listed <- dt$toList()

  expect_equal(listed,
               list(Signal = "oil rate",
                    Entity = "Well01",
                    Unit = "m3/d",
                    Data = list(dp1$toList(), dp2$toList())))
})

test_that("Time data instanciation and conversion works (empty constructor)",{
  dt <- TimeData$new()

  listed <- dt$toList()

  expect_equal(listed,
               list(Signal = "",
                    Entity = "",
                    Unit = "",
                    Data = ""))
})

##### DATA POINT #####
context("Data point instanciation and conversion to list")

test_that("Data point instanciation and conversion works",{
  dp <- DataPoint$new(date = "2020-03-01T00:00:00.000Z",
                      value = 10)

  listed <- dp$toList()

  expect_equal(listed,
               list(Date = "2020-03-01T00:00:00.000Z",
                    Value = 10))
})

test_that("Data point instanciation and conversion works (empty constructor)",{
  dp <- DataPoint$new()

  listed <- dp$toList()

  expect_equal(listed,
               list(Date = "",
                    Value = ""))
})

##### POINT #####
context("Point instanciation and conversion to list")

test_that("Point instanciation and conversion works",{
  p <- Point$new(x = 20,
                 y = 10)

  listed <- p$toList()

  expect_equal(listed,
               list(X = 20,
                    Y = 10))
})

test_that("Point instanciation and conversion works (empty constructor)",{
  p <- Point$new()

  listed <- p$toList()

  expect_equal(listed,
               list(X = "",
                    Y = ""))
})

##### NAMED POINT #####
context("Named point instanciation and conversion to list")

test_that("Named point instanciation and conversion works",{
  np <- NamedPoint$new(name = "MyPoint",
                       tagName = "MyTagName",
                       x = 20,
                       y = 10)

  listed <- np$toList()

  expect_equal(listed,
               list(Name = "MyPoint",
                    Tag = "MyTagName",
                    X = 20,
                    Y = 10))
})

test_that("Named point instanciation and conversion works
          (empty constructor)",{
  np <- NamedPoint$new()

  listed <- np$toList()

  expect_equal(listed,
               list(Name = "",
                    Tag = "",
                    X = "",
                    Y = ""))
})
