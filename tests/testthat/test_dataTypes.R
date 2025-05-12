##### STATIC DATA #####
context("Static data instantiation and conversion to list")

test_that("Static data instantiation and conversion works", {
  ds <- StaticData$new(signal_name = "x-coordinate",
                       unit_name = "m",
                       entity_name = "Well01",
                       data = 10,
                       scenario = NULL)

  listed <- ds$to_list()

  expect_equal(listed,
               list(Signal = "x-coordinate",
                    Entity = "Well01",
                    Unit = "m",
                    Data = 10,
                    Scenario = ""))
})

test_that("Static data instantiation and conversion works (empty constructor)", {
  ds <- StaticData$new()

  listed <- ds$to_list()

  expect_equal(listed,
               list(Signal = "",
                    Entity = "",
                    Unit = "",
                    Data = NULL,
                    Scenario = ""))
})

##### TIME DATA #####
context("Time data instantiation and conversion to list")

test_that("Time data instantiation and conversion works", {
  dp1 <- list(Date = "2020-01-01T00:00:00.000Z",
              Value = 10)
  dp2 <- list(Date = "2020-02-01T00:00:00.000Z",
              Value = 20)

  dt <- TimeData$new(signal_name = "oil rate",
                     unit_name = "m3/d",
                     entity_name = "Well01",
                     data = list(dp1, dp2),
                     scenario = NULL)

  listed <- dt$to_list()

  expect_equal(listed,
               list(Signal = "oil rate",
                    Entity = "Well01",
                    Unit = "m3/d",
                    Data = list(dp1, dp2),
                    Scenario = ""))
})

test_that("Time data instantiation and conversion works (empty constructor)", {
  dt <- TimeData$new()

  listed <- dt$to_list()

  expect_equal(listed,
               list(Signal = "",
                    Entity = "",
                    Unit = "",
                    Data = list(),
                    Scenario = ""))
})

##### DEPTH DATA #####
context("Depth data instantiation and conversion to list")

test_that("Depth data instantiation and conversion works", {
  dp1 <- list(Depth = 100,
              Value = 10)
  dp2 <- list(Depth = 200,
              Value = 20)

  dt <- DepthData$new(signal_name = "oil rate",
                      unit_name = "m3/d",
                      entity_name = "Well01",
                      data = list(dp1, dp2),
                      scenario = NULL)

  listed <- dt$to_list()

  expect_equal(listed,
               list(Signal = "oil rate",
                    Entity = "Well01",
                    Unit = "m3/d",
                    Data = list(dp1, dp2),
                    Scenario = ""))
})

test_that("Depth data instantiation and conversion works (empty constructor)", {
  dt <- DepthData$new()

  listed <- dt$to_list()

  expect_equal(listed,
               list(Signal = "",
                    Entity = "",
                    Unit = "",
                    Data = list(),
                    Scenario = ""))
})

##### PVT DATA #####
context("PVT data instantiation and conversion to list")

test_that("PVT data instantiation and conversion works", {
  p1 <- list(Pressure = 150,
             Temperature = 100,
             Value = 10)
  p2 <- list(Pressure = 200,
             Temperature = 100,
             Value = 20)

  dpvt <- PVTData$new(signal_name = "oil density pvt",
                      unit_name = "kg/m3",
                      entity_name = "Well01",
                      data = list(p1, p2),
                      scenario = NULL)

  listed <- dpvt$to_list()

  expect_equal(listed,
               list(Signal = "oil density pvt",
                    Entity = "Well01",
                    Unit = "kg/m3",
                    Data = list(p1, p2),
                    Scenario = ""))
})

test_that("PVT data instantiation and conversion works (empty constructor)", {
  dpvt <- PVTData$new()

  listed <- dpvt$to_list()

  expect_equal(listed,
               list(Signal = "",
                    Entity = "",
                    Unit = "",
                    Data = list(),
                    Scenario = ""))
})

##### DATA POINT #####
context("Data point instantiation and conversion to list")

test_that("Data point instantiation and conversion works", {
  dp <- DataPoint$new(date = "2020-03-01T00:00:00.000Z",
                      value = 10)

  listed <- dp$to_list()

  expect_equal(listed,
               list(Date = "2020-03-01T00:00:00.000Z",
                    Value = 10))
})

test_that("Data point instantiation and conversion works (empty constructor)", {
  dp <- DataPoint$new()

  listed <- dp$to_list()

  expect_equal(listed,
               list(Date = "",
                    Value = NULL))
})

##### POINT #####
context("Point instantiation and conversion to list")

test_that("Point instantiation and conversion works", {
  p <- Point$new(x = 20,
                 y = 10)

  listed <- p$toList()

  expect_equal(listed,
               list(X = 20,
                    Y = 10))
})

test_that("Point instantiation and conversion works (empty constructor)", {
  p <- Point$new()

  listed <- p$toList()

  expect_equal(listed,
               list(X = "",
                    Y = ""))
})

##### NAMED POINT #####
context("Named point instantiation and conversion to list")

test_that("Named point instantiation and conversion works", {
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

test_that("Named point instantiation and conversion works (empty constructor)", {
  np <- NamedPoint$new()

  listed <- np$toList()

  expect_equal(listed,
               list(Name = "",
                    Tag = "",
                    X = "",
                    Y = ""))
})
