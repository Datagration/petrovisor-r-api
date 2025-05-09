context("Scope Tests")

test_that("Scope instantiation and conversion works", {
  scp <- Scope$new(name = "Test R Scope",
                   start = "2020-03-01T00:00:00.000Z",
                   end = "2020-03-01T00:00:00.000Z",
                   time_increment = "Daily",
                   depth_increment = "Meter",
                   start_depth = 0,
                   end_depth = 5000,
                   formula = "Test Formula",
                   description = "Test description",
                   labels = list("label1", "label2"))

  listed <- scp$toList()

  expect_equal(listed,
               list(Name = "Test R Scope",
                    Start = "2020-03-01T00:00:00.000Z",
                    End = "2020-03-01T00:00:00.000Z",
                    TimeIncrement = "Daily",
                    DepthIncrement = "Meter",
                    StartDepth = 0,
                    EndDepth = 5000,
                    Formula = "Test Formula",
                    Description = "Test description",
                    Labels = list("label1", "label2")))
})

test_that("Scope instantiation and conversion works (empty constructor)", {
  scp <- Scope$new()

  listed <- scp$toList()

  expect_equal(listed,
               list(Name = "",
                    Start = "",
                    End = "",
                    TimeIncrement = "",
                    DepthIncrement = "",
                    StartDepth = "",
                    EndDepth = "",
                    Formula = "",
                    Description = "",
                    Labels = list()))
})

test_that("Scope can be created", {
  scope <- Scope$new(name = "Test R Scope",
                     start = "2020-03-01T00:00:00.000Z",
                     end = "2020-03-01T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 10,
                     end_depth = 1500,
                     description = "Test R Scope")

  result <- sp$items$save("Scope", scope)

  expect_equal(result$status_code, 201)
})

test_that("Scope can be retrieved", {
  scope <- sp$items$load("Scope", "Test R Scope")

  expect_equal(scope, Scope$new(name = "Test R Scope",
                                start = "2020-03-01T00:00:00",
                                end = "2020-03-01T00:00:00",
                                time_increment = "Daily",
                                depth_increment = "Meter",
                                start_depth = 10,
                                end_depth = 1500,
                                formula = "Scope \"Test R Scope\" \n\tBetween #03/01/2020 00:00# \n\tAnd #03/01/2020 00:00# \n\tStep Daily \n\tFrom 10 \n\tTo 1500 \n\tDepth Step Meter \nEnd Scope",
                                description = "Test R Scope"))
})

test_that("Scope can be deleted", {
  result <- sp$items$delete("Scope", "Test R Scope")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("Scope", "Test R Scope"))
})
