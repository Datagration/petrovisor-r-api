context("Unit Tests")

test_that("Unit instanciation and conversion works", {
  unit <- Unit$new(name = "MyUnit",
                   measurement_name = "Length",
                   factor = 1.5,
                   summand = 0)

  listed <- unit$toList()

  expect_equal(listed,
               list(Name = "MyUnit",
                    MeasurementName = "Length",
                    Factor = 1.5,
                    Summand = 0))
})

test_that("Unit instanciation and conversion works (empty constructor)", {
  unit <- Unit$new()

  listed <- unit$toList()

  expect_equal(listed,
               list(Name = "",
                    MeasurementName = "",
                    Factor = "",
                    Summand = ""))
})

test_that("Unit can be created", {
  unit <- Unit$new(name = "Test R Unit",
                   measurement_name = "Length",
                   factor = 1.5,
                   summand = 1)

  result <- sp$items$save("Unit", unit)

  expect_equal(result$status_code, 201)
})

test_that("Unit can be retrieved", {
  unit <- sp$items$load("Unit", "Test R Unit")

  expect_equal(unit, Unit$new(name = "Test R Unit",
                              measurement_name = "Length",
                              factor = 1.5,
                              summand = 1))
})

test_that("Unit can be deleted", {
  result <- sp$items$delete("Unit", "Test R Unit")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("Unit", "Test R Unit"))
})
