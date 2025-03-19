##### UNIT #####
context("Unit instanciation and conversion to list")

test_that("Unit instanciation and conversion works",{
  unit <- Unit$new(name = "MyUnit",
                   measurementName = "Length",
                   factor = 1.5,
                   summand = 0)

  listed <- unit$toList()

  expect_equal(listed,
               list(Name = "MyUnit",
                    MeasurementName = "Length",
                    Factor = 1.5,
                    Summand = 0))
})

test_that("Unit instanciation and conversion works (empty constructor)",{
  unit <- Unit$new()

  listed <- unit$toList()

  expect_equal(listed,
               list(Name = "",
                    MeasurementName = "",
                    Factor = "",
                    Summand = ""))
})
