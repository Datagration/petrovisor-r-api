##### DATA SET REQUEST #####
# NOTE: DataSetRequest is deprecated as of v3.6.0
# These tests are retained for backward compatibility verification only

context("Data set request instantiation and conversion to list")

test_that("Data set request instantiation and conversion works", {
  # Suppress deprecation warnings for testing deprecated functionality
  suppressWarnings({
    dsr <- DataSetRequest$new(entityName = "Well01",
                              signalName = "oil rate",
                              unitName = "m3/d")
    listed <- dsr$toList()
  })

  expect_equal(listed,
               list(Entity = "Well01",
                    Signal = "oil rate",
                    Unit = "m3/d"))
})

test_that("Data set request instantiation and conversion works
          (empty constructor)", {
  # Suppress deprecation warnings for testing deprecated functionality
  suppressWarnings({
    dsr <- DataSetRequest$new()
    listed <- dsr$toList()
  })

  expect_equal(listed,
               list(Entity = "",
                    Signal = "",
                    Unit = ""))
})
