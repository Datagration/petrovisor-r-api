##### DATA SET REQUEST #####
context("Data set request instanciation and conversion to list")

test_that("Data set request instanciation and conversion works",{
  dsr <- DataSetRequest$new(entityName = "Well01",
                            signalName = "oil rate",
                            unitName = "m3/d")

  listed <- dsr$toList()

  expect_equal(listed,
               list(Entity = "Well01",
                    Signal = "oil rate",
                    Unit = "m3/d"))
})

test_that("Data set request instanciation and conversion works
          (empty constructor)",{
  dsr <- DataSetRequest$new()

  listed <- dsr$toList()

  expect_equal(listed,
               list(Entity = "",
                    Signal = "",
                    Unit = ""))
})
