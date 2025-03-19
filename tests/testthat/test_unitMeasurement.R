##### UNIT MEASUREMENT #####
context("Unit measurement instanciation and conversion to list")

test_that("Unit measurement instanciation and conversion works",{
  um <- UnitMeasurement$new(name = "MyMeasurement",
                            canonicalUnitName = "MyCanonicalUnitName")

  listed <- um$toList()

  expect_equal(listed,
               list(Name = "MyMeasurement",
                    CanonicalUnitName = "MyCanonicalUnitName"))
})

test_that("Unit measurement instanciation and conversion works (empty constructor)",{
  um <- UnitMeasurement$new()

  listed <- um$toList()

  expect_equal(listed,
               list(Name = "",
                    CanonicalUnitName = ""))
})
