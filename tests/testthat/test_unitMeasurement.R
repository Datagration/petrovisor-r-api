context("UnitMeasurement Tests")

test_that("Unit measurement instanciation and conversion works", {
  um <- UnitMeasurement$new(name = "MyMeasurement",
                            canonical_unit_name = "MyCanonicalUnitName")

  listed <- um$toList()

  expect_equal(listed,
               list(Name = "MyMeasurement",
                    CanonicalUnitName = "MyCanonicalUnitName"))
})

test_that("Unit measurement instanciation and conversion works (empty constructor)", {
  um <- UnitMeasurement$new()

  listed <- um$toList()

  expect_equal(listed,
               list(Name = "",
                    CanonicalUnitName = ""))
})

test_that("UnitMeasurement can be retrieved", {
  um <- sp$items$load("UnitMeasurement", "Volume")

  expect_equal(um, UnitMeasurement$new(name = "Volume",
                                       canonical_unit_name = "m3"))
})
