context("ServiceProvider Tests")

test_that("parse_signal works", {
  parsed <- sp$parse_signal("test signal [m3]")

  expect_equal(parsed, list(Signal = "test signal", Unit = "m3"))
})

test_that("convert_unit works (single values)", {
  expect_equal(sp$convert_unit(5.1, "km", "m"), 5100)
  expect_equal(sp$convert_unit(10, "%", " "), 0.1)
  expect_equal(sp$convert_unit(0.1, " ", "%"), 10)
  expect_equal(sp$convert_unit(NA, "km", "m"), NA)
  expect_equal(sp$convert_unit(NaN, "km", "m"), NaN)
  expect_equal(sp$convert_unit(NULL, "%", " "), NULL)
  expect_error(sp$convert_unit("A", "%", " "))
})

test_that("conver_unit works (multiple values)", {
  data <- data.frame(
    c("", ""),
    c("2025-01-01T00:00:00", "2025-01-02T00:00:00"),
    c("Test", "Test"),
    c(23.45, NA),
    c(78.89, NaN)
  )

  colnames(data) <- c("scenario",
                      "date",
                      "entity",
                      "Signal1",
                      "Signal2")


  expect_equal(
    sp$convert_unit(data$Signal1, "km", "m"),
    c(23450, NaN)
  )
  expect_equal(
    sp$convert_unit(data$Signal2, "%", " "),
    c(0.7889, NaN)
  )

  data <- list(12.5, 7, 3.1415, NA, NaN)
  expect_equal(
    sp$convert_unit(data, "km", "m"),
    c(12500, 7000, 3141.5, NaN, NaN)
  )

  data <- c("Test", 23.3, NA)
  expect_error(sp$convert_unit(data, " ", "%"))
})
