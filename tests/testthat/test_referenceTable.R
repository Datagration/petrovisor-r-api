context("ReferenceTable Tests")

test_that("ReferenceTable instantiation and conversion works", {
  key <- ReferenceTableColumn$new(name = "TestKey",
                                  column_type = "Numeric",
                                  unit_name = "m3")

  value_one <- ReferenceTableColumn$new(name = "Value One",
                                        column_type = "Numeric",
                                        unit_name = "m3")

  value_two <- ReferenceTableColumn$new(name = "Value Two",
                                        column_type = "Bool",
                                        unit_name = " ")

  reference_table <- ReferenceTable$new(name = "Test R ReferenceTable",
                                        description = "Test Description",
                                        labels = list("A", "B"),
                                        key = key,
                                        values = list(value_one, value_two))

  listed <- reference_table$toList()

  expect_equal(listed,
               list(Name = "Test R ReferenceTable",
                    Description = "Test Description",
                    Labels = list("A", "B"),
                    Key = key$toList(),
                    Values = list(value_one$toList(), value_two$toList())))
})

test_that("ReferenceTable instantiation and conversion works (empty constructor)", {
  reference_table <- ReferenceTable$new()

  listed <- reference_table$toList()

  expect_equal(listed,
               list(Name = "",
                    Description = "",
                    Labels = list(),
                    Key = "",
                    Values = list()))
})

test_that("ReferenceTable can be created", {
  key <- ReferenceTableColumn$new(name = "TestKey",
                                  column_type = "Numeric",
                                  unit_name = "m3")

  value_one <- ReferenceTableColumn$new(name = "Value One",
                                        column_type = "Numeric",
                                        unit_name = "m3")

  value_two <- ReferenceTableColumn$new(name = "Value Two",
                                        column_type = "Bool",
                                        unit_name = " ")

  reference_table <- ReferenceTable$new(name = "Test R ReferenceTable",
                                        description = "Test Description",
                                        key = key,
                                        values = list(value_one, value_two))

  result <- sp$items$save("ReferenceTable", reference_table)

  expect_equal(result$status_code, 201)
})

test_that("ReferenceTable can be retrieved", {
  reference_table <- sp$items$load("ReferenceTable", "Test R ReferenceTable")

  expect_equal(reference_table$name, "Test R ReferenceTable")
  expect_equal(reference_table$description, "Test Description")
  expect_equal(reference_table$labels, list())
  expect_equal(
    reference_table$key,
    ReferenceTableColumn$new(name = "TestKey",
                             column_type = "Numeric",
                             unit_name = "m3")
  )

  expect_equal(
    reference_table$values,
    c(
      ReferenceTableColumn$new(name = "Value One",
                               column_type = "Numeric",
                               unit_name = "m3"),
      ReferenceTableColumn$new(name = "Value Two",
                               column_type = "Bool",
                               unit_name = " ")
    )
  )
})

test_that("ReferenceTable can be deleted", {
  result <- sp$items$delete("ReferenceTable", "Test R ReferenceTable")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("ReferenceTable", "Test R ReferenceTable"))
})
