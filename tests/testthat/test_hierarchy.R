context("Hierarchy Tests")

test_that("Hierarchy instantiation and conversion works", {
  relationship <- data.frame(
    child = c("Child1", "Child2", "Child3"),
    parent = c("Parent1", "Parent2", "Parent3")
  )

  hierarchy <- Hierarchy$new(name = "Test R Hierarchy",
                             relationship = relationship,
                             is_time_dependent = FALSE,
                             time_stamp = NULL,
                             description = "Test R Description",
                             labels = list("label1", "label2"))

  listed <- hierarchy$toList()

  expect_equal(listed,
               list(Name = "Test R Hierarchy",
                    Relationship = list(Child1 = "Parent1",
                                        Child2 = "Parent2",
                                        Child3 = "Parent3"),
                    IsTimeDependent = FALSE,
                    Description = "Test R Description",
                    Labels = list("label1", "label2")))
})

test_that("Hierarchy instantiation and conversion works (empty constructor)", {
  hierarchy <- Hierarchy$new()

  listed <- hierarchy$toList()

  expect_equal(listed,
               list(Name = "",
                    Relationship = list(),
                    IsTimeDependent = FALSE,
                    Description = "",
                    Labels = list()))
})

test_that("Static hierarchy can be created", {
  # create entities for the hierarchy
  child_one <- Entity$new(name = "Test Child One",
                          entity_type_name = "Well",
                          alias = "Test Child One",
                          is_opportunity = FALSE)
  sp$items$save("Entity", child_one)

  child_two <- Entity$new(name = "Test Child Two",
                          entity_type_name = "Well",
                          alias = "Test Child Two",
                          is_opportunity = FALSE)
  sp$items$save("Entity", child_two)

  parent <- Entity$new(name = "Test Parent",
                       entity_type_name = "Field",
                       alias = "Test Parent",
                       is_opportunity = FALSE)
  sp$items$save("Entity", parent)

  # build relationship
  relationship <- data.frame(
    child = c("Test Parent", "Test Child One", "Test Child Two"),
    parent = c(NA, "Test Parent", "Test Parent")
  )

  hierarchy <- Hierarchy$new(name = "Hierarchy R Test",
                             relationship = relationship,
                             is_time_dependent = FALSE,
                             time_stamp = NULL,
                             description = "Test R Description")

  result <- sp$items$save("Hierarchy", hierarchy)

  expect_equal(result$status_code, 201)
})

test_that("Static hierarchy can be retrieved", {
  hierarchy <- sp$items$load("Hierarchy", "Hierarchy R Test")

  expected_relationship <- data.frame(
    child = c("Test Parent", "Test Child One", "Test Child Two"),
    parent = c(NA, "Test Parent", "Test Parent")
  )

  expect_equal(hierarchy, Hierarchy$new(name = "Hierarchy R Test",
                                        relationship = expected_relationship,
                                        is_time_dependent = FALSE,
                                        time_stamp = NULL,
                                        description = "Test R Description"))
})

test_that("Static hierarchy can be deleted", {
  result <- sp$items$delete("Hierarchy", "Hierarchy R Test")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("Hierarchy", "Hierarchy R Test"))

  # clean up - remove entities created during tests
  sp$items$delete("Entity", "Test Child One")
  sp$items$delete("Entity", "Test Child Two")
  sp$items$delete("Entity", "Test Parent")
})

test_that("Time-dependent hierarchy can be created", {
  # create entities for the hierarchy
  child_one <- Entity$new(name = "Test Child One",
                          entity_type_name = "Well",
                          alias = "Test Child One",
                          is_opportunity = FALSE)
  sp$items$save("Entity", child_one)

  child_two <- Entity$new(name = "Test Child Two",
                          entity_type_name = "Well",
                          alias = "Test Child Two",
                          is_opportunity = FALSE)
  sp$items$save("Entity", child_two)

  parent <- Entity$new(name = "Test Parent",
                       entity_type_name = "Field",
                       alias = "Test Parent",
                       is_opportunity = FALSE)
  sp$items$save("Entity", parent)

  # build relationship
  relationship <- data.frame(
    child = c("Test Parent", "Test Child One", "Test Child Two"),
    parent = c(NA, "Test Parent", "Test Parent")
  )

  hierarchy <- Hierarchy$new(name = "TD Hierarchy R Test",
                             relationship =
                               list(`2025-01-01T00:00:00` = relationship),
                             is_time_dependent = TRUE,
                             time_stamp = "2025-01-01T00:00:00",
                             description = "Test R Description")

  result <- sp$items$save("Hierarchy", hierarchy)

  expect_equal(result$status_code, 201)
})

test_that("Time-dependent hierarchy can be retrieved", {
  hierarchy <- sp$items$load("Hierarchy", "TD Hierarchy R Test")

  expected_relationship <- list(`2025-01-01T00:00:00` = data.frame(
    child = c("Test Parent", "Test Child One", "Test Child Two"),
    parent = c(NA, "Test Parent", "Test Parent")
  ))

  expect_equal(hierarchy, Hierarchy$new(name = "TD Hierarchy R Test",
                                        relationship = expected_relationship,
                                        is_time_dependent = TRUE,
                                        time_stamp = "2025-01-01T00:00:00",
                                        description = "Test R Description"))
})

test_that("Time-dependent hierarchy can be deleted", {
  result <- sp$items$delete("Hierarchy", "TD Hierarchy R Test")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("Hierarchy", "TD Hierarchy R Test"))

  # clean up - remove entities created during tests
  sp$items$delete("Entity", "Test Child One")
  sp$items$delete("Entity", "Test Child Two")
  sp$items$delete("Entity", "Test Parent")
})
