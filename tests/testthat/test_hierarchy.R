context("Hierarchy Tests")

test_that("Hierarchy instanciation and conversion works", {
  hierarchy <- Hierarchy$new(name = "Test R Hierarchy",
                             relationship = list(Child1 = "Parent1",
                                                 Child2 = "Parent2",
                                                 Child3 = "Parent3"),
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

test_that("Hierarchy instanciation and conversion works (empty constructor)", {
  hierarchy <- Hierarchy$new()

  listed <- hierarchy$toList()

  expect_equal(listed,
               list(Name = "",
                    Relationship = "",
                    IsTimeDependent = FALSE,
                    Description = "",
                    Labels = list()))
})

test_that("Hierarchy can be created", {
  # create entities for the hierarchy
  child_one <- Entity$new(name = "Test Child One",
                          entity_type_name = "Well",
                          alias = "Test Child One",
                          is_opportunity = FALSE)
  sp$repositoryService$AddOrEditItem("Entity", child_one)

  child_two <- Entity$new(name = "Test Child Two",
                          entity_type_name = "Well",
                          alias = "Test Child Two",
                          is_opportunity = FALSE)
  sp$repositoryService$AddOrEditItem("Entity", child_two)

  parent <- Entity$new(name = "Test Parent",
                       entity_type_name = "Field",
                       alias = "Test Parent",
                       is_opportunity = FALSE)
  sp$repositoryService$AddOrEditItem("Entity", parent)

  # build relationship
  relationship <- list()
  relationship[[child_one$name]] <- parent$name
  relationship[[child_two$name]] <- parent$name
  relationship[[parent$name]] <- NA

  hierarchy <- Hierarchy$new(name = "Hierarchy R Test",
                             relationship = relationship,
                             is_time_dependent = FALSE,
                             time_stamp = NULL,
                             description = "Test R Description")

  result <- sp$repositoryService$AddOrEditItem("Hierarchy", hierarchy)

  expect_equal(result$status_code, 201)
})

test_that("Hierarchy can be retrieved", {
  hierarchy <- sp$repositoryService$GetItemByName("Hierarchy",
                                                  "Hierarchy R Test")

  expected_relationship <- list("Test Parent", "Test Parent", NULL)
  names(expected_relationship) <- c("Test Child One",
                                    "Test Child Two",
                                    "Test Parent")

  expect_equal(hierarchy, Hierarchy$new(name = "Hierarchy R Test",
                                        relationship = expected_relationship,
                                        is_time_dependent = FALSE,
                                        time_stamp = NULL,
                                        description = "Test R Description"))
})

test_that("Hierarchy can be deleted", {
  result <- sp$repositoryService$DeleteItem("Hierarchy", "Hierarchy R Test")

  expect_equal(result$status_code, 200)
  expect_error(sp$repositoryService$GetItemByName("Hierarchy",
                                                  "Hierarchy R Test"))

  # clean up - remove entities created during tests
  sp$repositoryService$DeleteItem("Entity", "Test Child One")
  sp$repositoryService$DeleteItem("Entity", "Test Child Two")
  sp$repositoryService$DeleteItem("Entity", "Test Parent")
})
