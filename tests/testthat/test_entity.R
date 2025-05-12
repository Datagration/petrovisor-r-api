context("Entity Tests")

test_that("Entity instantiation and conversion works", {
  entity <- Entity$new(name = "TestName",
                       entity_type_name = "Well",
                       alias = "TestAlias",
                       is_opportunity = FALSE)
  entity_list <- entity$toList()

  expect_equal(entity_list, list(Name = "TestName",
                                 EntityTypeName = "Well",
                                 Alias = "TestAlias",
                                 IsOpportunity = FALSE))
})

test_that("Entity instantiation and conversion works (empty constructor)", {
  entity <- Entity$new()
  entity_list <- entity$toList()

  expect_equal(entity_list, list(Name = "",
                                 EntityTypeName = "",
                                 Alias = "",
                                 IsOpportunity = FALSE))
})

test_that("Entity can be created", {
  entity <- Entity$new(name = "TestName",
                       entity_type_name = "Well",
                       alias = "TestAlias",
                       is_opportunity = FALSE)

  result <- sp$items$save("Entity", entity)

  expect_equal(result$status_code, 201)
})

test_that("Entity can be retrieved", {
  entity <- sp$items$load("Entity", "TestName")

  expect_equal(entity, Entity$new(name = "TestName",
                                  entity_type_name = "Well",
                                  alias = "TestAlias",
                                  is_opportunity = NULL))
})

test_that("Entity can be deleted", {
  result <- sp$items$delete("Entity", "TestName")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("Entity", "TestName"))
})
