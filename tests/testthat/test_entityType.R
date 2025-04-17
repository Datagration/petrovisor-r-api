context("EntityType Tests")

test_that("Entity type instanciation and conversion works", {
  etype <- EntityType$new(name = "MyEntityType",
                          image = "11x87ztrd")

  listed <- etype$toList()

  expect_equal(listed,
               list(Name = "MyEntityType",
                    Image = "11x87ztrd"))
})

test_that("Entity type instanciation and conversion works (empty constructor)", {
  etype <- EntityType$new()

  listed <- etype$toList()

  expect_equal(listed,
               list(Name = "",
                    Image = ""))
})

test_that("Entity type can be created", {
  entity_type <- EntityType$new(name = "Test R Type")

  result <- sp$items$save("EntityType", entity_type)

  expect_equal(result$status_code, 201)
})

test_that("Entity type can be retrieved", {
  entity_type <- sp$items$load("EntityType", "Test R Type")

  expect_equal(entity_type, EntityType$new(name = "Test R Type",
                                           image = NULL))
})

test_that("Entity type can be deleted", {
  result <- sp$items$delete("EntityType", "Test R Type")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("EntityType", "Test R Type"))
})
