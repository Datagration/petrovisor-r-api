context("Entity instanciation and conversion to list")

test_that("Entity instanciation and conversion works",{
  entity <- Entity$new(name = "TestName",
                       entityTypeName = "Well",
                       alias = "TestAlias")
  entityList <- entity$toList()

  expect_equal(entityList, list(Name = "TestName",
                                EntityTypeName = "Well",
                                Alias = "TestAlias"))
})

test_that("Entity instanciation and conversion works (empty constructor)",{
  entity <- Entity$new()
  entityList <- entity$toList()

  expect_equal(entityList, list(Name = "",
                                EntityTypeName = "",
                                Alias = ""))
})
