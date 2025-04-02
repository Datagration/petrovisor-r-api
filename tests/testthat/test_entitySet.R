context("EntitySet Tests")

test_that("Entity set instanciation and conversion works", {
  e1 <- Entity$new(name = "MyWell1",
                   entity_type_name = "Well",
                   alias = "MyAlias1")

  e2 <- Entity$new(name = "MyWell2",
                   entity_type_name = "Well",
                   alias = "MyAlias2")

  eset <- EntitySet$new(name = "MyEntitySet",
                        entities = list(e1, e2),
                        formula = "MyEsetFormula",
                        description = "My new entity set",
                        labels = list("label1", "label2"))

  listed <- eset$toList()

  expect_equal(listed,
               list(Name = "MyEntitySet",
                    Entities = list(e1$toList(), e2$toList()),
                    Formula = "MyEsetFormula",
                    Description = "My new entity set",
                    Labels = list("label1", "label2")))
})

test_that("Entity set instanciation and conversion works (empty constructor)", {
  eset <- EntitySet$new()

  listed <- eset$toList()

  expect_equal(listed,
               list(Name = "",
                    Entities = "",
                    Formula = "",
                    Description = "",
                    Labels = list()))
})

test_that("EntitySet can be created", {
  # create entities for the entity set
  entity_one <- Entity$new(name = "TestName 1",
                           entity_type_name = "Well",
                           alias = "TestAlias 1",
                           is_opportunity = FALSE)
  sp$repositoryService$AddOrEditItem("Entity", entity_one)

  entity_two <- Entity$new(name = "TestName 2",
                           entity_type_name = "Well",
                           alias = "TestAlias 2",
                           is_opportunity = FALSE)
  sp$repositoryService$AddOrEditItem("Entity", entity_two)

  entity_set <- EntitySet$new(name = "Eset R Test",
                              entities = list(entity_one, entity_two))

  result <- sp$repositoryService$AddOrEditItem("EntitySet", entity_set)

  expect_equal(result$status_code, 201)
})

test_that("EntitySet can be retrieved", {
  entity_set <- sp$repositoryService$GetItemByName("EntitySet", "Eset R Test")

  expect_equal(entity_set$name, "Eset R Test")
  expect_equal(entity_set$labels, list())
  expect_equal(entity_set$entities, c(Entity$new(name = "TestName 1",
                                                 entity_type_name = "Well",
                                                 alias = "TestAlias 1",
                                                 is_opportunity = NULL),
                                      Entity$new(name = "TestName 2",
                                                 entity_type_name = "Well",
                                                 alias = "TestAlias 2",
                                                 is_opportunity = NULL)))
})

test_that("EntitySet can be deleted", {
  result <- sp$repositoryService$DeleteItem("EntitySet", "Eset R Test")

  expect_equal(result$status_code, 200)
  expect_error(sp$repositoryService$GetItemByName("EntitySet", "Eset R Test"))

  # clean up - remove entities created during tests
  sp$repositoryService$DeleteItem("Entity", "TestName 1")
  sp$repositoryService$DeleteItem("Entity", "TestName 2")
})
