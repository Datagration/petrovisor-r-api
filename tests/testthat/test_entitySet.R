##### ENTITY SET #####
context("Entity set instanciation and conversion to list")

test_that("Entity set instanciation and conversion works",{
  e1 <- Entity$new(name = "MyWell1",
                   entityTypeName = "Well",
                   alias = "MyAlias1")

  e2 <- Entity$new(name = "MyWell2",
                   entityTypeName = "Well",
                   alias = "MyAlias2")

  eset <- EntitySet$new(name = "MyEntitySet",
                        entities = list(e1, e2),
                        formula = "MyEsetFormula",
                        isLocked = TRUE,
                        user = "MyUser",
                        isFavorite = TRUE,
                        labels = list("label1", "label2"))

  listed <- eset$toList()

  expect_equal(listed,
               list(Name = "MyEntitySet",
                    Entities = list(e1$toList(), e2$toList()),
                    Formula = "MyEsetFormula",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Entity set instanciation and conversion works (empty constructor)",{
  eset <- EntitySet$new()

  listed <- eset$toList()

  expect_equal(listed,
               list(Name = "",
                    Entities = "",
                    Formula = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
