##### CONTEXT #####
context("Context instanciation and conversion to list")

test_that("Context instanciation and conversion works",{
  eset <- EntitySet$new(name = "MyEset",
                        entities = c(Entity$new(name = "Well1",
                                                entityTypeName = "Well",
                                                alias = "WellAlias1"),
                                     Entity$new(name = "Well2",
                                                entityTypeName = "Well",
                                                alias = "WellAlias2")),
                        formula = "EsetFormula",
                        isLocked = FALSE,
                        user = "MyUser",
                        isFavorite = FALSE,
                        labels = list("label1", "label2"))

  scope <- Scope$new(name = "MyScope",
                     start = "2020-01-01T00:00:00.000Z",
                     end = "2020-03-01T00:00:00.000Z",
                     timeIncrement = "Daily",
                     formula = "ScopeFormula",
                     isLocked = FALSE,
                     user = "MyUser",
                     isFavorite = FALSE,
                     labels = list("label1", "label2"))

  hierarchy <- Hierarchy$new(name = "MyHierarchy",
                             relationship = list(Well1 = "FieldA",
                                                 Well2 = "FieldB"),
                             isLocked = FALSE,
                             user = "MyUser",
                             isFavorite = TRUE,
                             labels = list("label1", "label2"))

  context <- Context$new(name = "MyContext",
                         entitySet = eset,
                         scope = scope,
                         hierarchy = hierarchy,
                         formula = "MyContextFormula",
                         isLocked = TRUE,
                         user = "MyUser",
                         isFavorite = TRUE,
                         labels = list("label1", "label2"))

  listed <- context$toList()

  expect_equal(listed,
               list(Name = "MyContext",
                    EntitySet = eset$toList(),
                    Scope = scope$toList(),
                    Hierarchy = hierarchy$toList(),
                    Formula = "MyContextFormula",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Context instanciation and conversion works
          (empty constructor)",{
  context <- Context$new()

  listed <- context$toList()

  expect_equal(listed,
               list(Name = "",
                    EntitySet = "",
                    Scope = "",
                    Hierarchy = "",
                    Formula = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
