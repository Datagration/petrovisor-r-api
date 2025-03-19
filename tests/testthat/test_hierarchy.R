##### HIERARCHY #####
context("Hierarchy instanciation and conversion to list")

test_that("Hierarchy instanciation and conversion works",{
  hierarchy <- Hierarchy$new(name = "MyHierarchy",
                             relationship = list(Child1 = "Parent1",
                                                 Child2 = "Parent2",
                                                 Child3 = "Parent3"),
                             isLocked = TRUE,
                             user = "MyUser",
                             isFavorite = TRUE,
                             labels = list("label1", "label2"))

  listed <- hierarchy$toList()

  expect_equal(listed,
               list(Name = "MyHierarchy",
                    Relationship = list(Child1 = "Parent1",
                                        Child2 = "Parent2",
                                        Child3 = "Parent3"),
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Hierarchy instanciation and conversion works (empty constructor)",{
  hierarchy <- Hierarchy$new()

  listed <- hierarchy$toList()

  expect_equal(listed,
               list(Name = "",
                    Relationship = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
