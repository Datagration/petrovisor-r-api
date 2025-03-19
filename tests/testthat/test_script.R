##### CLEANSING SCRIPT #####
context("Cleansing script instanciation and conversion to list")

test_that("Cleansing Script instanciation and conversion works",{
  cScript <- CleansingScript$new(name = "MyScript",
                                 content = "MyContent",
                                 isLocked = TRUE,
                                 user = "MyUser",
                                 isFavorite = TRUE,
                                 labels = list("label1", "label2"))

  listed <- cScript$toList()

  expect_equal(listed,
               list(Name = "MyScript",
                    Content = "MyContent",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Cleansing script instanciation and conversion works (empty constructor)",{
  cScript <- RScript$new()

  listed <- cScript$toList()

  expect_equal(listed,
               list(Name = "",
                    Content = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})

##### P SHARP SCRIPT #####
context("P# script instanciation and conversion to list")

test_that("P# Script instanciation and conversion works",{
  pScript <- PSharpScript$new(name = "MyScript",
                         content = "MyContent",
                         isLocked = TRUE,
                         user = "MyUser",
                         isFavorite = TRUE,
                         labels = list("label1", "label2"))

  listed <- pScript$toList()

  expect_equal(listed,
               list(Name = "MyScript",
                    Content = "MyContent",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("P# script instanciation and conversion works (empty constructor)",{
  pScript <- PSharpScript$new()

  listed <- pScript$toList()

  expect_equal(listed,
               list(Name = "",
                    Content = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})

##### R SCRIPT #####
context("R script instanciation and conversion to list")

test_that("R Script instanciation and conversion works",{
  rScript <- RScript$new(name = "MyScript",
                         content = "MyContent",
                         isLocked = TRUE,
                         user = "MyUser",
                         isFavorite = TRUE,
                         labels = list("label1", "label2"))

  listed <- rScript$toList()

  expect_equal(listed,
               list(Name = "MyScript",
                    Content = "MyContent",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("R script instanciation and conversion works (empty constructor)",{
  rScript <- RScript$new()

  listed <- rScript$toList()

  expect_equal(listed,
               list(Name = "",
                    Content = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
