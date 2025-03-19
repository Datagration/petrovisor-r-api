##### SCOPE #####
context("Scope instanciation and conversion to list")

test_that("Scope instanciation and conversion works",{
  scp <- Scope$new(name = "MyScope",
                   start = "2020-03-01T00:00:00.000Z",
                   end = "2020-03-01T00:00:00.000Z",
                   timeIncrement = "Daily",
                   formula = "MyFormula",
                   isLocked = TRUE,
                   user = "MyUser",
                   isFavorite = TRUE,
                   labels = list("label1", "label2"))

  listed <- scp$toList()

  expect_equal(listed,
               list(Name = "MyScope",
                    Start = "2020-03-01T00:00:00.000Z",
                    End = "2020-03-01T00:00:00.000Z",
                    TimeIncrement = "Daily",
                    Formula = "MyFormula",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Scope instanciation and conversion works (empty constructor)",{
  scp <- Scope$new()

  listed <- scp$toList()

  expect_equal(listed,
               list(Name = "",
                    Start = "",
                    End = "",
                    TimeIncrement = "EveryMinute",
                    Formula = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
