context("Tag instanciation and conversion to list")

test_that("Tag instanciation and conversion works",{
  tag <- Tag$new(name = "TagName", tagGroup = "TestGroup")
  tagList <- tag$toList()

  expect_equal(tagList, list(Name = "TagName", TagGroup = "TestGroup"))
})

test_that("Tag instanciation and conversion works (empty constructor)",{
  tag <- Tag$new()
  tagList <- tag$toList()

  expect_equal(tagList, list(Name = "", TagGroup = ""))
})
