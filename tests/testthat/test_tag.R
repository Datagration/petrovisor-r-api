context("Tag Tests")

test_that("Tag instantiation and conversion works", {
  tag <- Tag$new(name = "TagName", tag_group = "TestGroup")
  tag_list <- tag$toList()

  expect_equal(tag_list, list(Name = "TagName", TagGroup = "TestGroup"))
})

test_that("Tag instantiation and conversion works (empty constructor)", {
  tag <- Tag$new()
  tag_list <- tag$toList()

  expect_equal(tag_list, list(Name = "", TagGroup = ""))
})

test_that("Tag can be created", {
  tag <- Tag$new(name = "Test R Tag",
                 tag_group = "Testing")

  result <- sp$items$save("Tag", tag)

  expect_equal(result$status_code, 201)
})

test_that("Tag can be retrieved", {
  tag <- sp$items$load("Tag", "Test R Tag")

  expect_equal(tag, Tag$new(name = "Test R Tag",
                            tag_group = "Testing"))
})

test_that("Tag can be deleted", {
  result <- sp$items$delete("Tag", "Test R Tag")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("Tag", "Test R Tag"))
})
