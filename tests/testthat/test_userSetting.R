##### USER SETTING #####
context("User setting instantiation and conversion to list")

test_that("User setting instantiation and conversion works",{
  us <- UserSetting$new(name = "MySetting",
                        value = "MyValue")

  listed <- us$toList()

  expect_equal(listed,
               list(Name = "MySetting",
                    Value = "MyValue"))
})

test_that("User setting instantiation and conversion works
          (empty constructor)",{
  us <- UserSetting$new()

  listed <- us$toList()

  expect_equal(listed,
               list(Name = "",
                    Value = ""))
})
