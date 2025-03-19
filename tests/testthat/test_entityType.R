##### ENTITY TYPE #####
context("Entity type instanciation and conversion to list")

test_that("Entity type instanciation and conversion works",{
  etype <- EntityType$new(name = "MyEntityType",
                          rank = 11)

  listed <- etype$toList()

  expect_equal(listed,
               list(Name = "MyEntityType",
                    Rank = 11))
})

test_that("Entity type instanciation and conversion works (empty constructor)",{
  etype <- EntityType$new()

  listed <- etype$toList()

  expect_equal(listed,
               list(Name = "",
                    Rank = ""))
})
