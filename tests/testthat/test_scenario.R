context("Scenario Tests")

test_that("Scenario instanciation and conversion works", {
  cs_one <- ConfigurationSetting$new(name = "MyCsOne",
                                     numeric_value = 1,
                                     string_value = "string",
                                     list_value = list("string1", "string2"),
                                     enumeration_value = list(One = 1,
                                                              Two = 2,
                                                              Three = 3),
                                     dictionary_value = list(One = 1,
                                                             Two = 2,
                                                             Three = 3),
                                     value_type = "Numeric",
                                     unit_name = "m",
                                     possible_values = list("value1", "value2"),
                                     is_system = FALSE,
                                     description = "wsv test description",
                                     labels = list())

  cs_two <- ConfigurationSetting$new(name = "MyCsTwo",
                                     numeric_value = 2,
                                     string_value = "string",
                                     list_value = list("string1", "string2"),
                                     enumeration_value = list(One = 1,
                                                              Two = 2,
                                                              Three = 3),
                                     dictionary_value = list(One = 1,
                                                             Two = 2,
                                                             Three = 3),
                                     value_type = "Numeric",
                                     unit_name = "m",
                                     possible_values = list("value1", "value2"),
                                     is_system = FALSE,
                                     description = "wsv test description",
                                     labels = list())

  scenario <- Scenario$new(name = "Test R Scenario",
                           configuration_settings = list(cs_one, cs_two),
                           description = "Test R Scenario Description",
                           labels = list("label1", "label2"))

  listed <- scenario$toList()

  expect_equal(listed,
               list(Name = "Test R Scenario",
                    WorkspaceValues = list(cs_one$toList(), cs_two$toList()),
                    Description = "Test R Scenario Description",
                    Labels = list("label1", "label2")))
})

test_that("Scenario instanciation and conversion works (empty constructor)", {
  scenario <- Scenario$new()

  listed <- scenario$toList()

  expect_equal(listed,
               list(Name = "",
                    WorkspaceValues = list(),
                    Description = "",
                    Labels = list()))
})

test_that("Scenario can be created", {
  scenario <- Scenario$new(name = "Test R Scenario")

  result <- sp$items$save("Scenario", scenario)

  expect_equal(result$status_code, 201)
})

test_that("Scenario can be retrieved", {
  scenario <- sp$items$load("Scenario", "Test R Scenario")

  expect_equal(scenario$name, "Test R Scenario")
  expect_equal(scenario$labels, list())
  expect_equal(scenario$description, NULL)
})

test_that("Scenario can be deleted", {
  result <- sp$items$delete("Scenario", "Test R Scenario")

  expect_equal(result$status_code, 200)
  expect_error(
    sp$items$load("Scenario", "Test R Scenario")
  )
})
