context("ConfigurationSetting Tests")

test_that("ConfigurationSetting instanciation and conversion works", {
  cs <- ConfigurationSetting$new(name = "MyCS",
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

  listed <- cs$toList()

  expect_equal(listed,
               list(Name = "MyCS",
                    NumericValue = 1,
                    StringValue = "string",
                    ListValue = list("string1", "string2"),
                    ValueType = "Numeric",
                    UnitName = "m",
                    PossibleValues = list("value1", "value2"),
                    IsSystem = FALSE,
                    Description = "wsv test description",
                    Labels = list(),
                    EnumerationValue = list(One = 1,
                                            Two = 2,
                                            Three = 3),
                    DictionaryValue = list(One = 1,
                                           Two = 2,
                                           Three = 3)))
})

test_that("ConfigurationSetting instanciation and conversion works (empty constructor)", {
  cs <- ConfigurationSetting$new()

  listed <- cs$toList()

  expect_equal(listed,
               list(Name = "",
                    NumericValue = "",
                    StringValue = "",
                    ListValue = list(),
                    ValueType = "Numeric",
                    UnitName = "",
                    PossibleValues = list(),
                    IsSystem = "",
                    Description = "",
                    Labels = list()))
})

test_that("ConfigurationSetting (Numeric) can be created", {
  cs <- ConfigurationSetting$new(name = "TestRCs",
                                 numeric_value = 1,
                                 value_type = "Numeric",
                                 is_system = FALSE,
                                 description = "wsv (numeric) test description")

  result <- sp$items$save("ConfigurationSetting", cs)

  expect_equal(result$status_code, 201)
})

test_that("ConfigurationSetting (Numeric) can be retrieved", {
  cs <- sp$items$load("ConfigurationSetting", "TestRCs")

  expect_equal(cs, ConfigurationSetting$new(name = "TestRCs",
                                            numeric_value = 1,
                                            string_value = "",
                                            list_value = list(),
                                            value_type = "Numeric",
                                            unit_name = "",
                                            possible_values = list(),
                                            is_system = NULL,
                                            description =
                                              "wsv (numeric) test description",
                                            labels = list()))
})

test_that("ConfigurationSetting (Numeric) can be deleted", {
  result <- sp$items$delete("ConfigurationSetting", "TestRCs")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("ConfigurationSetting", "TestRCs"))
})
