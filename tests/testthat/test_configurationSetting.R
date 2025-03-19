##### CONFIGURATION SETTING #####
context("Configuration setting instanciation and conversion to list")

test_that("ConfigurationSetting instanciation and conversion works",{
  cs <- ConfigurationSetting$new(name = "MyCS",
                                 numericValue = 1,
                                 stringValue = "string",
                                 listValue = list("string1", "string2"),
                                 enumerationValue = list(One = 1,
                                                         Two = 2,
                                                         Three = 3),
                                 dictionaryValue = list(One = 1,
                                                        Two = 2,
                                                        Three = 3),
                                 valueType = "Numeric",
                                 unitName = "m",
                                 isSystem = FALSE)

  listed <- cs$toList()

  expect_equal(listed,
               list(Name = "MyCS",
                    NumericValue = 1,
                    StringValue = "string",
                    ListValue = list("string1", "string2"),
                    EnumerationValue = list(One = 1,
                                            Two = 2,
                                            Three = 3),
                    DictionaryValue = list(One = 1,
                                           Two = 2,
                                           Three = 3),
                    ValueType = "Numeric",
                    UnitName = "m",
                    IsSystem = FALSE))
})

test_that("ConfigurationSetting instanciation and conversion works
          (empty constructor)",{
  cs <- ConfigurationSetting$new()

  listed <- cs$toList()

  expect_equal(listed,
               list(Name = "",
                    NumericValue = "",
                    StringValue = "",
                    ListValue = "",
                    EnumerationValue = "",
                    DictionaryValue = "",
                    ValueType = "Numeric",
                    UnitName = "",
                    IsSystem = ""))
})
