##### PROCESS TEMPLATE #####
context("Process template instantiation and conversion to list")

test_that("Process template instantiation and conversion works",{
  cf1 <- CustomField$new(name = "MyCustomField1",
                         evaluatedColumn =
                           Column$new(name = "MyColumn1",
                                      unit = Unit$new(name = "MyUnitName1",
                                                      measurement_name =
                                                        "Length",
                                                      factor = 1,
                                                      summand = 0),
                                      formula = "MyColumnFormula",
                                      isStatic = FALSE,
                                      savingSignalName = "diameter1",
                                      saveToParentEntity = FALSE),
                         evaluatedDate = "2020-01-01T00:00:00.000Z",
                         evaluatedValue = "25",
                         fieldType = 0,
                         scriptName = "MyScript",
                         scriptColumnName = "MyColumn1",
                         currentManualValue = "0",
                         initialValue = "1",
                         expressionTime = 1,
                         activeStep = "MyStep",
                         settingName = "MySetting")

  cf2 <- CustomField$new(name = "MyCustomField2",
                         evaluatedColumn =
                           Column$new(name = "MyColumn2",
                                      unit = Unit$new(name = "MyUnitName2",
                                                      measurement_name =
                                                        "Length",
                                                      factor = 1,
                                                      summand = 0),
                                      formula = "MyColumnFormula",
                                      isStatic = FALSE,
                                      savingSignalName = "diameter2",
                                      saveToParentEntity = FALSE),
                         evaluatedDate = "2020-01-01T00:00:00.000Z",
                         evaluatedValue = "25",
                         fieldType = 0,
                         scriptName = "MyScript",
                         scriptColumnName = "MyColumn2",
                         currentManualValue = "0",
                         initialValue = "1",
                         expressionTime = 1,
                         activeStep = "MyStep",
                         settingName = "MySetting")

  s1 <- Step$new(name = "MyStep1",
                 userGroupName = "MyUserGroup1")

  s2 <- Step$new(name = "MyStep2",
                 userGroupName = "MyUserGroup2")

  pt <- ProcessTemplate$new(name = "MyProcessTemplate",
                            description = "Description",
                            processTemplateGroup = "MyGroup",
                            dueIntervals = 0,
                            priority = "High",
                            severity = "Medium",
                            steps = list(s1, s2),
                            customFields = list(cf1, cf2))

  listed <- pt$toList()

  expect_equal(listed,
               list(Name = "MyProcessTemplate",
                    Description = "Description",
                    ProcessTemplateGroup = "MyGroup",
                    DueIntervals = 0,
                    Priority = "High",
                    Severity = "Medium",
                    Steps = list(s1$toList(), s2$toList()),
                    CustomFields = list(cf1$toList(), cf2$toList())))
})

test_that("Process template instantiation and conversion works
          (empty constructor)",{
  pt <- ProcessTemplate$new()

  listed <- pt$toList()

  expect_equal(listed,
               list(Name = "",
                    Description = "",
                    ProcessTemplateGroup = "",
                    DueIntervals = "",
                    Priority = "",
                    Severity = "",
                    Steps = "",
                    CustomFields = ""))
})

##### STEP #####
context("Step instantiation and conversion to list")

test_that("Step instantiation and conversion works",{
  step <- Step$new(name = "MyStep",
                   userGroupName = "MyUserGroup")

  listed <- step$toList()

  expect_equal(listed,
               list(Name = "MyStep",
                    UserGroup = "MyUserGroup"))
})

test_that("Step instantiation and conversion works (empty constructor)",{
  step <- Step$new()

  listed <- step$toList()

  expect_equal(listed,
               list(Name = "",
                    UserGroup = ""))
})

##### CUSTOM FIELD #####
context("Custom field instantiation and conversion to list")

test_that("Custom field instantiation and conversion works",{
  c1 <- Column$new(name = "MyColumn1",
                   unit = Unit$new(name = "MyUnitName1",
                                   measurement_name = "Length",
                                   factor = 1,
                                   summand = 0),
                   formula = "MyColumnFormula",
                   isStatic = FALSE,
                   savingSignalName = "diameter1",
                   saveToParentEntity = FALSE)

  cf <- CustomField$new(name = "MyCustomField",
                        evaluatedColumn = c1,
                        evaluatedDate = "2020-01-01T00:00:00.000Z",
                        evaluatedValue = "25",
                        fieldType = 0,
                        scriptName = "MyScript",
                        scriptColumnName = "MyColumn1",
                        currentManualValue = "0",
                        initialValue = "1",
                        expressionTime = 1,
                        activeStep = "MyStep",
                        settingName = "MySetting")

  listed <- cf$toList()

  expect_equal(listed,
               list(Name = "MyCustomField",
                    Evaluated = list(Column = c1$toList(),
                                     Date = "2020-01-01T00:00:00.000Z",
                                     Value = "25"),
                    FieldType = 0,
                    ScriptName = "MyScript",
                    ScriptColumnName = "MyColumn1",
                    CurrentManualValue = "0",
                    InitialValue = "1",
                    ExpressionTime = 1,
                    ActiveStep = "MyStep",
                    SettingName = "MySetting"))
})

test_that("Custom field instantiation and conversion works
          (empty constructor)",{
  cf <- CustomField$new()

  listed <- cf$toList()

  expect_equal(listed,
               list(Name = "",
                    Evaluated = list(Column = "",
                                     Date = "",
                                     Value = ""),
                    FieldType = "",
                    ScriptName = "",
                    ScriptColumnName = "",
                    CurrentManualValue = "",
                    InitialValue = "",
                    ExpressionTime = "",
                    ActiveStep = "",
                    SettingName = ""))
})
