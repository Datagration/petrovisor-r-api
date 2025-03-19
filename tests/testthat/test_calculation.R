##### CLEANSING CALCULATION #####
context("Cleansing calculation instanciation and conversion to list")

test_that("CleansingCalculation instanciation and conversion works",{
  filter1 <- CleansingFilter$new(name = "MyFilterName1",
                                 formula = "MyFormula1",
                                 unitName = "m")
  filter2 <- CleansingFilter$new(name = "MyFilterName2",
                                 formula = "MyFormula2",
                                 unitName = "m")

  cc <- CleansingCalculation$new(name = "MyCC",
                                 filters = list(filter1, filter2),
                                 isLocked = TRUE,
                                 user = "MyUser",
                                 isFavorite = FALSE,
                                 labels = c("label1", "label2"),
                                 formula = "MyCcFormula")

  listed <- cc$toList()

  expect_equal(listed,
               list(Name = "MyCC",
                    Filters = list(filter1$toList(), filter2$toList()),
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = FALSE,
                    Labels = c("label1", "label2"),
                    Formula = "MyCcFormula"))
})

test_that("CleansingCalculation instanciation and conversion works
          (empty constructor)",{
  cc <- CleansingCalculation$new()

  listed <- cc$toList()

  expect_equal(listed,
               list(Name = "",
                    Filters = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = "",
                    Formula = ""))
})

##### CLEANSING FILTER #####
context("Cleansing filter instanciation and conversion to list")

test_that("CleansingFilter instanciation and conversion works",{
  filter <- CleansingFilter$new(name = "MyFilterName1",
                                 formula = "MyFormula1",
                                 unitName = "m")

  listed <- filter$toList()

  expect_equal(listed,
               list(Name = "MyFilterName1",
                    Formula = "MyFormula1",
                    UnitName = "m"))
})

test_that("CleansingFilter instanciation and conversion works
          (empty constructor)",{
  filter <- CleansingFilter$new()

  listed <- filter$toList()

  expect_equal(listed,
               list(Name = "",
                    Formula = "",
                    UnitName = ""))
})

##### EVENT CALCULATION #####
context("Event calculation instanciation and conversion to list")

test_that("EventCalculation instanciation and conversion works",{
  event1 <- Event$new(name = "MyEventName1",
                      formula = "MyFormula1")
  event2 <- Event$new(name = "MyEventName2",
                      formula = "MyFormula2")

  ec <- EventCalculation$new(name = "MyEC",
                             events = list(event1, event2),
                             inputTableNames = list("table1", "table2"),
                             isLocked = TRUE,
                             user = "MyUser",
                             isFavorite = FALSE,
                             labels = c("label1", "label2"),
                             formula = "MyEcFormula")

  listed <- ec$toList()

  expect_equal(listed,
               list(Name = "MyEC",
                    Events = list(event1$toList(), event2$toList()),
                    InputTableNames = list("table1", "table2"),
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = FALSE,
                    Labels = c("label1", "label2"),
                    Formula = "MyEcFormula"))
})

test_that("EventCalculation instanciation and conversion works
          (empty constructor)",{
  ec <- EventCalculation$new()

  listed <- ec$toList()

  expect_equal(listed,
               list(Name = "",
                    Events = "",
                    InputTableNames = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = "",
                    Formula = ""))
})

##### EVENT #####
context("Event instanciation and conversion to list")

test_that("Event instanciation and conversion works",{
  event <- Event$new(name = "MyEventName",
                     formula = "MyFormula")

  listed <- event$toList()

  expect_equal(listed,
               list(Name = "MyEventName",
                    Formula = "MyFormula"))
})

test_that("Event instanciation and conversion works (empty constructor)",{
  event <- Event$new()

  listed <- event$toList()

  expect_equal(listed,
               list(Name = "",
                    Formula = ""))
})

##### TABLE CALCULATION #####
context("Table calculation instanciation and conversion to list")

test_that("TableCalculation instanciation and conversion works",{
  column1 <- Column$new(name = "MyColumnName1",
                        unit = Unit$new(name = "MyUnit",
                                        measurementName = "Length",
                                        factor = 1,
                                        summand = 0),
                        formula = "MyFormula1",
                        isStatic = TRUE,
                        savingSignalName = "x-coordinate",
                        saveToParentEntity = FALSE)
  column2 <- Column$new(name = "MyColumnName2",
                        unit = Unit$new(name = "MyUnit",
                                        measurementName = "Length",
                                        factor = 1,
                                        summand = 0),
                        formula = "MyFormula2",
                        isStatic = TRUE,
                        savingSignalName = "y-coordinate",
                        saveToParentEntity = FALSE)

  tc <- TableCalculation$new(name = "MyTC",
                             columns = list(column1, column2),
                             inputTableNames = list("table1", "table2"),
                             isLocked = TRUE,
                             user = "MyUser",
                             isFavorite = FALSE,
                             labels = c("label1", "label2"),
                             formula = "MyTcFormula")

  listed <- tc$toList()

  expect_equal(listed,
               list(Name = "MyTC",
                    Columns = list(column1$toList(), column2$toList()),
                    InputTableNames = list("table1", "table2"),
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = FALSE,
                    Labels = c("label1", "label2"),
                    Formula = "MyTcFormula"))
})

test_that("TableCalculation instanciation and conversion works
          (empty constructor)",{
  tc <- TableCalculation$new()

  listed <- tc$toList()

  expect_equal(listed,
               list(Name = "",
                    Columns = "",
                    InputTableNames = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = "",
                    Formula = ""))
})

##### COLUMN #####
context("Column instanciation and conversion to list")

test_that("Column instanciation and conversion works",{
  column <- Column$new(name = "MyColumnName",
                        unit = Unit$new(name = "MyUnit",
                                        measurementName = "Length",
                                        factor = 1,
                                        summand = 0),
                        formula = "MyFormula",
                        isStatic = TRUE,
                        savingSignalName = "x-coordinate",
                        saveToParentEntity = FALSE)

  listed <- column$toList()

  expect_equal(listed,
               list(Name = "MyColumnName",
                    Unit = list(Name = "MyUnit",
                                MeasurementName = "Length",
                                Factor = 1,
                                Summand = 0),
                    Formula = "MyFormula",
                    IsStatic = TRUE,
                    SavingSignalName = "x-coordinate",
                    SaveToParentEntity = FALSE))
})

test_that("Column instanciation and conversion works (empty constructor)",{
  column <- Column$new()

  listed <- column$toList()

  expect_equal(listed,
               list(Name = "",
                    Unit = "",
                    Formula = "",
                    IsStatic = "",
                    SavingSignalName = "",
                    SaveToParentEntity = ""))
})
