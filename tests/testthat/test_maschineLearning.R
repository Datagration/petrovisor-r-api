##### ML MODEL #####
context("ML model instanciation and conversion to list")

test_that("ML model instanciation and conversion works",{
  mlModel <- MLModel$new(name = "MyModel",
                         modelType = "Regression",
                         trainedModel = "MyTrainedModel",
                         tableFormula = "MyTableFormula",
                         contextFormula = "MyContextFormula",
                         labelColumnName = "MyLabelColumnName",
                         isLocked = TRUE,
                         user = "MyUser",
                         isFavorite = TRUE,
                         labels = list("label1", "label2"))

  listed <- mlModel$toList()

  expect_equal(listed,
               list(Name = "MyModel",
                    ModelType = "Regression",
                    TrainedModel = "MyTrainedModel",
                    TableFormula = "MyTableFormula",
                    ContextFormula = "MyContextFormula",
                    LabelColumnName = "MyLabelColumnName",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("ML model instanciation and conversion works (empty constructor)",{
  mlModel <- MLModel$new()

  listed <- mlModel$toList()

  expect_equal(listed,
               list(Name = "",
                    ModelType = "",
                    TrainedModel = "",
                    TableFormula = "",
                    ContextFormula = "",
                    LabelColumnName = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
