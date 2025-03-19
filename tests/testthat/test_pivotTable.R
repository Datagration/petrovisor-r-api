##### PIVOT TABLE #####
context("Pivot table instanciation and conversion to list")

test_that("Pivot table instanciation and conversion works",{
  pt <- PivotTable$new(name = "MyPivotTable",
                       addEntityAliasColumn = TRUE,
                       addEntityParentColumn = TRUE,
                       addEntityTypeColumn = TRUE,
                       scopeFormula = "MyScopeFormula",
                       entitySetFormula = "MyEsetformula",
                       tableFormula = "MyTableFormula",
                       hierarchyName = "MyHierarchy",
                       useDataPreloading = TRUE,
                       allowScriptExecution = FALSE,
                       tagEntryDate = "2020-03-01T00:00:00.000Z",
                       savedDate = "2020-03-01T00:00:00.000Z",
                       usedTags = list("Tag1", "Tag2", "Tag3"),
                       dateTimeFormat = "DateTimeFormat",
                       numberFormat = "NumberFormat",
                       nanValueString = "NaN",
                       tagActiveString = "Active",
                       isLocked = TRUE,
                       user = "MyUser",
                       isFavorite = TRUE,
                       labels = list("label1", "label2"))

  listed <- pt$toList()

  expect_equal(listed,
               list(Name = "MyPivotTable",
                    AddEntityAliasColumn = TRUE,
                    AddEntityParentColumn = TRUE,
                    AddEntityTypeColumn = TRUE,
                    ScopeFormula = "MyScopeFormula",
                    EntitySetFormula = "MyEsetformula",
                    TableFormula = "MyTableFormula",
                    HierarchyName = "MyHierarchy",
                    UseDataPreloading = TRUE,
                    AllowScriptExecution = FALSE,
                    TagEntryDate = "2020-03-01T00:00:00.000Z",
                    SavedDate = "2020-03-01T00:00:00.000Z",
                    UsedTags = list("Tag1", "Tag2", "Tag3"),
                    DateTimeFormat = "DateTimeFormat",
                    NumberFormat = "NumberFormat",
                    NaNValueString = "NaN",
                    TagActiveString = "Active",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Pivot table instanciation and conversion works (empty constructor)",{
  pt <- PivotTable$new()

  listed <- pt$toList()

  expect_equal(listed,
               list(Name = "",
                    AddEntityAliasColumn = "",
                    AddEntityParentColumn = "",
                    AddEntityTypeColumn = "",
                    ScopeFormula = "",
                    EntitySetFormula = "",
                    TableFormula = "",
                    HierarchyName = "",
                    UseDataPreloading = "",
                    AllowScriptExecution = "",
                    TagEntryDate = "",
                    SavedDate = "",
                    UsedTags = "",
                    DateTimeFormat = "",
                    NumberFormat = "",
                    NaNValueString = "",
                    TagActiveString = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
