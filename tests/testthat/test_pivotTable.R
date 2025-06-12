context("PivotTable Tests")

test_that("Pivot table instantiation and conversion works", {
  pt <- PivotTable$new(name = "MyPivotTable",
                       add_entity_alias_column = TRUE,
                       entity_parent_columns = 2,
                       add_entity_type_column = TRUE,
                       scope_formula = "MyScopeFormula",
                       entity_set_formula = "MyEsetformula",
                       table_formula = "MyTableFormula",
                       hierarchy_name = "MyHierarchy",
                       tag_entry_date = "2020-03-01T00:00:00.000Z",
                       saved_date = "2020-03-01T00:00:00.000Z",
                       used_tags = list("Tag1", "Tag2", "Tag3"),
                       skip_empty_rows = TRUE,
                       add_is_opportunity_column = TRUE,
                       append_data = TRUE,
                       description = "Description",
                       labels = list("label1", "label2"))

  listed <- pt$toList()

  expect_equal(listed,
               list(Name = "MyPivotTable",
                    AddEntityAliasColumn = TRUE,
                    AddEntityParentsColumns = 2,
                    AddEntityTypeColumn = TRUE,
                    ScopeFormula = "MyScopeFormula",
                    EntitySetFormula = "MyEsetformula",
                    TableFormula = "MyTableFormula",
                    HierarchyName = "MyHierarchy",
                    TagEntryDate = "2020-03-01T00:00:00.000Z",
                    SavedDate = "2020-03-01T00:00:00.000Z",
                    UsedTags = list("Tag1", "Tag2", "Tag3"),
                    SkipEmptyDataRows = TRUE,
                    AddIsOpportunityColumn = TRUE,
                    AppendSavedData = TRUE,
                    Description = "Description",
                    Labels = list("label1", "label2")))
})

test_that("Pivot table instantiation and conversion works (empty constructor)", {
  pt <- PivotTable$new()

  listed <- pt$toList()

  expect_equal(listed,
               list(Name = "",
                    AddEntityAliasColumn = "",
                    AddEntityParentsColumns = 0,
                    AddEntityTypeColumn = "",
                    ScopeFormula = "",
                    EntitySetFormula = "",
                    TableFormula = "",
                    HierarchyName = "",
                    TagEntryDate = NA,
                    SavedDate = NA,
                    UsedTags = list(),
                    SkipEmptyDataRows = "",
                    AddIsOpportunityColumn = "",
                    AppendSavedData = "",
                    Description = "",
                    Labels = list()))
})

test_that("PivotTable can be created", {
  # create pivot table
  pivot_table_definition <-
    PivotTable$new(name = "Test R PivotTable",
                   add_entity_alias_column = TRUE,
                   add_entity_type_column = TRUE,
                   scope_formula = "Scope \"At Date\"\r\n\tBetween #01/01/2025#\r\n\tAnd #02/01/2025#\r\n\tStep Daily\r\nEnd Scope",
                   entity_set_formula = "Entity Set \"Test\"\r\n\tEntitiesByType(\"Well\")\r\nEnd Set",
                   table_formula = "Table \"Test\"\r\n\tColumn \"Oil\" in \"m3\"\r\n\t\t\"produced oil per time increment\" in \"m3\"\r\n\tEnd Column\r\nEnd Table",
                   skip_empty_rows = TRUE,
                   add_is_opportunity_column = TRUE,
                   append_data = TRUE,
                   description = "Test R Description")

  result <- sp$items$save("PivotTable", pivot_table_definition)

  expect_equal(result$status_code, 201)
})

test_that("PivotTable can be retrieved", {
  retrieved_definition <- sp$items$load("PivotTable", "Test R PivotTable")

  pivot_table_definition <-
    PivotTable$new(name = "Test R PivotTable",
                   add_entity_alias_column = TRUE,
                   entity_parent_columns = NULL,
                   add_entity_type_column = TRUE,
                   scope_formula = "Scope \"At Date\"\r\n\tBetween #01/01/2025#\r\n\tAnd #02/01/2025#\r\n\tStep Daily\r\nEnd Scope",
                   entity_set_formula = "Entity Set \"Test\"\r\n\tEntitiesByType(\"Well\")\r\nEnd Set",
                   table_formula = "Table \"Test\"\r\n\tColumn \"Oil\" in \"m3\"\r\n\t\t\"produced oil per time increment\" in \"m3\"\r\n\tEnd Column\r\nEnd Table",
                   skip_empty_rows = TRUE,
                   add_is_opportunity_column = TRUE,
                   append_data = TRUE,
                   description = "Test R Description")

  # check equality
  expect_equal(retrieved_definition, pivot_table_definition)
})

test_that("PivotTable can be deleted", {
  result <- sp$items$delete("PivotTable", "Test R PivotTable")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("PivotTable", "Test R PivotTable"))
})
