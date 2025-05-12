context("Context Tests")

test_that("Context instantiation and conversion works", {
  e1 <- Entity$new(name = "MyWell1",
                   entity_type_name = "Well",
                   alias = "MyAlias1")

  e2 <- Entity$new(name = "MyWell2",
                   entity_type_name = "Well",
                   alias = "MyAlias2")

  parent <- Entity$new(name = "Test Parent",
                       entity_type_name = "Field",
                       alias = "Test Parent",
                       is_opportunity = FALSE)

  eset <- EntitySet$new(name = "MyEntitySet",
                        entities = list(e1, e2),
                        formula = "MyEsetFormula",
                        description = "My new entity set",
                        labels = list("label1", "label2"))

  scope <- Scope$new(name = "Test R Scope",
                     start = "2020-03-01T00:00:00.000Z",
                     end = "2020-03-01T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 5000,
                     formula = "Test Formula",
                     description = "Test description",
                     labels = list("label1", "label2"))

  # build relationship
  relationship <- list()
  relationship[[e1$name]] <- parent$name
  relationship[[e1$name]] <- parent$name
  relationship[[parent$name]] <- NA

  hierarchy <- Hierarchy$new(name = "Hierarchy R Test",
                             relationship = relationship,
                             is_time_dependent = FALSE,
                             time_stamp = NULL,
                             description = "Test R Description")

  context <- Context$new(name = "MyContext",
                         entity_set = eset,
                         scope = scope,
                         hierarchy = hierarchy,
                         loading_scenario_name = "Loading Scenario",
                         saving_scenario_name = "Saving Scenario",
                         scenario_data_only = FALSE,
                         formula = "MyFormula",
                         description = "Description",
                         labels = list("label1", "label2"))

  listed <- context$toList()

  expect_equal(listed,
               list(Name = "MyContext",
                    EntitySet = eset$toList(),
                    Scope = scope$toList(),
                    Hierarchy = hierarchy$toList(),
                    LoadScenarioName = "Loading Scenario",
                    ScenarioDataOnly = FALSE,
                    SavingScenarioName = "Saving Scenario",
                    Formula = "MyFormula",
                    Description = "Description",
                    Labels = list("label1", "label2")))
})

test_that("Context instantiation and conversion works (empty constructor)", {
  context <- Context$new()

  listed <- context$toList()

  expect_equal(listed,
               list(Name = "",
                    EntitySet = "",
                    Scope = "",
                    Hierarchy = "",
                    LoadScenarioName = "",
                    ScenarioDataOnly = FALSE,
                    SavingScenarioName = "",
                    Formula = "",
                    Description = "",
                    Labels = list()))
})

test_that("Context can be created", {
  # create entities for the entity set
  entity_one <- Entity$new(name = "TestNameOne",
                           entity_type_name = "Well",
                           alias = "TestAliasOne",
                           is_opportunity = FALSE)
  sp$items$save("Entity", entity_one)

  entity_two <- Entity$new(name = "TestNameTwo",
                           entity_type_name = "Well",
                           alias = "TestAliasTwo",
                           is_opportunity = FALSE)
  sp$items$save("Entity", entity_two)

  entity_set <- EntitySet$new(name = "Eset R Test",
                              entities = list(entity_one, entity_two))
  sp$items$save("EntitySet", entity_set)

  # create scope
  scope <- Scope$new(name = "Test R Scope",
                     start = "2020-03-01T00:00:00.000Z",
                     end = "2020-03-01T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 10,
                     end_depth = 1500,
                     description = "Test R Scope")
  sp$items$save("Scope", scope)

  # create hierarchy
  parent <- Entity$new(name = "Test Parent",
                       entity_type_name = "Field",
                       alias = "Test Parent",
                       is_opportunity = FALSE)
  sp$items$save("Entity", parent)

  # build relationship
  relationship <- list()
  relationship[[entity_one$name]] <- parent$name
  relationship[[entity_two$name]] <- parent$name
  relationship[[parent$name]] <- NA

  hierarchy <- Hierarchy$new(name = "Hierarchy R Test",
                             relationship = relationship,
                             is_time_dependent = FALSE,
                             time_stamp = NULL,
                             description = "Test R Description")

  sp$items$save("Hierarchy", hierarchy)

  # create scenario
  scenario <- Scenario$new(name = "Test R Scenario")
  sp$items$save("Scenario", scenario)

  # create context
  context <- Context$new(name = "Test R Context",
                         entity_set = entity_set,
                         scope = scope,
                         hierarchy = hierarchy,
                         loading_scenario_name = "Test R Scenario",
                         saving_scenario_name = "Test R Scenario",
                         scenario_data_only = FALSE,
                         description = "Test R Context Description")

  result <- sp$items$save("Context", context)

  expect_equal(result$status_code, 201)
})

test_that("Context can be retrieved", {
  context <- sp$items$load("Context", "Test R Context")

  # create items for checking equality
  # create entities for the entity set
  entity_one <- Entity$new(name = "TestNameOne",
                           entity_type_name = "Well",
                           alias = "TestAliasOne",
                           is_opportunity = NULL)

  entity_two <- Entity$new(name = "TestNameTwo",
                           entity_type_name = "Well",
                           alias = "TestAliasTwo",
                           is_opportunity = NULL)

  entity_set <- EntitySet$new(name = "Eset R Test",
                              entities = list(entity_one, entity_two),
                              formula = "Entity Set \"Eset R Test\"\n\t\"TestNameOne\"\n\t\"TestNameTwo\"\nEnd Set")

  # create scope
  scope <- Scope$new(name = "Test R Scope",
                     start = "2020-03-01T00:00:00",
                     end = "2020-03-01T00:00:00",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 10,
                     end_depth = 1500,
                     formula = "Scope \"Test R Scope\" \n\tBetween #03/01/2020 00:00# \n\tAnd #03/01/2020 00:00# \n\tStep Daily \n\tFrom 10 \n\tTo 1500 \n\tDepth Step Meter \nEnd Scope")

  # create hierarchy
  parent <- Entity$new(name = "Test Parent",
                       entity_type_name = "Field",
                       alias = "Test Parent",
                       is_opportunity = NULL)

  # build relationship
  relationship <- list("Test Parent", "Test Parent", NULL)
  names(relationship) <- c("TestNameOne", "TestNameTwo", "Test Parent")

  hierarchy <- Hierarchy$new(name = "Hierarchy R Test",
                             relationship = relationship,
                             is_time_dependent = FALSE,
                             time_stamp = NULL,
                             description = "Test R Description")

  # create context
  ctx <- Context$new(name = "Test R Context",
                     entity_set = entity_set,
                     scope = scope,
                     hierarchy = hierarchy,
                     loading_scenario_name = "Test R Scenario",
                     saving_scenario_name = "Test R Scenario",
                     scenario_data_only = NULL,
                     formula = "Context \"Test R Context\"\n\tEntity Set \"Eset R Test\"\n\tScope \"Test R Scope\"\n\tHierarchy \"Hierarchy R Test\"\n\tScenario \"Test R Scenario\" Saving Scenario\nEnd Context\n\n",
                     description = "Test R Context Description")

  # check equality
  expect_equal(context, ctx)
})

test_that("Context can be deleted", {
  result <- sp$items$delete("Context", "Test R Context")

  expect_equal(result$status_code, 200)
  expect_error(sp$items$load("Context", "Test R Context"))

  # clean up - remove items created during tests
  sp$items$delete("Hierarchy", "Hierarchy R Test")
  sp$items$delete("EntitySet", "Eset R Test")
  sp$items$delete("Scope", "Test R Scope")
  sp$items$delete("Scenario", "Test R Scenario")
  sp$items$delete("Entity", "TestNameOne")
  sp$items$delete("Entity", "TestNameTwo")
  sp$items$delete("Entity", "Test Parent")
})
