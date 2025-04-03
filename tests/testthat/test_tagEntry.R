context("TagEntry instanciation and conversion to list")

test_that("TagEntry instanciation and conversion works (no end date)", {
  entity <- Entity$new(name = "TestName",
                       entity_type_name = "Well",
                       alias = "TestAlias")
  tag <- Tag$new(name = "TagName", tag_group = "TestGroup")

  tag_entry <- TagEntry$new(entity = entity,
                            tag = tag,
                            start = "2020-02-01T00:00:00.000Z")
  tag_entry_list <- tag_entry$toList()

  expect_equal(tag_entry_list,
               list(Tag = list(Name = "TagName", TagGroup = "TestGroup"),
                    Entity = list(
                      Name = "TestName",
                      EntityTypeName = "Well",
                      Alias = "TestAlias",
                      IsOpportunity = FALSE
                    ),
                    Start = "2020-02-01T00:00:00.000Z",
                    End = ""))
})

test_that("TagEntry instanciation and conversion works (with end date)", {
  entity <- Entity$new(name = "TestName",
                       entity_type_name = "Well",
                       alias = "TestAlias")
  tag <- Tag$new(name = "TagName", tag_group = "TestGroup")

  tag_entry <- TagEntry$new(entity = entity,
                            tag = tag,
                            start = "2020-02-01T00:00:00.000Z",
                            end = "2020-03-01T00:00:00.000Z")
  tag_entry_list <- tag_entry$toList()

  expect_equal(tag_entry_list,
               list(Tag = list(Name = "TagName", TagGroup = "TestGroup"),
                    Entity = list(Name = "TestName",
                                  EntityTypeName = "Well",
                                  Alias = "TestAlias",
                                  IsOpportunity = FALSE),
                    Start = "2020-02-01T00:00:00.000Z",
                    End = "2020-03-01T00:00:00.000Z"))
})

test_that("TagEntry instanciation and conversion works (empty constructor)", {
  tag_entry <- TagEntry$new()
  tag_entry_list <- tag_entry$toList()

  expect_equal(tag_entry_list,
               list(Tag = "",
                    Entity = "",
                    Start = "",
                    End = ""))
})

test_that("TagEntryFilter instanciation and conversion works", {
  filter <- TagEntryFilter$new(entityNames = list("Well01", "Well02"),
                               tagName = "Active")
  filter_list <- filter$toList()

  expect_equal(filter_list,
               list(Entity = "",
                    Entities = list("Well01", "Well02"),
                    Tag = "Active",
                    Tags = "",
                    TagGroup = "",
                    TagGroups = "",
                    Start = "",
                    End = ""))
})

test_that("TagEntryFilter instanciation and conversion works (empty constructor)", {
  filter <- TagEntryFilter$new()
  filter_list <- filter$toList()

  expect_equal(filter_list,
               list(Entity = "",
                    Entities = "",
                    Tag = "",
                    Tags = "",
                    TagGroup = "",
                    TagGroups = "",
                    Start = "",
                    End = ""))
})
