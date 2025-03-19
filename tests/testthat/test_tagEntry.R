context("TagEntry instanciation and conversion to list")

test_that("TagEntry instanciation and conversion works (no end date)",{
  entity <- Entity$new(name = "TestName",
                       entityTypeName = "Well",
                       alias = "TestAlias")
  tag <- Tag$new(name = "TagName", tagGroup = "TestGroup")

  tagEntry <- TagEntry$new(entity = entity,
                           tag = tag,
                           start = "2020-02-01T00:00:00.000Z")
  tagEntryList <- tagEntry$toList()

  expect_equal(tagEntryList,
               list(Tag = list(Name = "TagName", TagGroup = "TestGroup"),
                    Entity = list(Name = "TestName", EntityTypeName = "Well", Alias = "TestAlias"),
                    Start = "2020-02-01T00:00:00.000Z",
                    End = ""))
})

test_that("TagEntry instanciation and conversion works (with end date)",{
  entity <- Entity$new(name = "TestName",
                       entityTypeName = "Well",
                       alias = "TestAlias")
  tag <- Tag$new(name = "TagName", tagGroup = "TestGroup")

  tagEntry <- TagEntry$new(entity = entity,
                           tag = tag,
                           start = "2020-02-01T00:00:00.000Z",
                           end = "2020-03-01T00:00:00.000Z")
  tagEntryList <- tagEntry$toList()

  expect_equal(tagEntryList,
               list(Tag = list(Name = "TagName", TagGroup = "TestGroup"),
                    Entity = list(Name = "TestName",
                                  EntityTypeName = "Well",
                                  Alias = "TestAlias"),
                    Start = "2020-02-01T00:00:00.000Z",
                    End = "2020-03-01T00:00:00.000Z"))
})

test_that("TagEntry instanciation and conversion works (empty constructor)",{
  tagEntry <- TagEntry$new()
  tagEntryList <- tagEntry$toList()

  expect_equal(tagEntryList,
               list(Tag = "",
                    Entity = "",
                    Start = "",
                    End = ""))
})

test_that("TagEntryFilter instanciation and conversion works", {
  filter <- TagEntryFilter$new(entityNames = list("Well01", "Well02"),
                               tagName = "Active")
  filterList <- filter$toList()

  expect_equal(filterList,
               list(Entity = "",
                    Entities = list("Well01", "Well02"),
                    Tag = "Active",
                    Tags = "",
                    TagGroup = "",
                    TagGroups = "",
                    Start = "",
                    End = ""))
})

test_that("TagEntryFilter instanciation and conversion works
          (empty constructor)", {
  filter <- TagEntryFilter$new()
  filterList <- filter$toList()

  expect_equal(filterList,
               list(Entity = "",
                    Entities = "",
                    Tag = "",
                    Tags = "",
                    TagGroup = "",
                    TagGroups = "",
                    Start = "",
                    End = ""))
})
