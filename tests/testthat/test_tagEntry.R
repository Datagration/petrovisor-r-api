context("TagEntry Tests")

test_that("TagEntry instanciation and conversion works (no end date)", {
  tag_entry <- TagEntry$new(entity_name = "Test Entity",
                            tag_name = "Test R Tag",
                            start = "2020-02-01T00:00:00.000Z")

  tag_entry_list <- tag_entry$to_list()

  expect_equal(tag_entry_list,
               list(TagName = "Test R Tag",
                    EntityName = "Test Entity",
                    Start = "2020-02-01T00:00:00.000Z",
                    End = ""))
})

test_that("TagEntry instanciation and conversion works (with end date)", {
  tag_entry <- TagEntry$new(entity_name = "Test Entity",
                            tag_name = "Test R Tag",
                            start = "2020-02-01T00:00:00.000Z",
                            end = "2020-03-01T00:00:00.000Z")

  tag_entry_list <- tag_entry$to_list()

  expect_equal(tag_entry_list,
               list(TagName = "Test R Tag",
                    EntityName = "Test Entity",
                    Start = "2020-02-01T00:00:00.000Z",
                    End = "2020-03-01T00:00:00.000Z"))
})

test_that("TagEntry instanciation and conversion works (empty constructor)", {
  tag_entry <- TagEntry$new()
  tag_entry_list <- tag_entry$to_list()

  expect_equal(tag_entry_list,
               list(TagName = "",
                    EntityName = "",
                    Start = "",
                    End = ""))
})

# Setup
# Create new entity and tag for testing
entity <- Entity$new(name = "TestName",
                     entity_type_name = "Well",
                     alias = "TestAlias",
                     is_opportunity = FALSE)

sp$items$save("Entity", entity)

tag <- Tag$new(name = "Test R Tag",
               tag_group = "Testing")

sp$items$save("Tag", tag)

# Create dataframe of tag entries
new_tag_entries <- data.frame(
  # Tag
  c(tag$name,
    tag$name,
    tag$name,
    tag$name,
    tag$name,
    tag$name),
  # Entity
  c(entity$name,
    entity$name,
    entity$name,
    entity$name,
    entity$name,
    entity$name),
  # Start
  c("2023-01-01T00:00:00",
    "2023-05-01T00:00:00",
    "2023-09-01T00:00:00",
    "2024-01-01T00:00:00",
    "2025-01-01T00:00:00",
    "2025-04-01T00:00:00"),
  # End
  c("2023-04-01T00:00:00",
    "2023-07-01T00:00:00",
    "2023-12-01T00:00:00",
    "2024-08-01T00:00:00",
    "2025-02-01T00:00:00",
    NA)
)

colnames(new_tag_entries) <- c("tag_name",
                               "entity_name",
                               "start",
                               "end")

# Perform tests
test_that("Tag entries can be saved", {
  result <- sp$tag_entries$save(new_tag_entries)

  expect_equal(result$status_code, 200)
})

test_that("Tag entries can be retrieved", {
  tag_entries_retrieved <- sp$tag_entries$load(tag_names = c(tag$name))

  expect_equal(tag_entries_retrieved, new_tag_entries)
})

test_that("Tag entries in range can be deleted (no end)", {
  result <- sp$tag_entries$delete_range(entity_name = entity$name,
                                        tag_name = tag$name,
                                        start = "2025-01-01T00:00:00")

  expect_equal(result$status_code, 200)

  tag_entries_retrieved <- sp$tag_entries$load(tag_names = c(tag$name))

  expect_equal(tag_entries_retrieved, new_tag_entries[1:4, ])
})

test_that("Tag entries in range can be deleted (with end)", {
  result <- sp$tag_entries$delete_range(entity_name = entity$name,
                                        tag_name = tag$name,
                                        start = "2024-01-01T00:00:00",
                                        end = "2024-12-01T00:00:00")

  expect_equal(result$status_code, 200)

  tag_entries_retrieved <- sp$tag_entries$load(tag_names = c(tag$name))

  expect_equal(tag_entries_retrieved, new_tag_entries[1:3, ])
})

test_that("Tag entries can be deleted", {
  result <- sp$tag_entries$delete(new_tag_entries[1:3, ])

  expect_equal(result$status_code, 200)

  tag_entries_retrieved <- sp$tag_entries$load(tag_names = c(tag$name))

  expect_equal(tag_entries_retrieved, list())
})

# Clean up
# Remoce test tag
sp$items$delete("Tag", tag$name)
# Remove test entity
sp$items$delete("Entity", entity$name)
