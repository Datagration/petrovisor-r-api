context("LogEntry Tests")

test_that("LogEntry instantiation and conversion works", {
  le <- LogEntry$new(timestamp = "2020-03-01-00:00:00.000Z",
                     message = "Test message",
                     category = "Test R",
                     user_name = "test user",
                     severity = "Debug",
                     workspace = "Testing",
                     item_name = "My Item",
                     item_type = "Unknown",
                     item_change = "Other",
                     schedule_name = "Test R Schedule",
                     workflow_name = "Test R Workflow",
                     start_time = "2020-02-29T00:00:00.000Z",
                     end_time = "2020-03-01T00:00:00.000Z",
                     elapsed_time = "20",
                     message_details = "My details",
                     directory = "Test Directory")

  listed <- le$to_list()

  expect_equal(listed,
               list(Timestamp = "2020-03-01-00:00:00.000Z",
                    Message = "Test message",
                    Category = "Test R",
                    UserName = "test user",
                    Severity = "Debug",
                    Workspace = "Testing",
                    ItemName = "My Item",
                    ItemType = "Unknown",
                    ItemChange = "Other",
                    Schedule = "Test R Schedule",
                    Workflow = "Test R Workflow",
                    StartTime = "2020-02-29T00:00:00.000Z",
                    EndTime = "2020-03-01T00:00:00.000Z",
                    ElapsedTime = "20",
                    MessageDetails = "My details",
                    Directory = "Test Directory"))
})

test_that("LogEntry instantiation and conversion works (empty constructor)", {
  le <- LogEntry$new(timestamp = "2020-03-01-00:00:00.000Z",
                     item_change = "Other",
                     item_type = "Unknown",
                     severity = "Debug")

  listed <- le$to_list()

  expect_equal(listed,
               list(Timestamp = "2020-03-01-00:00:00.000Z",
                    Message = "",
                    Category = "",
                    UserName = "",
                    Severity = "Debug",
                    Workspace = "",
                    ItemName = "",
                    ItemType = "Unknown",
                    ItemChange = "Other",
                    Schedule = "",
                    Workflow = "",
                    StartTime = "",
                    EndTime = "",
                    ElapsedTime = "",
                    MessageDetails = "",
                    Directory = ""))
})

# Setup
log_entry_one <- LogEntry$new(severity = "Debug",
                              message = "This is a test message",
                              category = "R Test",
                              item_type = "Unknown",
                              item_change = "Other")

log_entry_two <- LogEntry$new(severity = "Debug",
                              message = "This is a test message",
                              category = "R Test",
                              item_type = "Unknown",
                              item_change = "Other")

# Perform tests
test_that("LogEntry can be saved", {
  result <- sp$logs$save(c(log_entry_one, log_entry_two))

  expect_equal(result$status_code, 200)
})

test_that("LogEntry Categories can be retrieved", {
  categories <- sp$logs$load_categories()

  expect_true(length(categories) > 0)
  expect_true(is.element("R Test", categories))
})

test_that("LogEntry can be retrieved", {
  retrieved_entries <- sp$logs$load(categories = c("R Test"))

  expect_equal(nrow(retrieved_entries), 2)
})

test_that("LogEntry can be removed", {
  # make sure log entries created to day are deleted too (hence the -1)
  result <- sp$logs$delete(days_to_keep = -1, category = "R Test")

  expect_equal(result$status_code, 200)

  retrieved_entries <- sp$logs$load(categories = c("R Test"))

  expect_true(length(retrieved_entries) == 0)
})
