##### LOGENTRY #####
context("LogEntry instanciation and conversion to list")

test_that("LogEntry instanciation and conversion works",{
  le <- LogEntry$new(category = "MyCategory",
                     elapsedTime = "20",
                     endTime = "2020-03-01T00:00:00.000Z",
                     entityName = "Well1",
                     message = "MyMessage",
                     messageDetails = "MyDetails",
                     numberOfItems = "5",
                     scheduleName = "MySchedule",
                     scriptName = "MyScriptName",
                     severity = "High",
                     signalName = "MySignal",
                     startTime = "2020-02-29T00:00:00.000Z",
                     tagName = "MyTagName",
                     timestamp = "2020-03-01-00:00:00.000Z",
                     unitName = "MyUnitName",
                     userName = "MyUser",
                     valueAfter = "MyValueAfter",
                     valueBefore = "MyValueBefore",
                     workflowName = "MyWorkflow",
                     workspace = "MyWorkspace")

  listed <- le$toList()

  expect_equal(listed,
               list(Timestamp = "2020-03-01-00:00:00.000Z",
                    Message = "MyMessage",
                    Category = "MyCategory",
                    UserName = "MyUser",
                    Severity = "High",
                    Workspace = "MyWorkspace",
                    Schedule = "MySchedule",
                    Workflow = "MyWorkflow",
                    StartTime = "2020-02-29T00:00:00.000Z",
                    EndTime = "2020-03-01T00:00:00.000Z",
                    Script = "MyScriptName",
                    Entity = "Well1",
                    Signal = "MySignal",
                    Unit = "MyUnitName",
                    Tag = "MyTagName",
                    NumberOfItems = "5",
                    ValueBefore = "MyValueBefore",
                    ValueAfter = "MyValueAfter",
                    ElapsedTime = "20",
                    MessageDetails = "MyDetails"))
})

test_that("LogEntry instanciation and conversion works (empty constructor)",{
  le <- LogEntry$new()

  listed <- le$toList()

  expect_equal(listed,
               list(Timestamp = "",
                    Message = "",
                    Category = "",
                    UserName = "",
                    Severity = "",
                    Workspace = "",
                    Schedule = "",
                    Workflow = "",
                    StartTime = "",
                    EndTime = "",
                    Script = "",
                    Entity = "",
                    Signal = "",
                    Unit = "",
                    Tag = "",
                    NumberOfItems = "",
                    ValueBefore = "",
                    ValueAfter = "",
                    ElapsedTime = "",
                    MessageDetails = ""))
})
