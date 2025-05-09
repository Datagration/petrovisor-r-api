##### WORKFLOW #####
context("Workflow instantiation and conversion to list")

test_that("Workflow instantiation and conversion works",{
  ma1 <- ActivityMappedArgument$new(argumentName = "MyMappedArgument1",
                                    argumentType = "StaticSignalUnit",
                                    acceptableArgumentTypes = list(
                                      "StaticSignalUnit",
                                      "StringSignalUnit"
                                    ),
                                    mappedSignalName = "MyMappedsignal",
                                    mappedUnitName = "MyMappedUnit",
                                    mappedString = "MyMappedstring")

  ma2 <- ActivityMappedArgument$new(argumentName = "MyMappedArgument2",
                                    argumentType = "StaticSignalUnit",
                                    acceptableArgumentTypes = list(
                                      "StaticSignalUnit",
                                      "StringSignalUnit"
                                    ),
                                    mappedSignalName = "MyMappedsignal",
                                    mappedUnitName = "MyMappedUnit",
                                    mappedString = "MyMappedstring")

  arg1 <- ActivityArgument$new(argumentName = "MyArgument1",
                               acceptableArgumentTypes = list(
                                 "StaticSignalUnit",
                                 "StringSignalUnit"
                               ),
                               defaultArgumentType = "StaticSignalUnit",
                               defaultSignalName = "DefaultSignal",
                               defaultUnitName = "DefaultUnit",
                               defaultString = "DefaultString")

  arg2 <- ActivityArgument$new(argumentName = "MyArgument2",
                               acceptableArgumentTypes = list(
                                 "StaticSignalUnit",
                                 "StringSignalUnit"
                               ),
                               defaultArgumentType = "StaticSignalUnit",
                               defaultSignalName = "DefaultSignal",
                               defaultUnitName = "DefaultUnit",
                               defaultString = "DefaultString")

  activity1 <- WorkflowActivity$new(activityType = "R script",
                                    activityName = "MyScript",
                                    settings = "MySettings",
                                    mappedInputArguments = list(ma1, ma2),
                                    mappedOutputArguments = list(ma1, ma2),
                                    inputArgumentsInfo = list(arg1, arg2))

  activity2 <- WorkflowActivity$new(activityType = "R script",
                                    activityName = "MyScript",
                                    settings = "MySettings",
                                    mappedInputArguments = list(ma1, ma2),
                                    mappedOutputArguments = list(ma1, ma2),
                                    inputArgumentsInfo = list(arg1, arg2))

  wf <- Workflow$new(name = "MyWorkflow",
                     activities = list(activity1, activity2),
                     isLocked = TRUE,
                     user = "MyUser",
                     isFavorite = TRUE,
                     labels = list("label1", "label2"))

  listed <- wf$toList()

  expect_equal(listed,
               list(Name = "MyWorkflow",
                    Activities = list(activity1$toList(), activity2$toList()),
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Workflow instantiation and conversion works (empty constructor)",{
  wf <- Workflow$new()

  listed <- wf$toList()

  expect_equal(listed,
               list(Name = "",
                    Activities = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})

##### WORKFLOW ACTIVITY #####
context("Workflow activity instantiation and conversion to list")

test_that("Workflow activity instantiation and conversion works",{
  ma1 <- ActivityMappedArgument$new(argumentName = "MyMappedArgument1",
                                    argumentType = "StaticSignalUnit",
                                    acceptableArgumentTypes = list(
                                      "StaticSignalUnit",
                                      "StringSignalUnit"
                                    ),
                                    mappedSignalName = "MyMappedsignal",
                                    mappedUnitName = "MyMappedUnit",
                                    mappedString = "MyMappedstring")

  ma2 <- ActivityMappedArgument$new(argumentName = "MyMappedArgument2",
                                    argumentType = "StaticSignalUnit",
                                    acceptableArgumentTypes = list(
                                      "StaticSignalUnit",
                                      "StringSignalUnit"
                                    ),
                                    mappedSignalName = "MyMappedsignal",
                                    mappedUnitName = "MyMappedUnit",
                                    mappedString = "MyMappedstring")

  arg1 <- ActivityArgument$new(argumentName = "MyArgument1",
                               acceptableArgumentTypes = list(
                                 "StaticSignalUnit",
                                 "StringSignalUnit"
                               ),
                               defaultArgumentType = "StaticSignalUnit",
                               defaultSignalName = "DefaultSignal",
                               defaultUnitName = "DefaultUnit",
                               defaultString = "DefaultString")

  arg2 <- ActivityArgument$new(argumentName = "MyArgument2",
                               acceptableArgumentTypes = list(
                                 "StaticSignalUnit",
                                 "StringSignalUnit"
                               ),
                               defaultArgumentType = "StaticSignalUnit",
                               defaultSignalName = "DefaultSignal",
                               defaultUnitName = "DefaultUnit",
                               defaultString = "DefaultString")

  wfa <- WorkflowActivity$new(activityType = "R script",
                              activityName = "MyScript",
                              settings = "MySettings",
                              mappedInputArguments = list(ma1, ma2),
                              mappedOutputArguments = list(ma1, ma2),
                              inputArgumentsInfo = list(arg1, arg2))

  listed <- wfa$toList()

  expect_equal(listed,
               list(ActivityType = "R script",
                    ActivityName = "MyScript",
                    Settings = "MySettings",
                    MappedInputArguments = list(ma1$toList(), ma2$toList()),
                    MappedOutputArguments = list(ma1$toList(), ma2$toList()),
                    InputArgumentsInfo = list(arg1$toList(), arg2$toList())))
})

test_that("Workflow activity instantiation and conversion works
          (empty constructor)",{
  wfa <- WorkflowActivity$new()

  listed <- wfa$toList()

  expect_equal(listed,
               list(ActivityType = "",
                    ActivityName = "",
                    Settings = "",
                    MappedInputArguments = "",
                    MappedOutputArguments = "",
                    InputArgumentsInfo = ""))
})

##### CUSTOM WORKFLOW ACTIVITY #####
context("Custom workflow activity instantiation and conversion to list")

test_that("Custom workflow activity instantiation and conversion works",{
  cwfa <- CustomWorkflowActivity$new(name = "MyActivity",
                                     className = "MyName",
                                     assemblyContent = "MyContent")

  listed <- cwfa$toList()

  expect_equal(listed,
               list(Name = "MyActivity",
                    ClassName = "MyName",
                    AssemblyContent = "MyContent"))
})

test_that("Custom workflow activity instantiation and conversion works
          (empty constructor)",{
  cwfa <- CustomWorkflowActivity$new()

  listed <- cwfa$toList()

  expect_equal(listed,
               list(Name = "",
                    ClassName = "",
                    AssemblyContent = ""))
})

##### R WORKFLOW ACTIVITY #####
context("R workflow activity instantiation and conversion to list")

test_that("R workflow activity instantiation and conversion works",{
  config <- RProviderConfiguration$new(providerType = "RNET",
                                       rServerAddress = "192.168.23.21",
                                       rServerPort = 8099,
                                       rServerScriptsFolder = "/home/scripts")

  arg1 <- ActivityArgument$new(argumentName = "MyArgument1",
                               acceptableArgumentTypes = list(
                                 "StaticSignalUnit",
                                 "StringSignalUnit"
                               ),
                               defaultArgumentType = "StaticSignalUnit",
                               defaultSignalName = "DefaultSignal",
                               defaultUnitName = "DefaultUnit",
                               defaultString = "DefaultString")

  arg2 <- ActivityArgument$new(argumentName = "MyArgument2",
                               acceptableArgumentTypes = list(
                                 "StaticSignalUnit",
                                 "StringSignalUnit"
                               ),
                               defaultArgumentType = "StaticSignalUnit",
                               defaultSignalName = "DefaultSignal",
                               defaultUnitName = "DefaultUnit",
                               defaultString = "DefaultString")

  rWorkflowActivity <- RWorkflowActivity$new(name = "MyRActivity",
                                             providerConfiguration = config,
                                             scriptName = "MyScript",
                                             functionName = "MyFunction",
                                             input = list(arg1, arg2),
                                             output = list(arg1, arg2))

  listed <- rWorkflowActivity$toList()

  expect_equal(listed,
               list(Name = "MyRActivity",
                    ProviderConfiguration = config$toList(),
                    ScriptName = "MyScript",
                    FunctionName = "MyFunction",
                    Input = list(arg1$toList(), arg2$toList()),
                    Output = list(arg1$toList(), arg2$toList())))
})

test_that("R workflow activity instantiation and conversion works
          (empty constructor)",{
  rWorkflowActivity <- RWorkflowActivity$new()

  listed <- rWorkflowActivity$toList()

  expect_equal(listed,
               list(Name = "",
                    ProviderConfiguration = "",
                    ScriptName = "",
                    FunctionName = "",
                    Input = "",
                    Output = ""))
})

##### R PROVIDER CONFIGURATION #####
context("R provider configuration instantiation and conversion to list")

test_that("R provider configuration instantiation and conversion works",{
  rConfig <- RProviderConfiguration$new(providerType = "RNET",
                                        rServerAddress = "192.168.23.21",
                                        rServerPort = 8099,
                                        rServerScriptsFolder = "/home/scripts")

  listed <- rConfig$toList()

  expect_equal(listed,
               list(ProviderType = "RNET",
                    RServerAddress = "192.168.23.21",
                    RServerPort = 8099,
                    RServerScriptsFolder = "/home/scripts"))
})

test_that("R provider configuration instantiation and conversion works
          (empty constructor)",{
  rConfig <- RProviderConfiguration$new()

  listed <- rConfig$toList()

  expect_equal(listed,
               list(ProviderType = "RNET",
                    RServerAddress = "",
                    RServerPort = "",
                    RServerScriptsFolder = ""))
})

##### ACTIVITY ARGUMENT #####
context("Activity argument instantiation and conversion to list")

test_that("Activity argument instantiation and conversion works",{
  aa <- ActivityArgument$new(argumentName = "MyArgument",
                             acceptableArgumentTypes = list(
                               "StaticSignalUnit",
                               "StringSignalUnit"
                             ),
                             defaultArgumentType = "StaticSignalUnit",
                             defaultSignalName = "DefaultSignal",
                             defaultUnitName = "DefaultUnit",
                             defaultString = "DefaultString")

  listed <- aa$toList()

  expect_equal(listed,
               list(ArgumentName = "MyArgument",
                    AcceptableArgumentTypes = list(
                      "StaticSignalUnit",
                      "StringSignalUnit"
                    ),
                    DefaultArgumentType = "StaticSignalUnit",
                    DefaultSignal = "DefaultSignal",
                    DefaultUnit = "DefaultUnit",
                    DefaultString = "DefaultString"))
})

test_that("Activity argument instantiation and conversion works
          (empty constructor)",{
  aa <- ActivityArgument$new()

  listed <- aa$toList()

  expect_equal(listed,
               list(ArgumentName = "",
                    AcceptableArgumentTypes = "",
                    DefaultArgumentType = "TimeSignalUnit",
                    DefaultSignal = "",
                    DefaultUnit = "",
                    DefaultString = ""))
})

##### ACTIVITY MAPPED ARGUMENT #####
context("Activity mapped argument instantiation and conversion to list")

test_that("Activity mapped argument instantiation and conversion works",{
  ama <- ActivityMappedArgument$new(argumentName = "MyMappedArgument",
                                    argumentType = "StaticSignalUnit",
                                    acceptableArgumentTypes = list(
                                      "StaticSignalUnit",
                                      "StringSignalUnit"
                                    ),
                                    mappedSignalName = "MyMappedsignal",
                                    mappedUnitName = "MyMappedUnit",
                                    mappedString = "MyMappedstring")

  listed <- ama$toList()

  expect_equal(listed,
               list(ArgumentName = "MyMappedArgument",
                    ArgumentType = "StaticSignalUnit",
                    AcceptableArgumentTypes = list(
                      "StaticSignalUnit",
                      "StringSignalUnit"
                    ),
                    MappedSignal = "MyMappedsignal",
                    MappedUnit = "MyMappedUnit",
                    MappedString = "MyMappedstring"))
})

test_that("Activity mapped argument instantiation and conversion works
          (empty constructor)",{
  ama <- ActivityMappedArgument$new()

  listed <- ama$toList()

  expect_equal(listed,
               list(ArgumentName = "",
                    ArgumentType = "TimeSignalUnit",
                    AcceptableArgumentTypes = "",
                    MappedSignal = "",
                    MappedUnit = "",
                    MappedString = ""))
})

##### WORKFLOW SCHEDULE #####
context("Workflow schedule instantiation and conversion to list")

test_that("Workflow schedule instantiation and conversion works",{
  se <- ScheduleEnd$new(endType = 1,
                        repetitionsCount = 2,
                        totalRepetitions = 5,
                        endDate = "2020-04-30T00:00:00.000Z")

  sr <- ScheduleRecurrence$new(recurrenceType = "Daily",
                               intervalsAmount = 2,
                               skipWeekend = TRUE,
                               repeatOnSpecificDayMonth = TRUE,
                               specificDayNumber = 0,
                               orderNumberDay = "First",
                               typeDayMonthDay = "Day",
                               runOnMonday = TRUE,
                               runOnTuesday = TRUE,
                               runOnWednesday = TRUE,
                               runOnThursday = TRUE,
                               runOnFriday = TRUE,
                               runOnSaturday = TRUE,
                               runOnSunday = TRUE)

  schedule <- WorkflowSchedule$new(name = "MySchedule",
                                   modified = "2020-04-01T00:00:00.000Z",
                                   scheduleName = "MyScheduleName",
                                   workflowName = "MyWorkflow",
                                   workspaceName = "MyWorkspace",
                                   active = TRUE,
                                   processingEntitySetName = "AllWells",
                                   processingScopeName = "Now",
                                   processingContextNames = list("Field A",
                                                                 "Field B"),
                                   absoluteDate = "2020-04-01T00:00:00.000Z",
                                   executedWorkflowName = "ExecutedWfName",
                                   executedWorkflowScheduleName = "SchedName",
                                   scheduleStart = "2020-04-01T00:00:00.000Z",
                                   scheduleEnd = se,
                                   scheduleRecurrence = sr,
                                   isLocked = TRUE,
                                   user = "MyUser",
                                   isFavorite = TRUE,
                                   labels = list("label1", "label2"))

  listed <- schedule$toList()

  expect_equal(listed,
               list(Name = "MySchedule",
                    Modified = "2020-04-01T00:00:00.000Z",
                    ScheduleName = "MyScheduleName",
                    WorkflowName = "MyWorkflow",
                    WorkspaceName = "MyWorkspace",
                    Active = TRUE,
                    ProcessingEntitySet = "AllWells",
                    ProcessingScope = "Now",
                    ProcessingContexts = list("Field A",
                                              "Field B"),
                    AbsoluteDate = "2020-04-01T00:00:00.000Z",
                    ExecutedWorkflowName = "ExecutedWfName",
                    ExecutedWorkflowScheduleName = "SchedName",
                    ScheduleStart = "2020-04-01T00:00:00.000Z",
                    ScheduleEnd = se$toList(),
                    ScheduleRecurrence = sr$toList(),
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Workflow schedule instantiation and conversion works
          (empty constructor)",{
  schedule <- WorkflowSchedule$new()

  listed <- schedule$toList()

  expect_equal(listed,
               list(Name = "",
                    Modified = "",
                    ScheduleName = "",
                    WorkflowName = "",
                    WorkspaceName = "",
                    Active = "",
                    ProcessingEntitySet = "",
                    ProcessingScope = "",
                    ProcessingContexts = "",
                    AbsoluteDate = "",
                    ExecutedWorkflowName = "",
                    ExecutedWorkflowScheduleName = "",
                    ScheduleStart = "",
                    ScheduleEnd = "",
                    ScheduleRecurrence = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})

##### SCHEDULE END #####
context("Schedule end instantiation and conversion to list")

test_that("Schedule end instantiation and conversion works",{
  se <- ScheduleEnd$new(endType = 1,
                        repetitionsCount = 2,
                        totalRepetitions = 5,
                        endDate = "2020-04-30T00:00:00.000Z")

  listed <- se$toList()

  expect_equal(listed,
               list(EndType = 1,
                    RepetitionsCount = 2,
                    TotalRepetitions = 5,
                    EndDate = "2020-04-30T00:00:00.000Z"))
})

test_that("Schedule end instantiation and conversion works
          (empty constructor)",{
  se <- ScheduleEnd$new()

  listed <- se$toList()

  expect_equal(listed,
               list(EndType = "",
                    RepetitionsCount = "",
                    TotalRepetitions = "",
                    EndDate = ""))
})

##### SCHEDULE RECURRENCE #####
context("Schedule recurrence instantiation and conversion to list")

test_that("Schedule recurrence instantiation and conversion works",{
  sr <- ScheduleRecurrence$new(recurrenceType = "Daily",
                               intervalsAmount = 2,
                               skipWeekend = TRUE,
                               repeatOnSpecificDayMonth = TRUE,
                               specificDayNumber = 0,
                               orderNumberDay = "First",
                               typeDayMonthDay = "Day",
                               runOnMonday = TRUE,
                               runOnTuesday = TRUE,
                               runOnWednesday = TRUE,
                               runOnThursday = TRUE,
                               runOnFriday = TRUE,
                               runOnSaturday = TRUE,
                               runOnSunday = TRUE)

  listed <- sr$toList()

  expect_equal(listed,
               list(RecurrenceType = "Daily",
                    IntervalsAmount = 2,
                    SkipWeekend = TRUE,
                    RepeatOnSpecificDayMonth = TRUE,
                    SpecificDayNumber = 0,
                    OrderNumberDay = "First",
                    TypeDayMonthDay = "Day",
                    RunOnMonday = TRUE,
                    RunOnTuesday = TRUE,
                    RunOnWednesday = TRUE,
                    RunOnThursday = TRUE,
                    RunOnFriday = TRUE,
                    RunOnSaturday = TRUE,
                    RunOnSunday = TRUE))
})

test_that("Schedule recurrence instantiation and conversion works
          (empty constructor)",{
  sr <- ScheduleRecurrence$new()

  listed <- sr$toList()

  expect_equal(listed,
               list(RecurrenceType = "",
                    IntervalsAmount = "",
                    SkipWeekend = "",
                    RepeatOnSpecificDayMonth = "",
                    SpecificDayNumber = "",
                    OrderNumberDay = "",
                    TypeDayMonthDay = "",
                    RunOnMonday = "",
                    RunOnTuesday = "",
                    RunOnWednesday = "",
                    RunOnThursday = "",
                    RunOnFriday = "",
                    RunOnSaturday = "",
                    RunOnSunday = ""))
})
