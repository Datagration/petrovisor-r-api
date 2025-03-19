##### DATA CONNECTION #####
context("Data connection instanciation and conversion to list")

test_that("Data connection instanciation and conversion works",{
  dc <- DataConnection$new(name = "MyDataConnection",
                           connectionType = "MyType",
                           settings = "MySettings",
                           isLocked = TRUE,
                           user = "MyUser",
                           isFavorite = TRUE,
                           labels = list("label1", "label2"))

  listed <- dc$toList()

  expect_equal(listed,
               list(Name = "MyDataConnection",
                    ConnectionType = "MyType",
                    Settings = "MySettings",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Data Connection instanciation and conversion works
          (empty constructor)",{
  dc <- DataConnection$new()

  listed <- dc$toList()

  expect_equal(listed,
               list(Name = "",
                    ConnectionType = "",
                    Settings = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})

##### DATA SOURCE #####
context("Data source instanciation and conversion to list")

test_that("Data source instanciation and conversion works",{
  mapping1 <- DataMapping$new(sourceRef = "MySourceRef1",
                              entityName = "Well01",
                              signalName = "oil rate",
                              unitName = "m3/d")

  mapping2 <- DataMapping$new(sourceRef = "MySourceRef2",
                              entityName = "Well02",
                              signalName = "oil rate",
                              unitName = "m3/d")

  ds <- DataSource$new(name = "MyDataConnection",
                       dataConnectionName = "MyDataConnection",
                       connectionType = "MyConnectionType",
                       cultureName = "MyCultureName",
                       settings = "MySettings",
                       dataKind = "Static",
                       extraCategory = "MyCategory",
                       isStackedEntities = TRUE,
                       isStackedSignals = TRUE,
                       isNewEntitiesTracked = FALSE,
                       createNewEntitiesTypeName = "Well",
                       createNewEntitiesRank = 3,
                       isNewMappingsCreated = TRUE,
                       addNewEntitiesToHierarchies = list(Prop1 = "Value1",
                                                          Prop2 = "Value2"),
                       importTagEntriesQuery = "MyQuery2",
                       entityAliasQuery = "MyQuery2",
                       stepColumn = "1",
                       stackedEntitiesColumn = "2",
                       stackedSignalsColumn = "3",
                       dataMappings = list(mapping1, mapping2),
                       isLocked = TRUE,
                       user = "MyUser",
                       isFavorite = TRUE,
                       labels = list("label1", "label2"))

  listed <- ds$toList()

  expect_equal(listed,
               list(Name = "MyDataConnection",
                    DataConnectionName = "MyDataConnection",
                    ConnectionType = "MyConnectionType",
                    CultureName = "MyCultureName",
                    Settings = "MySettings",
                    DataKind = "Static",
                    ExtraCategory = "MyCategory",
                    IsStackedEntities = TRUE,
                    IsStackedSignals = TRUE,
                    IsNewEntitiesTracked = FALSE,
                    CreateNewEntities = list(Name = "Well",
                                             Rank = 3),
                    IsNewMappingsCreated = TRUE,
                    AddNewEntitiesToHierarchies = list(Prop1 = "Value1",
                                                       Prop2 = "Value2"),
                    ImportTagEntriesQuery = "MyQuery2",
                    EntityAliasQuery = "MyQuery2",
                    StepColumn = "1",
                    StackedEntitiesColumn = "2",
                    StackedSignalsColumn = "3",
                    DataMappings = list(mapping1$toList(), mapping2$toList()),
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Data source instanciation and conversion works
          (empty constructor)",{
  ds <- DataSource$new()

  listed <- ds$toList()

  expect_equal(listed,
               list(Name = "",
                    DataConnectionName = "",
                    ConnectionType = "",
                    CultureName = "",
                    Settings = "",
                    DataKind = "",
                    ExtraCategory = "",
                    IsStackedEntities = "",
                    IsStackedSignals = "",
                    IsNewEntitiesTracked = "",
                    CreateNewEntities = list(Name = "",
                                             Rank = ""),
                    IsNewMappingsCreated = "",
                    AddNewEntitiesToHierarchies = "",
                    ImportTagEntriesQuery = "",
                    EntityAliasQuery = "",
                    StepColumn = "",
                    StackedEntitiesColumn = "",
                    StackedSignalsColumn = "",
                    DataMappings = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})

##### DATA MAPPING #####
context("Data mapping instanciation and conversion to list")

test_that("Data mapping instanciation and conversion works",{
  dm <- DataMapping$new(sourceRef = "MySourceRef",
                        entityName = "Well01",
                        signalName = "oil rate",
                        unitName = "m3/d")

  listed <- dm$toList()

  expect_equal(listed,
               list(SourceRef = "MySourceRef",
                    EntityName = "Well01",
                    SignalName = "oil rate",
                    UnitName = "m3/d"))
})

test_that("Data mapping instanciation and conversion works
          (empty constructor)",{
  dm <- DataMapping$new()

  listed <- dm$toList()

  expect_equal(listed,
               list(SourceRef = "",
                    EntityName = "",
                    SignalName = "",
                    UnitName = ""))
})

##### DATA INTEGRATION SET #####
context("Data integration set instanciation and conversion to list")

test_that("Data integration set instanciation and conversion works",{
  dis <- DataIntegrationSet$new(name = "MyIntegrationSet",
                                overwriteExistingData = FALSE,
                                trackNewEntities = TRUE,
                                dataSourceNames = list("source1", "source2"),
                                cleansingScriptNames = list("script1",
                                                            "script2"),
                                timeStep = "Daily",
                                depthStep = "Every foot",
                                timeStart = "2020-02-01T00:00:00.000Z",
                                timeEnd = "2020-02-01T00:00:00.000Z",
                                depthStart = 0,
                                depthEnd = 100,
                                isLocked = TRUE,
                                user = "MyUser",
                                isFavorite = TRUE,
                                labels = list("label1", "label2"))

  listed <- dis$toList()

  expect_equal(listed,
               list(Name = "MyIntegrationSet",
                    OverwriteExistingData = FALSE,
                    TrackNewEntities = TRUE,
                    DataSources = list("source1", "source2"),
                    CleansingScripts = list("script1",
                                                "script2"),
                    TimeStep = "Daily",
                    DepthStep = "Every foot",
                    TimeStart = "2020-02-01T00:00:00.000Z",
                    TimeEnd = "2020-02-01T00:00:00.000Z",
                    DepthStart = 0,
                    DepthEnd = 100,
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Data integration set instanciation and conversion works
          (empty constructor)",{
  dis <- DataIntegrationSet$new()

  listed <- dis$toList()

  expect_equal(listed,
               list(Name = "",
                    OverwriteExistingData = "",
                    TrackNewEntities = "",
                    DataSources = "",
                    CleansingScripts = "",
                    TimeStep = "EveryMinute",
                    DepthStep = "Every tenth of meter",
                    TimeStart = "",
                    TimeEnd = "",
                    DepthStart = "",
                    DepthEnd = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
