library("R6")

#' @title DataConnection
#'
#' @description Class representing a PetroVisor data connection object.
#'
#' @export DataConnection
#'
#' @field name The name of the data connection.
#' @field connectionType The type of the data connection.
#' @field settings The settings of the data connection.
#' @field isLocked This flag specifies whether the data connection is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the data connection belongs to.
#' @field isFavorite This flag specifies whether the data connection is marked
#'   as favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the data connection.
#' @examples
#' \dontrun{
#' DataConnection$new()
#'}
DataConnection <- R6Class("DataConnection",
  public = list(
    name = NULL,
    connectionType = NULL,
    settings = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new DataConnection instance.
    #'
    #' @param name The name of the data connection.
    #' @param connectionType The type of the data connection.
    #' @param settings The settings of the data connection.
    #' @param isLocked This flag specifies whether the data connection is
    #'   locked. Defaults to \code{FALSE}.
    #' @param user The user the data connection belongs to.
    #' @param isFavorite This flag specifies whether the data connection is
    #'   marked as favorite item, and thus shown in the favorites tab on the
    #'   home module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the data
    #'   connection.
    initialize = function(name = NULL,
                          connectionType = NULL,
                          settings = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$connectionType <- connectionType
      self$settings <- settings
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        ConnectionType = if(is.null(self$connectionType)) {
          ""
        } else {
          self$connectionType
        },
        Settings = if(is.null(self$settings)) "" else self$settings,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)

#' @title DataSource
#'
#' @description Class representing a PetroVisor data source object.
#'
#' @export DataSource
#'
#' @field name The name of the data source.
#' @field dataConnectionName The name of the associated data connection.
#' @field connectionType The type of the data connection.
#' @field cultureName The name of the culture settings.
#' @field settings The settings of the data source.
#' @field dataKind The type of the dat in the data source.
#' @field extraCategory A string stating an extra category.
#' @field isStackedEntities This flag specifies whether the source data
#'   contains entity information in a stacked column.
#' @field isStackedSignals This flag specifies whether the source data
#'   contains signal information in a stacked column.
#' @field isNewEntitiesTracked This flag specifies whether new entities are
#'   tracked in the log.
#' @field createNewEntitiesTypeName If new entities are added to the database
#'   automatically, this field gives the name of the entity type with which
#'   the new entities are created.
#' @field createNewEntitiesRank If new entities are added to the database
#'   automatically, this field gices the ranke of the entity type with which
#'   the new entities are created.
#' @field isNewMappingsCreated This flag specifies whether mappings for new
#'   entities are added to the source automatically.
#' @field addNewEntitiesToHierarchies ??
#' @field importTagEntriesQuery The query that is executed to add tag entries.
#' @field entityAliasQuery The query that is executed to retrieve aliases for
#'   entities.
#' @field stepColumn The index of the column that specifies the timestamp.
#' @field stackedEntitiesColumn The index of the column that specifies
#'   the entities.
#' @field stackedSignalsColumn The index of the column that specifies the
#'   signals.
#' @field dataMappings A list stating the data mappings. Each item of the list
#'   must be an instance of the class \code{DataMapping}.
#' @field isLocked This flag specifies whether the data source is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the data source belongs to.
#' @field isFavorite This flag specifies whether the data source is marked
#'   as favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the data source.
#' @examples
#' \dontrun{
#' DataSource$new()
#'}
DataSource <- R6Class("DataSource",
  public = list(
    name = NULL,
    dataConnectionName = NULL,
    connectionType = NULL,
    cultureName = NULL,
    settings = NULL,
    dataKind = NULL,
    extraCategory = NULL,
    isStackedEntities = NULL,
    isStackedSignals = NULL,
    isNewEntitiesTracked = NULL,
    createNewEntitiesTypeName = NULL,
    createNewEntitiesRank = NULL,
    isNewMappingsCreated = NULL,
    addNewEntitiesToHierarchies = NULL,
    importTagEntriesQuery = NULL,
    entityAliasQuery = NULL,
    stepColumn = NULL,
    stackedEntitiesColumn = NULL,
    stackedSignalsColumn = NULL,
    dataMappings = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new DataSource instance.
    #'
    #' @param name The name of the data source.
    #' @param dataConnectionName The name of the associated data connection.
    #' @param connectionType The type of the data connection.
    #' @param cultureName The name of the culture settings.
    #' @param settings The settings of the data source.
    #' @param dataKind The type of the dat in the data source.
    #' @param extraCategory A string stating an extra category.
    #' @param isStackedEntities This flag specifies whether the source data
    #'   contains entity information in a stacked column.
    #' @param isStackedSignals This flag specifies whether the source data
    #'   contains signal information in a stacked column.
    #' @param isNewEntitiesTracked This flag specifies whether new entities are
    #'   tracked in the log.
    #' @param createNewEntitiesTypeName If new entities are added to the
    #'   database automatically, this field gives the name of the entity type
    #'   with which the new entities are created.
    #' @param createNewEntitiesRank If new entities are added to the database
    #'   automatically, this field gices the ranke of the entity type with which
    #'   the new entities are created.
    #' @param isNewMappingsCreated This flag specifies whether mappings for new
    #'   entities are added to the source automatically.
    #' @param addNewEntitiesToHierarchies ??
    #' @param importTagEntriesQuery The query that is executed to add tag
    #'   entries.
    #' @param entityAliasQuery The query that is executed to retrieve aliases
    #'   for entities.
    #' @param stepColumn The index of the column that specifies the timestamp.
    #' @param stackedEntitiesColumn The index of the column that specifies
    #'   the entities.
    #' @param stackedSignalsColumn The index of the column that specifies the
    #'   signals.
    #' @param dataMappings A list stating the data mappings. Each item of the
    #'   list  must be an instance of the class \code{DataMapping}.
    #' @param isLocked This flag specifies whether the data source is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the data source belongs to.
    #' @param isFavorite This flag specifies whether the data source is marked
    #'   as favorite item, and thus shown in the favorites tab on the home
    #'   module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the data source.
    initialize = function(name = NULL,
                          dataConnectionName = NULL,
                          connectionType = NULL,
                          cultureName = NULL,
                          settings = NULL,
                          dataKind = NULL,
                          extraCategory = NULL,
                          isStackedEntities = NULL,
                          isStackedSignals = NULL,
                          isNewEntitiesTracked = NULL,
                          createNewEntitiesTypeName = NULL,
                          createNewEntitiesRank = NULL,
                          isNewMappingsCreated = NULL,
                          addNewEntitiesToHierarchies = NULL,
                          importTagEntriesQuery = NULL,
                          entityAliasQuery = NULL,
                          stepColumn = NULL,
                          stackedEntitiesColumn = NULL,
                          stackedSignalsColumn = NULL,
                          dataMappings = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$dataConnectionName <- dataConnectionName
      self$connectionType <- connectionType
      self$cultureName <- cultureName
      self$settings <- settings
      self$dataKind <- dataKind
      self$extraCategory <- extraCategory
      self$isStackedEntities <- isStackedEntities
      self$isStackedSignals <- isStackedSignals
      self$isNewEntitiesTracked <- isNewEntitiesTracked
      self$createNewEntitiesTypeName <- createNewEntitiesTypeName
      self$createNewEntitiesRank <- createNewEntitiesRank
      self$isNewMappingsCreated <- isNewMappingsCreated
      self$addNewEntitiesToHierarchies <- addNewEntitiesToHierarchies
      self$importTagEntriesQuery <- importTagEntriesQuery
      self$entityAliasQuery <- entityAliasQuery
      self$stepColumn <- stepColumn
      self$stackedEntitiesColumn <- stackedEntitiesColumn
      self$stackedSignalsColumn <- stackedSignalsColumn
      self$dataMappings <- dataMappings
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of dataMappings
      mappingsList <- list()
      if(!is.null(self$dataMappings)) {
        for (i in 1:length(self$dataMappings)){
          mappingsList[[i]] <- self$dataMappings[[i]]$toList()
        }
      } else {
        mappingsList[[1]] <- ""
      }

      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        DataConnectionName = if(is.null(self$dataConnectionName)) {
          ""
        } else {
          self$dataConnectionName
        },
        ConnectionType = if(is.null(self$connectionType)) {
          ""
        } else {
          self$connectionType
        },
        CultureName = if(is.null(self$cultureName)) "" else self$cultureName,
        Settings = if(is.null(self$settings)) "" else self$settings,
        DataKind = if(is.null(self$dataKind)) "" else self$dataKind,
        ExtraCategory = if(is.null(self$extraCategory)) {
          ""
        } else {
          self$extraCategory
        },
        IsStackedEntities = if(is.null(self$isStackedEntities)) {
          ""
        } else {
          self$isStackedEntities
        },
        IsStackedSignals = if(is.null(self$isStackedSignals)) {
          ""
        } else {
          self$isStackedSignals
        },
        IsNewEntitiesTracked = if(is.null(self$isNewEntitiesTracked)) {
          ""
        } else {
          self$isNewEntitiesTracked
        },
        CreateNewEntities = list(
          Name = if(is.null(self$createNewEntitiesTypeName)) {
            ""
          } else {
            self$createNewEntitiesTypeName
          },
          Rank = if(is.null(self$createNewEntitiesRank)) {
            ""
          } else {
            self$createNewEntitiesRank
          }
        ),
        IsNewMappingsCreated = if(is.null(self$isNewMappingsCreated)) {
          ""
        } else {
          self$isNewMappingsCreated
        },
        AddNewEntitiesToHierarchies =
          if(is.null(self$addNewEntitiesToHierarchies)) {
            ""
          } else {
            self$addNewEntitiesToHierarchies
          },
        ImportTagEntriesQuery =
          if(is.null(self$importTagEntriesQuery)) {
            ""
          } else {
            self$importTagEntriesQuery
          },
        EntityAliasQuery = if(is.null(self$entityAliasQuery)) {
          ""
        } else {
          self$entityAliasQuery
        },
        StepColumn = if(is.null(self$stepColumn)) "" else self$stepColumn,
        StackedEntitiesColumn = if(is.null(self$stackedEntitiesColumn)) {
          ""
        } else {
          self$stackedEntitiesColumn
        },
        StackedSignalsColumn = if(is.null(self$stackedSignalsColumn)) {
          ""
        } else {
          self$stackedSignalsColumn
        },
        DataMappings = if(is.null(self$dataMappings)) "" else mappingsList,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)

#' @title DataMapping
#'
#' @description Class representing a data mapping as used in data sources.
#'
#' @export DataMapping
#'
#' @field sourceRef The reference in the source.
#' @field entityName The name of the mapped entity.
#' @field signalName The name of the mapped signal.
#' @field unitName The name of the mapped unit.
#'
#' @examples
#' \dontrun{
#' DataMapping$new()
#'}
DataMapping <- R6Class("DataMapping",
  public = list(
    sourceRef = NULL,
    entityName = NULL,
    signalName = NULL,
    unitName = NULL,

    #' @description Create a new DataMapping instance.
    #'
    #' @param sourceRef The reference in the source.
    #' @param entityName The name of the mapped entity.
    #' @param signalName The name of the mapped signal.
    #' @param unitName The name of the mapped unit.
    initialize = function(sourceRef = NULL,
                          entityName = NULL,
                          signalName = NULL,
                          unitName = NULL){
      self$sourceRef <- sourceRef
      self$entityName <- entityName
      self$signalName <- signalName
      self$unitName <- unitName
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the Services to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        SourceRef = if(is.null(self$sourceRef)) "" else self$sourceRef,
        EntityName = if(is.null(self$entityName)) "" else self$entityName,
        SignalName = if(is.null(self$signalName)) "" else self$signalName,
        UnitName = if(is.null(self$unitName)) "" else self$unitName)
      return(dl)
    }
  )
)

#' @title DataIntegrationSet
#'
#' @description Class representing a data integration set.
#'
#' @export DataIntegrationSet
#'
#' @field name The name of the data integration set.
#' @field overwriteExistingData This flag specifies whether existing data
#'   shall be overwritten by data extracted from the source.
#' @field trackNewEntities This flag specifies whether new entity shall be
#'   tracked in the log.
#' @field dataSourceNames A list of all data sources of the data entegration
#'   set (names only).
#' @field cleansingScriptNames A list of all cleansing scripts applied to
#'   the data (names only).
#' @field timeStep The time step for the data integration. One of:
#'   \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
#'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
#'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}.
#' @field depthStep The depth step for the data integration. One of:
#'   \code{"Every tenth of meter"}, \code{"Every eighth of meter"},
#'   \code{"Every half of foot"}, \code{"Every foot"},
#'   \code{"Every half of meter"}, \code{"Every meter"}.
#' @field timeStart The start date for the data aquisition.
#' @field timeEnd The end date for the data aquisition.
#' @field depthStart The start depth for the data aquisition.
#' @field depthEnd The end depth for the data aquisition.
#' @field isLocked This flag specifies whether the data integration set is
#'   locked. Defaults to \code{FALSE}.
#' @field user The user the data integration set belongs to.
#' @field isFavorite This flag specifies whether the data integration set is
#'   marked as favorite item, and thus shown in the favorites tab on the home
#'   module in PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the data integration
#'   set.
#'
#' @examples
#' \dontrun{
#' DataIntegrationSet$new()
#'}
DataIntegrationSet <- R6Class("DataIntegrationSet",
  public = list(
    name = NULL,
    overwriteExistingData = NULL,
    trackNewEntities = NULL,
    dataSourceNames = NULL,
    cleansingScriptNames = NULL,
    timeStep = NULL,
    depthStep = NULL,
    timeStart = NULL,
    timeEnd = NULL,
    depthStart = NULL,
    depthEnd = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new DataIntegrationSet instance.
    #'
    #' @param name The name of the data integration set.
    #' @param overwriteExistingData This flag specifies whether existing data
    #'   shall be overwritten by data extracted from the source.
    #' @param trackNewEntities This flag specifies whether new entity shall be
    #'   tracked in the log.
    #' @param dataSourceNames A list of all data sources of the data entegration
    #'   set (names only).
    #' @param cleansingScriptNames A list of all cleansing scripts applied to
    #'   the data (names only).
    #' @param timeStep The time step for the data integration. One of:
    #'   \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
    #'   \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
    #'   \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}.
    #' @param depthStep The depth step for the data integration. One of:
    #'   \code{"Every tenth of meter"}, \code{"Every eighth of meter"},
    #'   \code{"Every half of foot"}, \code{"Every foot"},
    #'   \code{"Every half of meter"}, \code{"Every meter"}.
    #' @param timeStart The start date for the data aquisition.
    #' @param timeEnd The end date for the data aquisition.
    #' @param depthStart The start depth for the data aquisition.
    #' @param depthEnd The end depth for the data aquisition.
    #' @param isLocked This flag specifies whether the data integration set is
    #'   locked. Defaults to \code{FALSE}.
    #' @param user The user the data integration set belongs to.
    #' @param isFavorite This flag specifies whether the data integration set is
    #'   marked as favorite item, and thus shown in the favorites tab on the home
    #'   module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the data integration
    #'   set.
    initialize = function(name = NULL,
                          overwriteExistingData = NULL,
                          trackNewEntities = NULL,
                          dataSourceNames = NULL,
                          cleansingScriptNames = NULL,
                          timeStep = c("EveryMinute",
                                       "EveryFiveMinutes",
                                        "EveryFifteenMinutes",
                                        "Hourly",
                                        "Daily",
                                        "Monthly",
                                        "Quarterly",
                                        "Yearly"),
                          depthStep = c("Every tenth of meter",
                                        "Every eighth of meter",
                                        "Every half of foot",
                                        "Every foot",
                                        "Every half of meter",
                                        "Every meter"),
                          timeStart = NULL,
                          timeEnd = NULL,
                          depthStart = NULL,
                          depthEnd = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$overwriteExistingData <- overwriteExistingData
      self$trackNewEntities <- trackNewEntities
      self$dataSourceNames <- dataSourceNames
      self$cleansingScriptNames <- cleansingScriptNames
      self$timeStep <- match.arg(timeStep)
      self$depthStep <- match.arg(depthStep)
      self$timeStart <- timeStart
      self$timeEnd <- timeEnd
      self$depthStart <- depthStart
      self$depthEnd <- depthEnd
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the Services to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        OverwriteExistingData = if(is.null(self$overwriteExistingData)) {
          ""
        } else {
          self$overwriteExistingData
        },
        TrackNewEntities = if(is.null(self$trackNewEntities)) {
          ""
        } else {
          self$trackNewEntities
        },
        DataSources = if(is.null(self$dataSourceNames)) {
          ""
        } else {
          self$dataSourceNames
        },
        CleansingScripts = if(is.null(self$cleansingScriptNames)) {
          ""
        } else {
          self$cleansingScriptNames
        },
        TimeStep = if(is.null(self$timeStep)) "" else self$timeStep,
        DepthStep = if(is.null(self$depthStep)) "" else self$depthStep,
        TimeStart = if(is.null(self$timeStart)) "" else self$timeStart,
        TimeEnd = if(is.null(self$timeEnd)) "" else self$timeEnd,
        DepthStart = if(is.null(self$depthStart)) "" else self$depthStart,
        DepthEnd = if(is.null(self$depthEnd)) "" else self$depthEnd,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels)
      return(dl)
    }
  )
)
