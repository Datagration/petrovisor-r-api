# ToDo: add input checks
# ToDo: style according to style guide
# ToDo: add classes for signal, hierarchy, tag, unit etc. or find a way to
#  implement this using lists or dfs
# ToDo: add documentation

library("R6")
library("httr")

#' @title RepositoryService
#'
#' @description Provides access to PetroVisor item related functionalities.
#'
#' @details A new instance of this class will be created by the ServiceProvider
#' automatically.
#'
#' @export RepositoryService
#'
#' @examples \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")
#'
#' # get the names of all available entities
#' entityNames <- sp$repositoryService$GetNamesOfItems("Entity")
#'
#' # delete an item (delete the hierarchy with the name "test")
#' sp$repositoryService$DeleteItem("Hierarchy", "test")
#'
#' # get an item by name
#' well01 <- sp$repositoryService$GetItemByName("Well", "Well01")
#'
#' # add or edit an item
#' entity <- Entity$new(
#'   name = "TestWell01",
#'   entityTypeName = "Well",
#'   alias = "TestAlias01"
#' )
#' sp$repositoryService$AddOrEditItem("Entity", entity)
#' }
RepositoryService <- R6Class("RepositoryService",
  public = list(

    #' @description Create a new RepositoryService instance. This is done by the
    #' ServiceProvider automatically.
    #'
    #' @param url the URL for the API calls.
    #' @param tokenType the type of the issued token.
    #' @param token the issued token.
    initialize = function(url, tokenType, token) {
      private$url <- url
      private$tokenType <- tokenType
      private$token <- token
    },

    #' @description Get the names of all items of the given type available in
    #' PetroVisor.
    #'
    #' @param type The type of the item.
    #'
    #' @return A character array containing the names of the items.
    GetNamesOfItems = function(type) {
      # get the urlType
      urlType <- private$GetUrlType(type)
      print(urlType)
      # get item names
      ret <- httr::GET(
        paste0(
          private$url,
          urlType
        ),
        httr::add_headers(
          Authorization = paste(private$tokenType, private$token)
        )
      )
      httr::stop_for_status(ret)
      cont <- httr::content(ret, as = "text")
      return(jsonlite::fromJSON(cont))
    },

    #' @description Delete an item by name.
    #'
    #' @param type The type of the item.
    #' @param name Name of the item to delete.
    DeleteItem = function(type, name) {
      # get the urlType
      urlType <- private$GetUrlType(type)

      # delete item
      ret <- httr::DELETE(
        gsub(" ", "%20", paste0(private$url, urlType, "/", name)),
        httr::add_headers(
          Authorization = paste(private$tokenType, private$token)
        )
      )
      httr::stop_for_status(ret)
    },

    #' @description  Get an item by name.
    #'
    #' @param type The type of the item.
    #' @param name Name of the item to retrieve.
    #'
    #' @return An object of the specified type class.
    GetItemByName = function(type, name) {
      # get the urlType
      urlType <- private$GetUrlType(type)

      # retrieve the item
      ret <- httr::GET(
        gsub(" ", "%20", paste0(private$url, urlType, "/", name)),
        httr::add_headers(
          Authorization = paste(private$tokenType, private$token)
        )
      )
      httr::stop_for_status(ret)
      cont <- jsonlite::fromJSON(httr::content(ret, as = "text"))

      # parse to object
      switch(type,
        ConfigurationSetting = return(
          ConfigurationSetting$new(
            name = cont$Name,
            numeric_value = cont$NumericValue,
            string_value = cont$StringValue,
            list_value = cont$ListValue,
            enumeration_value = if (length(cont$EnumerationValue)) {
              cont$EnumerationValue
            } else {
              list()
            },
            dictionary_value = if (length(cont$DictionaryValue)) {
              cont$DictionaryValue
            } else {
              list()
            },
            value_type = cont$ValueType,
            unit_name = cont$UnitName,
            possible_values = cont$PossibleValues,
            is_system = cont$IsSystem,
            description = cont$Description,
            labels = cont$Labels
          )
        ),
        Context = return(
          Context$new(
            name = cont$Name,
            entity_set = private$GetEntitySetFromContent(cont$EntitySet),
            scope = private$GetScopeFromContent(cont$Scope),
            hierarchy = private$GetHierarchyFromContent(cont$Hierarchy),
            loading_scenario_name = cont$LoadScenarioName,
            saving_scenario_name = cont$SavingScenarioName,
            scenario_data_only = cont$ScenarioDataOnly,
            formula = cont$Formula,
            description = cont$Description,
            labels = cont$Labels
          )
        ),
        Entity = return(
          Entity$new(
            name = cont$Name,
            entity_type_name = cont$EntityTypeName,
            alias = cont$Alias,
            is_opportunity = cont$IsOpportunity
          )
        ),
        EntitySet = return(private$GetEntitySetFromContent(cont)),
        EntityType = return(
          EntityType$new(
            name = cont$Name,
            image = cont$Image
          )
        ),
        Hierarchy = return(private$GetHierarchyFromContent(cont)),
        Unit = return(
          Unit$new(
            name = cont$Name,
            measurement_name = cont$MeasurementName,
            factor = cont$Factor,
            summand = cont$Summand
          )
        ),
        UnitMeasurement = return(
          UnitMeasurement$new(
            name = cont$Name,
            canonical_unit_name = cont$CanonicalUnitName
          )
        ),
        Scenario = {
          cs_list <- list()
          if (length(cont$WorkspaceValues) > 0) {
            for (i in seq_len(nrow(cont$WorkspaceValues))) {
              cs_list[[i]] <- ConfigurationSetting$new(
                name = cont$WorkspaceValues[i, "Name"],
                numeric_value = cont$WorkspaceValues[i, "NumericValue"],
                string_value = cont$WorkspaceValues[i, "StringValue"],
                list_value =
                  if (is.null(cont$WorkspaceValues[i, "ListValue"])) {
                    list()
                  } else {
                    cont$WorkspaceValues[i, "ListValue"]
                  },
                enumeration_value =
                  if (is.null(cont$WorkspaceValues[i, "EnumerationValue"])) {
                    list()
                  } else {
                    cont$WorkspaceValues[i, "EnumerationValue"]
                  },
                dictionary_value =
                  if (is.null(cont$WorkspaceValues[i, "DictionaryValue"])) {
                    list()
                  } else {
                    cont$WorkspaceValues[i, "DictionaryValue"]
                  },
                value_type = cont$WorkspaceValues[i, "ValueType"],
                unit_name = cont$WorkspaceValues[i, "UnitName"],
                possible_values =
                  if (is.null(cont$WorkspaceValues[i, "PossibleValues"])) {
                    list()
                  } else {
                    cont$WorkspaceValues[i, "PossibleValues"]
                  },
                is_system = cont$WorkspaceValues[i, "IsSystem"],
                description = cont$WorkspaceValues[i, "Description"],
                labels =
                  if (is.null(cont$WorkspaceValues[i, "Labels"])) {
                    list()
                  } else {
                    cont$WorkspaceValues[i, "Labels"]
                  }
              )
            }
          }

          return(
            Scenario$new(
              name = cont$Name,
              configuration_settings = cs_list,
              description = cont$Description,
              labels = cont$Labels
            )
          )
        },
        Scope = return(private$GetScopeFromContent(cont)),
        Signal = return(
          Signal$new(
            name = cont$Name,
            short_name = cont$ShortName,
            measurement_name = cont$MeasurementName,
            storage_unit_name = cont$StorageUnitName,
            aggregation_type = cont$AggregationType,
            container_aggregation_type = cont$ContainerAggregationType,
            signal_type = cont$SignalType,
            default_color = cont$DefaultColor,
            default_line_type <- cont$DefaultLineType,
            setting_name <- cont$SettingName,
            labels <- cont$Labels,
            description <- cont$Description
          )
        ),
        Tag = return(Tag$new(name = cont$Name, tag_group = cont$TagGroup))
      )
    },

    #' @details Add or edit an item.
    #'
    #' @param type The type of the item.
    #' @param item Item to add or edit. Has to be an object of the respective
    #' class.
    AddOrEditItem = function(type, item) {
      # get the urlType
      urlType <- private$GetUrlType(type)

      # add or edit item
      ret <- httr::PUT(
        gsub(" ", "%20", paste0(private$url, urlType, "/", item$name)),
        body =
          jsonlite::toJSON(item$toList(), auto_unbox = TRUE)
        ,
        httr::content_type_json(),
        httr::add_headers(
          Authorization = paste(private$tokenType, private$token)
        )
      )
      httr::stop_for_status(ret)
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL,
    GetUrlType = function(type = c("Chart", "CleansingCalculation",
                                   "CleansingScript", "ConfigurationSetting",
                                   "Context", "CrossPlot",
                                   "CustomWorkflowActivity", "DataConnection",
                                   "DataIntegrationSet", "DataSource", "DCAFit",
                                   "Entity", "EntitySet", "EntityType",
                                   "EventCalculation", "Filter", "GeoDataGrid",
                                   "Hierarchy", "MLModel", "PivotTable", "Plot",
                                   "Polygon", "ProcessTemplate", "PSharpScript",
                                   "RScript", "RWorkflowActivity", "Scenario",
                                   "Scope", "Signal", "TableCalculation", "Tag",
                                   "UnitMeasurement", "Unit", "UserSetting",
                                   "VoronoiGrid", "Workflow",
                                   "WorkflowSchedule")) {
      # check input
      type <- match.arg(type)

      # get type for URL
      switch(type,
        Chart = return("Charts"),
        CleansingCalculation = return("CleansingCalculations"),
        CleansingScript = return("CleansingScripts"),
        ConfigurationSetting = return("ConfigurationSettings"),
        Context = return("Contexts"),
        CrossPlot = return("CrossPlots"),
        CustomWorkflowActivity = return("CustomWorkflowActivities"),
        DataConnection = return("DataConnections"),
        DataIntegrationSet = return("DataIntegrationSets"),
        DataSource = return("DataSources"),
        DCAFit = return("DCAFits"),
        Entity = return("Entities"),
        EntitySet = return("EntitySets"),
        EntityType = return("EntityTypes"),
        EventCalculation = return("EventCalculations"),
        Filter = return("Filters"),
        GeoDataGrid = return("GeoDataGrids"),
        Hierarchy = return("Hierarchies"),
        MlModel = return("MLModels"),
        PivotTable = return("PivotTables"),
        Plot = return("Plots"),
        Polygon = return("Polygons"),
        ProcessTemplate = return("ProcessTemplates"),
        PSharpScript = return("PSharpScripts"),
        RScript = return("RScripts"),
        RWorkflowActivity = return("RWorkflowActivities"),
        Scenario = return("Scenarios"),
        Scope = return("Scopes"),
        Signal = return("Signals"),
        TableCalculation = return("TableCalculations", ),
        Tag = return("Tags"),
        UnitMeasurement = return("UnitMeasurements"),
        Unit = return("Units"),
        UserSetting = return("UserSettings"),
        VoronoiGrid = return("VoronoiGrids"),
        Workflow = return("Workflows"),
        WorkflowSchedule = return("WorkflowSchedules")
      )
    },

    GetEntitySetFromContent = function(content) {
      # create entity list
      entity_list <- list()

      # map entities from content
      for (i in seq_len(nrow(content$Entities))) {
        entity_list[[i]] <- Entity$new(
          name = content$Entities[i, "Name"],
          alias = content$Entities[i, "Alias"],
          entity_type_name = content$Entities[i, "EntityTypeName"],
          is_opportunity = content$Entities[i, "IsOpportunity"]
        )
      }

      # return new entity set
      return(
        EntitySet$new(
          name = content$Name,
          entities = entity_list,
          formula = content$Formula,
          description = content$Description,
          labels = content$Labels
        )
      )
    },

    GetScopeFromContent = function(content) {
      return(
        Scope$new(
          name = content$Name,
          start = content$Start,
          end = content$End,
          time_increment = content$TimeIncrement,
          depth_increment = content$DepthIncrement,
          start_depth = content$StartDepth,
          end_depth = content$EndDepth,
          formula = content$Formula,
          description = content$Description,
          labels = content$Labels
        )
      )
    },

    GetHierarchyFromContent = function(content) {
      return(
        Hierarchy$new(
          name = content$Name,
          relationship = content$Relationship,
          is_time_dependent = content$IsTimeDependent,
          time_stamp = content$TimeStamp,
          description = content$Description,
          labels = content$Labels
        )
      )
    }
  )
)
