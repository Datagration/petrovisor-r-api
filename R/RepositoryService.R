# ToDo: add input checks
# ToDo: style according to style guide
# ToDo: add classes for signal, hierarchy, tag, unit etc. or find a way to implement this using lists or dfs
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
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
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
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
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
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )
      httr::stop_for_status(ret)
      cont <- jsonlite::fromJSON(httr::content(ret, as = "text"))

      # parse to object
      switch(type,
        Entity = return(Entity$new(
          name = cont$Name,
          entityTypeName = cont$EntityTypeName,
          alias = cont$Alias
        )),
        Tag = return(Tag$new(name = cont$Name, tagGroup = cont$TagGroup))
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
        body = gsub(
          "\\[",
          "",
          gsub(
            "\\]",
            "",
            jsonlite::toJSON(item$toList(), auto_unbox = TRUE)
          )
        ),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )
      httr::stop_for_status(ret)
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL,
    GetUrlType = function(type = c(
                            "Chart", "CleansingCalculation",
                            "CleansingScript", "ConfigurationSetting",
                            "Context", "CrossPlot",
                            "CustomWorkflowActivity", "DataConnection",
                            "DataIntegrationSet", "DataSource", "DCAFit",
                            "Entity", "EntitySet", "EntityType",
                            "EventCalculation", "Filter", "GeoDataGrid",
                            "Hierarchy", "MLModel", "PivotTable", "Plot",
                            "Polygon", "ProcessTemplate", "PSharpScript",
                            "RScript", "RWorkflowActivity", "Scope",
                            "Signal", "TableCalculation", "Tag",
                            "UnitMeasurement", "Unit", "UserSetting",
                            "VoronoiGrid", "Workflow",
                            "WorkflowSchedule"
                          )) {
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
    }
  )
)
