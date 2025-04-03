library("R6")

#' @title Context
#'
#' @description Class representing a PetroVisor context object.
#'
#' @export Context
#'
#' @field name The name of the context.
#' @field entity_set The context's entity set (object of class EntitySet).
#' @field scope The context's scope (object of class Scope).
#' @field hierarchy (Optional) The hierarchy used for automatic aggregation
#'   (Object of type Hierarchy).
#' @field loading_scenario_name (Optional) The name of the loading scenario.
#' @field saving_scenario_name (Optional) The name of the saving scenario.
#' @field scenario_data_only (Optional) Whether to load data from the specified
#'   loading scenario only. If \code{FALSE}, data will be merged with workspace
#'   data.
#' @field formula The context's definition as string (P# syntax).
#' @field description The description of the item.
#' @field labels A list of strings holding the labels of the context.
#' @examples
#' \dontrun{
#' Context$new(name = "MyContext",
#'           entity_set = EntitySet$new(name = "MyEntities",
#'               entities = c(Entity$new(name = "Well1",
#'                                       entityTypeName = "Well",
#'                                       alias = "WellAlias1"),
#'                            Entity$new(name = "Well2",
#'                                       entityTypeName = "Well",
#'                                       alias = "WellAlias2")),
#'           scope = Scope$new(name = "MyScope",
#'                             start = "2020-01-01T00:00:00.000Z",
#'                             end = "2020-03-01T00:00:00.000Z",
#'                             timeIncrement = "Daily"))
#'}
Context <- R6Class("Context",
  public = list(
    name = NULL,
    entity_set = NULL,
    scope = NULL,
    hierarchy = NULL,
    loading_scenario_name = NULL,
    saving_scenario_name = NULL,
    scenario_data_only = NULL,
    formula = NULL,
    description = NULL,
    labels = NULL,

    #' @description Create a new Context instance.
    #'
    #' @param name The name of the context.
    #' @param entity_set The context's entity set (object of class EntitySet).
    #' @param scope The context's scope (object of class Scope).
    #' @param hierarchy (Optional) The hierarchy used for automatic aggregation
    #'   (Object of type Hierarchy).
    #' @param loading_scenario_name (Optional) The name of the loading scenario.
    #' @param saving_scenario_name (Optional) The name of the saving scenario.
    #' @param scenario_data_only (Optional) Whether to load data from the
    #'   specified loading scenario only. If \code{FALSE}, data will be merged
    #'   with workspace data.
    #' @param formula The context's definition as string (P# syntax).
    #' @param description The description of the item.
    #' @param labels A list of strings holding the labels of the context.
    initialize = function(name = NULL,
                          entity_set = NULL,
                          scope = NULL,
                          hierarchy = NULL,
                          loading_scenario_name = NULL,
                          saving_scenario_name = NULL,
                          scenario_data_only = FALSE,
                          formula = NULL,
                          description = NULL,
                          labels = list()) {
      self$name <- name
      self$entity_set <- entity_set
      self$scope <- scope
      self$hierarchy <- hierarchy
      self$loading_scenario_name <- loading_scenario_name
      self$saving_scenario_name <- saving_scenario_name
      self$scenario_data_only <- scenario_data_only
      self$formula <- formula
      self$description <- description
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        EntitySet = if (is.null(self$entity_set)) {
          ""
        } else {
          self$entity_set$toList()
        },
        Scope = if (is.null(self$scope)) "" else self$scope$toList(),
        Hierarchy = if (is.null(self$hierarchy)) {
          ""
        } else {
          self$hierarchy$toList()
        },
        LoadScenarioName = if (is.null(self$loading_scenario_name)) {
          ""
        } else {
          self$loading_scenario_name
        },
        ScenarioDataOnly = self$scenario_data_only,
        SavingScenarioName = if (is.null(self$saving_scenario_name)) {
          ""
        } else {
          self$saving_scenario_name
        },
        Formula = if (is.null(self$formula)) "" else self$formula,
        Description = if (is.null(self$description)) "" else self$description,
        Labels = self$labels
      )
      return(dl)
    }
  )
)
