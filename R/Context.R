library("R6")

#' @title Context
#'
#' @description Class representing a PetroVisor context object.
#'
#' @export Context
#'
#' @field name The name of the context.
#' @field entity_set The context's [EntitySet] defining which entities to
#'   include.
#' @field scope The context's [Scope] defining the time or depth range.
#' @field hierarchy (Optional) The [Hierarchy] used for automatic aggregation.
#' @field loading_scenario_name (Optional) The name of the loading scenario.
#' @field saving_scenario_name (Optional) The name of the saving scenario.
#' @field scenario_data_only (Optional) Whether to load data from the specified
#'   loading scenario only. If \code{FALSE}, data will be merged with workspace
#'   data.
#' @field formula The context's definition as string (P# syntax).
#' @field description The description of the item.
#' @field labels A list of strings holding the labels of the context.
#'
#' @seealso
#' * [Scope] for scope definitions
#' * [EntitySet] for entity set definitions
#' * [Hierarchy] for hierarchy definitions
#' * [RepositoryService] for loading and saving contexts
#' * [DataServices] for using contexts in data queries
#' * `vignette("working-with-data")` for data examples
#'
#' @examples
#' \dontrun{
#' Context$new(name = "MyContext",
#'           entity_set = EntitySet$new(name = "MyEntities",
#'               entities = c(Entity$new(name = "Well1",
#'                                       entity_type_name = "Well",
#'                                       alias = "WellAlias1"),
#'                            Entity$new(name = "Well2",
#'                                       entity_type_name = "Well",
#'                                       alias = "WellAlias2"))),
#'           scope = Scope$new(name = "MyScope",
#'                             start = "2020-01-01T00:00:00.000Z",
#'                             end = "2020-03-01T00:00:00.000Z",
#'                             time_increment = "Daily"))
#' }
Context <- R6Class("Context", # nolint: object_name_linter
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
    #' @param entity_set The context's [EntitySet] defining which entities to
    #'   include.
    #' @param scope The context's [Scope] defining the time or depth range.
    #' @param hierarchy (Optional) The [Hierarchy] used for automatic
    #'   aggregation.
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
    #' by the [RepositoryService] to convert the objects to lists and then
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
