library("R6")

#' @title Scenario
#'
#' @description Class representing a PetroVisor scenario object.
#'
#' @export Scenario
#'
#' @field name The name of the scenario
#' @field configuration_settings A list holding configuration items associated
#'   with the scenario. Objects in this list are of type ConfigurationSetting.
#' @field description The description of the item.
#' @field labels A list of strings holding the labels of the item.
#'
#' @examples
#' \dontrun{
#' Scenario$new(
#'   name = "Test R Scenario",
#'   description = "Test Scenario")
#' }
Scenario <- R6Class("Scenario",
  public = list(
    name = NULL,
    configuration_settings = NULL,
    description = NULL,
    labels = FALSE,

    #' @description Create a new Scenario instance.
    #'
    #' @param name The name of the scenario
    #' @param configuration_settings A list holding configuration items
    #'   associated with the scenario. Objects in this list are of type
    #'   ConfigurationSetting.
    #' @param description The description of the item.
    #' @param labels A list of strings holding the labels of the item.
    initialize = function(name = NULL,
                          configuration_settings = list(),
                          description = NULL,
                          labels = list()) {
      self$name <- name
      self$configuration_settings <- configuration_settings
      self$description <- description
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      # create list from list of configuration settings
      configuration_settings_list <- list()
      if (length(self$configuration_settings)) {
        for (i in seq_along(self$configuration_settings)){
          configuration_settings_list[[i]] <-
            self$configuration_settings[[i]]$toList()
        }
      }

      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        WorkspaceValues = configuration_settings_list,
        Description = if (is.null(self$description)) "" else self$description,
        Labels = self$labels
      )
      return(dl)
    }
  )
)
