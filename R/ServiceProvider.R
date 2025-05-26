library("R6")
library("jsonlite")

#' @title ServiceProvider
#'
#' @description Provides access to all services provided through the web API.
#'
#' @details This class allows interaction with the PetroVisor API by providing
#'   access to various services such as logging, repository, data, and tag
#'   entries.
#'
#' @import R6
#' @import base64enc
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import lifecycle
#'
#' @export ServiceProvider
#'
#' @field url Full URL to the PetroVisor API (either with or without token).
#' @field data_url The base URL for data services.
#' @field workspace The currently used workspace.
#' @field user The current user.
#' @field client_token Access token for the API (optional, used if URL is
#'   provided).
#' @field workspace_data_url The URL for the workspace-specific data services.
#' @field logs Instance of class \code{LoggingService} wrapping all
#'   logging-related functionalities.
#' @field items Instance of class \code{RepositoryService} wrapping all
#'   functionality related to PetroVisor items (entities, signals, units, etc.).
#' @field data Instance of class \code{DataServices} wrapping all functionality
#'   related to data.
#' @field tag_entries Instance of class \code{TagEntriesService} wrapping all
#'   functionality related to tag entries.
#' @field files Instance of class \code{FileService} wrapping all functionality
#'   related to files.
#'
#' @examples
#' \dontrun{
#' # Create a new instance of the service provider using token
#' sp <- ServiceProvider$new(
#'   url = "https://identity.us1.petrovisor.com/PetroVisor/API/WorkspaceA/",
#'   client_token = "your_token_here"
#' )
#'
#' # Create a new instance of the service provider using username and password
#' sp <- ServiceProvider$new(
#'   url = "https://identity.us1.petrovisor.com",
#'   workspace = "WorkspaceA",
#'   user = "your_username",
#'   password = "your_password"
#' )
#' }
ServiceProvider <- R6Class("ServiceProvider",
  public = list(
    url = NULL,
    data_url = NULL,
    user = NULL,
    workspace = NULL,
    client_token = NULL,
    logs = NULL,
    items = NULL,
    data = NULL,
    tag_entries = NULL,
    files = NULL,
    workspace_data_url = NULL, # Define as a public field

    #' @description Create a new ServiceProvider instance.
    #' @param url Full URL to the PetroVisor API (if token is provided).
    #' @param workspace Name of the workspace to connect to.
    #' @param user Username for authentication.
    #' @param password Password for the given user.
    #' @param client_token Access token for authentication (optional).
    initialize = function(url = NULL,
                          workspace = NULL,
                          user = NULL,
                          password = NULL,
                          client_token = NULL) {
      # Handle case where URL and token are provided
      if (is.null(url)) {
        stop("Discovery URL must be provided.")
      }
      self$url <- url
      self$user <- user
      self$workspace <- workspace
      private$password <- password
      private$token <- client_token
      private$tokenType <- "Bearer"
      self$getDataUrl()
      if (!is.null(workspace)) {
        self$set_workspace_data_url(self$data_url, workspace)
      }
      if (!is.null(user) && !is.null(password)) {
        private$getToken()
      }
      # Initialize services
      self$items <- RepositoryService$new(
        self$workspace_data_url,
        private$tokenType,
        private$token
      )
      self$data <- DataServices$new(
        self$workspace_data_url,
        private$tokenType,
        private$token,
        self
      )
      self$tag_entries <- TagEntriesService$new(
        self$workspace_data_url,
        private$tokenType,
        private$token
      )
      self$logs <- LoggingService$new(
        self$workspace_data_url,
        private$tokenType,
        private$token
      )
      self$files <- FileService$new(
        self$workspace_data_url,
        private$tokenType,
        private$token
      )
    },

    #' @description Parse the mapped signal received from PetroVisor to a list
    #'  containing the name and unit of the mapped signal.
    #' @param mapped_signal The mapped signal (string) as received from
    #'  PetroVisor.
    parse_signal = function(mapped_signal) {
      # Split the signal string into name and unit
      parts <- strsplit(mapped_signal, " \\[")[[1]]
      name <- parts[1]
      unit <- substr(parts[2], 1, nchar(parts[2]) - 1) # Remove the trailing "]"
      return(list(Signal = name, Unit = unit))
    },

    #' @description Set the workspace-specific data URL.
    #' @param data_url The base URL for data services.
    #' @param workspace The name of the workspace.
    set_workspace_data_url = function(data_url, workspace) {
      encoded_workspace <- URLencode(workspace, reserved = TRUE)
      self$workspace_data_url <- paste0(data_url, "/", encoded_workspace, "/")
    },

    #' @description Retrieve the base data URL from the authentication service.
    getDataUrl = function() {
      tryCatch(
        {
          self$data_url <-
            AuthenticationService$new()$get_web_api_endpoint(self$url)
        },
        error = function(e) {
          stop("Failed to retrieve data URL. Error: ", e$message)
        }
      )
    }
  ),
  private = list(
    password = NULL,
    tokenType = NULL,
    token = NULL,

    getToken = function() {
      tryCatch(
        {
          discovery_url <- sub("/.*", "", self$url)
          authentication_service <- AuthenticationService$new()
          token_response <- authentication_service$get_access_token(
            username = self$user,
            password = private$password,
            discovery_url = self$url
          )

          private$tokenType <- token_response$token_type
          private$token <- token_response$access_token
        },
        error = function(e) {
          stop("Failed to retrieve token. Error: ", e$message)
        }
      )
    }
  )
)
