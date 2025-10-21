library("R6")
library("jsonlite")

# Define %||% operator for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

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
#' @field ml Instance of class \code{MlTrainingService} wrapping
#'   all functionality related to ML model training and prediction.
#'
#' @seealso
#' Service classes for interacting with PetroVisor:
#' * [AuthenticationService] for authentication methods
#' * [DataServices] for loading and saving data
#' * [RepositoryService] for managing items (entities, signals, units, etc.)
#' * [LoggingService] for logging operations
#' * [FileService] for file operations
#' * [MlTrainingService] for machine learning
#' * [TagEntriesService] for tag entry operations
#'
#' Authentication helpers:
#' * [get_auth_context()] for accessing the authentication context
#' * [with_auth_context()] for executing code with specific auth context
#' * [require_authentication()] for ensuring authentication
#'
#' Vignettes:
#' * `vignette("getting-started")` for an introduction to the package
#' * `vignette("authentication")` for authentication details
#' * `vignette("working-with-data")` for data operations
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
ServiceProvider <- R6Class( # nolint: object_name_linter
  "ServiceProvider",
  inherit = ApiRequests,
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
    ml = NULL,
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

      # Set the authentication context
      auth_context <- get_auth_context()
      auth_context$set_auth(
        token = private$token,
        token_type = private$tokenType,
        workspace_data_url = self$workspace_data_url,
        user = self$user,
        workspace = self$workspace
      )

      # Initialize services
      self$items <- RepositoryService$new()
      self$data <- DataServices$new(self)
      self$tag_entries <- TagEntriesService$new()
      self$logs <- LoggingService$new()
      self$files <- FileService$new()
      self$ml <- MlTrainingService$new()
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
    },

    #' @description Convert values from a source unit to a target unit
    #' @param x The value to convert. Either a single value, or a collection of
    #'   values.
    #' @param source_unit The name of the source unit.
    #' @param target_unit The name of the target unit.
    #'
    #' @returns Converted values. Either single number or collection of values.
    convert_unit = function(x, source_unit, target_unit) {
      #handle special units (see swagger)
      if (source_unit == " ") source_unit <- "_"
      if (source_unit == "%") source_unit <- "@"
      if (target_unit == " ") target_unit <- "_"
      if (target_unit == "%") target_unit <- "@"

      # handle forward slashes in units (replace with "%2F")
      source_unit <- gsub("/", "%2F", source_unit)
      target_unit <- gsub("/", "%2F", target_unit)

      if (is.null(x) || all(is.na(x))) {
        # Input is NULL, NA or consists entirely of NA values (including NaN)
        return(x)
      }

      if (is.numeric(x) && length(x) == 1) {
        # Input is a single numeric value
        return(super$get(self$workspace_data_url,
                         paste0("Units/",
                                source_unit,
                                "/Convert/",
                                target_unit,
                                "/",
                                x),
                         private$tokenType,
                         private$token))
      } else if ((is.vector(x) || is.list(x)) &&
                   all(sapply(x, function(el) is.numeric(el) || is.na(el)))) {
        # Input is a collection of numeric values (including NA)
        # replace NA with NaN
        x[is.na(x)] <- NaN
        return(super$post(x,
                          self$workspace_data_url,
                          paste0("Units/",
                                 source_unit,
                                 "/Convert/",
                                 target_unit),
                          private$tokenType,
                          private$token,
                          expect_data = TRUE))
      } else {
        stop(paste0("Error: Input must be a numeric value or a ",
                    "list/vector of numeric values (including NA)."))
      }
    },

    #' @description Send an email using the PetroVisor email configuration.
    #'
    #' @param address The recipient email address (single string).
    #' @param subject The subject of the email.
    #' @param body The body content of the email.
    #'
    #' @examples
    #' \dontrun{
    #'   # Send a simple email
    #'   sp$send_mail(
    #'     address = "example@domain.com",
    #'     subject = "Test Subject",
    #'     body = "This is a test email from PetroVisor R client."
    #'   )
    #' }
    send_mail = function(address, subject, body) {
      # Validate email address
      if (is.null(address) || !is.character(address) || length(address) != 1) {
        stop("Error: 'address' must be a single character string.")
      }

      # Check email format using regex
      email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
      if (!grepl(email_pattern, address, perl = TRUE)) {
        stop("Error: '", address, "' is not a valid email address format.")
      }

      query <- list(
        Address = address,
        Subject = subject,
        Body = body
      )

      super$get(url = paste0(self$data_url, "/"),
                route = "Configuration/Send/Mail",
                query = query,
                parse_json = FALSE)
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
