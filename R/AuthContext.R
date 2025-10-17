library("R6")

#' @title AuthContext
#'
#' @description Singleton class for managing authentication context across the
#'   application.
#'
#' @details This class provides a centralized authentication context that can be
#'   accessed from anywhere in the application. It stores authentication
#'   credentials and provides methods to safely access them. The class
#'   implements the singleton pattern to ensure a single source of truth for
#'   authentication state.
#'
#' @export AuthContext
#'
#' @seealso
#' * [ServiceProvider] for creating authenticated sessions
#' * [AuthenticationService] for authentication methods
#' * [get_auth_context()] for accessing the singleton instance
#' * [with_auth_context()] for executing code with authentication
#' * [require_authentication()] for checking authentication status
#' * `vignette("authentication")` for authentication examples
#' * `vignette("getting-started")` for setup guide
#'
#' @examples
#' \dontrun{
#' # Get the singleton instance
#' auth <- get_auth_context()
#'
#' # Set authentication (typically done by ServiceProvider)
#' auth$set_auth(
#'   token = "your_access_token",
#'   token_type = "Bearer",
#'   workspace_data_url = "https://api.example.com/workspace/test/",
#'   user = "username",
#'   workspace = "workspace_name"
#' )
#'
#' # Check if authenticated
#' if (auth$is_authenticated()) {
#'   # Get auth information
#'   auth_info <- auth$get_auth()
#'
#'   # Get formatted authorization header
#'   header <- auth$get_auth_header()
#' }
#'
#' # Clear authentication when done
#' auth$clear_auth()
#' }
AuthContext <- R6Class("AuthContext", # nolint: object_name_linter
  public = list(
    #' @description Initialize the AuthContext. This should not be called
    #'   directly. Use AuthContext$get_instance() or get_auth_context() instead.
    initialize = function() {
      # Singleton instance is managed globally, no need for instance checks here
    },

    #' @description Set authentication credentials
    #' @param token The access token
    #' @param token_type The token type (e.g., "Bearer")
    #' @param workspace_data_url The workspace-specific API URL
    #' @param user Optional user information
    #' @param workspace Optional workspace name
    set_auth = function(token,
                        token_type,
                        workspace_data_url,
                        user = NULL,
                        workspace = NULL) {
      if (is.null(token) ||
            is.null(token_type) ||
            is.null(workspace_data_url)) {
        stop("token, token_type, and workspace_data_url are required")
      }

      private$token <- token
      private$token_type <- token_type
      private$workspace_data_url <- workspace_data_url
      private$user <- user
      private$workspace <- workspace
      private$is_auth_set <- TRUE

      # Optional: Log authentication event (without sensitive data)
      if (!is.null(user)) {
        message(paste("Authentication context set for user:", user))
      }

      invisible(self)
    },

    #' @description Get authentication information
    #' @return List containing auth details
    get_auth = function() {
      if (!private$is_auth_set) {
        stop("No authentication context available. Please authenticate first.")
      }

      list(
        token = private$token,
        token_type = private$token_type,
        workspace_data_url = private$workspace_data_url,
        user = private$user,
        workspace = private$workspace
      )
    },

    #' @description Get formatted authorization header
    #' @return Character string for Authorization header
    get_auth_header = function() {
      auth <- self$get_auth()
      paste(auth$token_type, auth$token)
    },

    #' @description Check if authenticated
    #' @return Logical indicating if auth is available
    is_authenticated = function() {
      private$is_auth_set && !is.null(private$token)
    },

    #' @description Clear authentication (logout)
    clear_auth = function() {
      private$token <- NULL
      private$token_type <- NULL
      private$workspace_data_url <- NULL
      private$user <- NULL
      private$workspace <- NULL
      private$is_auth_set <- FALSE
      message("Authentication context cleared")

      invisible(self)
    },

    #' @description Update token (for token refresh scenarios)
    #' @param new_token The new access token
    #' @param new_token_type Optional new token type
    refresh_token = function(new_token, new_token_type = NULL) {
      if (!private$is_auth_set) {
        stop("Cannot refresh token: not authenticated")
      }

      if (is.null(new_token)) {
        stop("new_token is required")
      }

      private$token <- new_token
      if (!is.null(new_token_type)) {
        private$token_type <- new_token_type
      }
      message("Token refreshed successfully")

      invisible(self)
    },

    #' @description Get current user information
    #' @return Character string with username or NULL if not set
    get_user = function() {
      if (!private$is_auth_set) {
        return(NULL)
      }
      private$user
    },

    #' @description Get current workspace information
    #' @return Character string with workspace name or NULL if not set
    get_workspace = function() {
      if (!private$is_auth_set) {
        return(NULL)
      }
      private$workspace
    },

    #' @description Get workspace data URL
    #' @return Character string with workspace data URL
    get_workspace_url = function() {
      if (!private$is_auth_set) {
        stop("No authentication context available")
      }
      private$workspace_data_url
    },

    #' @description Check if authentication context has specific user
    #' @param username Username to check
    #' @return Logical indicating if current user matches
    is_user = function(username) {
      if (!private$is_auth_set || is.null(private$user)) {
        return(FALSE)
      }
      identical(private$user, username)
    },

    #' @description Check if authentication context has specific workspace
    #' @param workspace_name Workspace name to check
    #' @return Logical indicating if current workspace matches
    is_workspace = function(workspace_name) {
      if (!private$is_auth_set || is.null(private$workspace)) {
        return(FALSE)
      }
      identical(private$workspace, workspace_name)
    }
  ),

  private = list(
    token = NULL,
    token_type = NULL,
    workspace_data_url = NULL,
    user = NULL,
    workspace = NULL,
    is_auth_set = FALSE
  )
)

# Environment to store the singleton instance (avoids locked binding issues)
.auth_context_env <- new.env(parent = emptyenv())

# Static method to get singleton instance
AuthContext$get_instance <- function() { # nolint: object_name_linter
  if (is.null(.auth_context_env$instance)) {
    .auth_context_env$instance <- AuthContext$new()
  }
  .auth_context_env$instance
}

#' @title Get Authentication Context
#'
#' @description Get the singleton AuthContext instance
#'
#' @details This function provides convenient access to the singleton
#'   AuthContext instance. It's the recommended way to access authentication
#'   context throughout the application.
#'
#' @return AuthContext singleton instance
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get authentication context
#' auth <- get_auth_context()
#'
#' # Check if authenticated
#' if (auth$is_authenticated()) {
#'   user <- auth$get_user()
#'   workspace <- auth$get_workspace()
#' }
#' }
get_auth_context <- function() {
  AuthContext$get_instance()
}

#' @title Require Authentication
#'
#' @description Helper function that throws an error if not authenticated
#'
#' @details This is a convenience function that can be called at the start of
#'   functions that require authentication to ensure the user is properly
#'   authenticated before proceeding.
#'
#' @param message Optional custom error message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # At the start of a function that requires auth
#' some_protected_function <- function() {
#'   require_authentication()
#'
#'   # Proceed with authenticated operations...
#' }
#' }
require_authentication <- function(
  message = "This operation requires authentication"
) {
  if (!get_auth_context()$is_authenticated()) {
    stop(message)
  }
  invisible(TRUE)
}

#' @title With Authentication Context
#'
#' @description Execute code block with temporary authentication context
#'
#' @details This function allows you to temporarily set an authentication
#'   context for testing or special operations, then restore the previous
#'   context.
#'
#' @param token The access token
#' @param token_type The token type
#' @param workspace_data_url The workspace URL
#' @param code Code block to execute
#' @param user Optional user name
#' @param workspace Optional workspace name
#'
#' @return Result of the code block execution
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Execute code with temporary auth context
#' result <- with_auth_context(
#'   token = "test-token",
#'   token_type = "Bearer",
#'   workspace_data_url = "https://test.api.com/",
#'   user = "test-user",
#'   {
#'     # Code that needs authentication
#'     some_authenticated_operation()
#'   }
#' )
#' }
with_auth_context <- function(token,
                              token_type,
                              workspace_data_url,
                              code,
                              user = NULL,
                              workspace = NULL) {
  auth <- get_auth_context()

  # Save current state
  was_authenticated <- auth$is_authenticated()
  old_auth <- if (was_authenticated) auth$get_auth() else NULL

  # Set temporary auth
  auth$set_auth(token, token_type, workspace_data_url, user, workspace)

  # Execute code and handle cleanup
  tryCatch({
    force(code)
  }, finally = {
    # Restore previous state
    if (was_authenticated) {
      auth$set_auth(
        token = old_auth$token,
        token_type = old_auth$token_type,
        workspace_data_url = old_auth$workspace_data_url,
        user = old_auth$user,
        workspace = old_auth$workspace
      )
    } else {
      auth$clear_auth()
    }
  })
}
