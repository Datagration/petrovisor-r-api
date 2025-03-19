#' @title AuthenticationService
#' @name AuthenticationService
#'
#' @description Provides methods for authenticating with the PetroVisor API.
#'
#' @details This class includes methods to obtain access tokens using various
#' authentication mechanisms such as API keys, user credentials, and refresh tokens.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an instance of the AuthenticationService
#' auth_service <- AuthenticationService$new()
#'
#' # Get access token using API key
#' token <- auth_service$get_access_token(
#'   key = "your_api_key",
#'   discovery_url = "https://example.com"
#' )
#'
#' # Get access token using username and password
#' token <- auth_service$get_access_token(
#'   username = "user",
#'   password = "pass",
#'   discovery_url = "https://example.com"
#' )
#'
#' # Get access token using refresh token
#' token <- auth_service$get_access_token(
#'   refresh_token = "your_refresh_token",
#'   discovery_url = "https://example.com"
#' )
#' }

library(httr)
library(jsonlite)
library(base64enc)

AuthenticationService <- R6::R6Class("AuthenticationService",
  public = list(

    # Method to get access token
    get_access_token = function(key = "",
                                username = "",
                                password = "",
                                refresh_token = "",
                                discovery_url = "",
                                token_endpoint = "") {
      access_response <- NULL
      if (key != "") {
        access_response <- self$get_access_token_from_key(
          key,
          discovery_url,
          token_endpoint
        )
      }
      if (is.null(access_response) && username != "" && password != "") {
        access_response <- self$get_access_token_from_credentials(
          username,
          password,
          discovery_url,
          token_endpoint
        )
      }
      if (is.null(access_response) && refresh_token != "") {
        access_response <- self$get_access_token_from_refresh_token(
          refresh_token,
          discovery_url,
          token_endpoint
        )
      }

      return(access_response)
    },

    # Get access token from key
    get_access_token_from_key = function(key, discovery_url = "",
                                         token_endpoint = "") {
      credentials <- self$get_credentials_from_key(key)
      username <- credentials$username
      password <- credentials$password
      return(self$get_access_token_from_credentials(
        username,
        password,
        discovery_url,
        token_endpoint
      ))
    },

    # Get access token from credentials
    get_access_token_from_credentials = function(username,
                                                 password,
                                                 discovery_url = "",
                                                 token_endpoint = "") {
      if (token_endpoint == "") {
        token_endpoint <- self$get_token_endpoint(discovery_url)
      }

      grant_type <- "password"
      client_id <- "petrovisor.python.client"
      scope <- "petrovisor.api"

      response <- POST(
        url = token_endpoint,
        body = list(
          username = username,
          password = password,
          client_id = client_id,
          grant_type = grant_type,
          scope = scope
        ),
        encode = "form",
        content_type("application/x-www-form-urlencoded")
      )

      access_response <- content(response, "text")
      return(fromJSON(access_response))
    },

    # Get access token from refresh token
    get_access_token_from_refresh_token = function(refresh_token,
                                                   discovery_url = "",
                                                   token_endpoint = "") {
      if (token_endpoint == "") {
        token_endpoint <- self$get_token_endpoint(discovery_url)
      }

      grant_type <- "refresh_token"
      client_id <- "petrovisor.python.client" # there is not R client yet
      scope <- "petrovisor.api"

      response <- POST(
        url = token_endpoint,
        body = list(
          refresh_token = refresh_token,
          client_id = client_id,
          grant_type = grant_type,
          scope = scope
        ),
        encode = "form",
        content_type("application/x-www-form-urlencoded")
      )

      access_response <- content(response, "text")
      return(fromJSON(access_response))
    },

    # Get credentials from key
    get_credentials_from_key = function(key) {
      # Decode the key (base64 decoding)
      auth <- self$decode_base64(key)
      credentials <- strsplit(auth, ":")[[1]]
      list(
        username = credentials[1],
        password = paste(credentials[-1], collapse = ":")
      )
    },

    # Decode base64 message
    decode_base64 = function(base64_message) {
      raw_msg <- base64enc::base64decode(base64_message)
      rawToChar(raw_msg)
    },

    # Get token endpoint
    get_token_endpoint = function(discovery_url) {
      discovery_doc <- self$get_discovery_document(discovery_url)
      if (!is.null(discovery_doc$token_endpoint)) {
        return(discovery_doc$token_endpoint)
      }
      stop("Token endpoint not found.")
    },

    # Get WebApi endpoint
    get_web_api_endpoint = function(discovery_url) {
      discovery_doc <- self$get_discovery_document(discovery_url)
      petrovisor_webapi_endpoint <- discovery_doc$petrovisor_webapi_endpoint
      if (!is.null(discovery_doc$petrovisor_webapi_endpoint)) {
        return(
          paste0(petrovisor_webapi_endpoint, "/PetroVisor/API")
        )
      }
      stop("Web API endpoint not found.")
    },

    # Get discovery document
    get_discovery_document = function(discovery_url) {
      library(httr)
      library(jsonlite)
      library(base64enc)
      url <- paste0(discovery_url, ".well-known/openid-configuration")
      response <- httr::GET(url)
      discovery_doc <- content(response, "text")
      return(jsonlite::fromJSON(discovery_doc))
    }
  )
)
