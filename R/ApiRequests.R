library("R6")
library("httr")
library("jsonlite")

#' @title ApiRequests
#'
#' @description Performs REST API calls
#'
#' @export ApiRequests
#'
ApiRequests <- R6Class("ApiRequests",
  public = list(
    #' @description Create a new ApiRequests instance.
    initialize = function() { }
  ),
  private = list(
    #' description Issue a POST request to the api
    #'
    #' param body The body of the post request.
    #' param url The base url of the API.
    #' param route The route to use. E.g. Signal, Files, etc.
    #' param token_type The type of the used token.
    #' param token The token used for authenticating the request.
    #' param expect_data Whether to expect data being returned from the request.
    #' param query Named list of additional query parameters.
    post = function(body,
                    url,
                    route,
                    token_type,
                    token,
                    expect_data = FALSE,
                    query = NULL) {
      ret <- httr::POST(
        url = gsub(" ", "%20", paste0(url, route)),
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        query = query,
        httr::content_type_json(),
        httr::add_headers(
          Authorization = paste(token_type, token)
        )
      )

      if(expect_data) {
        httr::stop_for_status(ret)
        cont <- httr::content(ret, as = "text")
        return(jsonlite::fromJSON(cont))
      }

      httr::stop_for_status(ret)
    },

    #' description Upload a file using a POST request to the api
    #'
    #' param file The file to upload (path incl. file name).
    #' param url The base url of the API.
    #' param route The route to use. E.g. Signal, Files, etc.
    #' param token_type The type of the used token.
    #' param token The token used for authenticating the request.
    upload_file = function(file, url, route, token_type, token) {
      headers <- c(
        accept = "application/json",
        Authorization = paste(token_type, token),
        `Content-Type` = "multipart/form-data"
      )

      files <- list(
        file = httr::upload_file(file)
      )

      ret <- httr::POST(
        url = gsub(" ", "%20", paste0(url, route)),
        httr::add_headers(.headers = headers),
        body = files,
        encode = "multipart"
      )

      httr::stop_for_status(ret)
    },

    download_file = function(name, url, route, token_type, token) {
      ret <- httr::GET(
        url = gsub(" ", "%20", paste0(url, route, name)),
        httr::add_headers(
          Authorization = paste(token_type, token)
        )
      )

      httr::stop_for_status(ret)

      return(ret)
    },

    #' description Issue a GET request to the api
    #'
    #' param url The base url of the API.
    #' param route The route to use. E.g. Signal, Files, etc.
    #' param token_type The type of the used token.
    #' param token The token used for authenticating the request.
    #' param query Additional parameters for the query string.
    #' param parse_json Whether to parse the result of the request. Defaults to
    #'  \code{TRUE}. If set to \code{FALSE}, the raw content will be returned.
    #'
    #' returns The result of the GET request. Either as received, or parsed to
    #'  an object using \code{jsonlite::fromJSON}.
    get = function(url,
                   route,
                   token_type,
                   token,
                   query = NULL,
                   parse_json = TRUE) {

      ret <- httr::GET(
        url = gsub(" ", "%20", paste0(url, route, query)),
        httr::add_headers(
          Authorization = paste(token_type, token)
        )
      )

      httr::stop_for_status(ret)

      cont <- httr::content(ret, as = "text")

      if (parse_json) {
        return(jsonlite::fromJSON(cont))
      } else {
        return(cont)
      }
    },

    #' description Issue a DELETE request to the api
    #'
    #' param name Name of the item to delete.
    #' param url The base url of the API.
    #' param route The route to use. E.g. Signal, Files, etc.
    #' param token_type The type of the used token.
    #' param token The token used for authenticating the request.
    delete = function(name, url, route, token_type, token, query = NULL) {
      ret <- httr::DELETE(
        url = gsub(" ", "%20", paste0(url, route, name)),
        query = query,
        httr::add_headers(
          Authorization = paste(token_type, token)
        )
      )

      httr::stop_for_status(ret)
    },


    #' description Issue a PUT request to the api
    #'
    #' param body The body of the request.
    #' param url The base url of the API.
    #' param route The route to use. E.g. Signal, Files, etc.
    #' param token_type The type of the used token.
    #' param token The token used for authenticating the request.
    put = function(body, url, route, token_type, token) {
      ret <- httr::PUT(
        gsub(" ", "%20", paste0(url, route)),
        body =
          jsonlite::toJSON(body, auto_unbox = TRUE)
        ,
        httr::content_type_json(),
        httr::add_headers(
          Authorization = paste(token_type, token)
        )
      )
      httr::stop_for_status(ret)
    }
  )
)
