library("R6")
library("httr")
#' @title DataServices
#'
#' @description Provides access to logging functionality provided through the
#' web API.
#'
#' @details A new instance of this class will be created by the ServiceProvider
#' automatically.
#'
#' @export DataServices
#'
#' @examples \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")
#'
#' # load static numeric data
#' dataSetRequest1 <- DataSetRequest$new(
#'   entityName = "Well01",
#'   signalName = "surface x-coordinate",
#'   unitName = "m"
#' )
#' dataSetRequest2 <- DataSetRequest$new(
#'   entityName = "Well01",
#'   signalName = "surface y-coordinate",
#'   unitName = "m"
#' )
#' data <- sp$dataServices$GetStaticNumeric()$Load(list(
#'   dataSetRequest1,
#'   dataSetRequest2
#' ))
#'
#' # save static string data
#' staticStringData <- StaticData$new(
#'   signalName = "model",
#'   entiyName = "Well01",
#'   unitName = " ",
#'   data = "string one"
#' )
#' sp$dataServices$GetStaticString()$Save(list(staticStringData))
#' }
DataServices <- R6Class("DataServices",
  public = list(

    #' @description Create a new DataServices instance. This is done by the
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

    #' @description Get functionality related to static numeric data.
    #' @return A new instance of the class 'StaticNumeric'.
    GetStaticNumeric = function() {
      return(StaticNumeric$new(private$url, private$tokenType, private$token))
    },

    #' @description Get functionality related to static string data.
    #' @return A new instance of the class 'StaticString'.
    GetStaticString = function() {
      return(StaticString$new(private$url, private$tokenType, private$token))
    },

    #' @description Get functionality related to time-dependent numeric data.
    #' @return A new instance of the class 'TimeNumeric'.
    GetTimeNumeric = function() {
      return(TimeNumeric$new(private$url, private$tokenType, private$token))
    },

    #' @description Get functionality related to time-dependent string data.
    #' @return A new instance of the class 'TimeString'.
    GetTimeString = function() {
      return(TimeString$new(private$url, private$tokenType, private$token))
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL
  )
)

#' @title StaticNumeric
#'
#' @description Provides access to functionality related to static numeric data.
#'
#' @details A new instance of this class will be created by the DataServices
#' when calling the method 'GetStaticNumeric'.
#'
#' @export StaticNumeric
StaticNumeric <- R6Class("StaticNumeric",
  public = list(
    #' @description Create a new StaticNumeric object instance. This is done by
    #' the DataServices automatically if needed.
    #'
    #' @param url The URL for the API calls.
    #' @param tokenType The type of the issued token.
    #' @param token The issued token.
    initialize = function(url, tokenType, token) {
      private$url <- url
      private$tokenType <- tokenType
      private$token <- token
    },

    #' @description Delete the static numeric data specified by the requests.
    #'
    #' @param dataSetRequests A list of data set requests specifying which data
    #' shall be deleted. The items in the list must be objects of the class
    #' 'DataSetRequest'.
    Delete = function(dataSetRequests) {
      # create list from list of dataset requests
      dl <- list()
      for (i in 1:length(dataSetRequests)) {
        dl[[i]] <- dataSetRequests[[i]]$toList()
      }

      # delete data
      ret <- httr::POST(
        paste0(
          private$url, "Data/Static/Delete"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    },

    #' @description Load the static numeric data specified by the requests.
    #'
    #' @param dataSetRequests A list of data set requests specifying which data
    #' shall be loaded. The items in the list must be objects of the class
    #' 'DataSetRequest'.
    #'
    #' @return An object of type list containing the data and other fields.
    Load = function(dataSetRequests) {
      # create list from list of dataset requests
      dl <- list()
      for (i in 1:length(dataSetRequests)) {
        dl[[i]] <- dataSetRequests[[i]]$toList()
      }

      # get return data
      ret <- httr::POST(
        paste0(
          private$url, "Data/Static/Load"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
      cont <- httr::content(ret, as = "text")
      return(jsonlite::fromJSON(cont))
    },

    #' @description Save the specified static numeric data.
    #'
    #' @param staticData The data to save. Has to be an object of the class
    #' 'StaticData'. If the unit is not specified, the data will be saved using
    #' the signal's storage unit.
    Save = function(staticData) {
      # create list from list of static data
      dl <- list()
      for (i in 1:length(staticData)) {
        dl[[i]] <- staticData[[i]]$toList()
      }

      # save the data
      ret <- httr::POST(
        paste0(
          private$url, "Data/Static/Save"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL
  )
)

#' @title StaticString
#'
#' @description Provides access to functionality related to static string data.
#'
#' @details A new instance of this class will be created by the DataServices
#' when calling the method 'GetStaticString'.
#'
#' @export StaticString
StaticString <- R6Class("StaticString",
  public = list(
    #' @description Create a new StaticString object instance. This is done by
    #' the DataServices automatically if needed.
    #'
    #' @param url The URL for the API calls.
    #' @param tokenType The type of the issued token.
    #' @param token The issued token.
    initialize = function(url, tokenType, token) {
      private$url <- url
      private$tokenType <- tokenType
      private$token <- token
    },

    #' @description Delete the static string data specified by the requests.
    #'
    #' @param dataSetRequests A list of data set requests specifying which data
    #' shall be deleted. The items in the list must be objects of the class
    #' 'DataSetRequest'.
    Delete = function(dataSetRequests) {
      # create list from list of dataset requests
      dl <- list()
      for (i in 1:length(dataSetRequests)) {
        dl[[i]] <- dataSetRequests[[i]]$toList()
      }

      # delete data
      ret <- httr::POST(
        paste0(
          private$url, "Data/String/Delete"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    },

    #' @description Load the static string data specified by the requests.
    #'
    #' @param dataSetRequests A list of data set requests specifying which data
    #' shall be loaded. The items in the list must be objects of the class
    #' 'DataSetRequest'.
    #'
    #' @return An object of type list containing the data and other fields.
    Load = function(dataSetRequests) {
      # create list from list of dataset requests
      dl <- list()
      for (i in 1:length(dataSetRequests)) {
        dl[[i]] <- dataSetRequests[[i]]$toList()
      }

      # get return data
      ret <- httr::POST(
        paste0(
          private$url, "Data/String/Load"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
      cont <- httr::content(ret, as = "text")
      return(jsonlite::fromJSON(cont))
    },

    #' @description Save the specified static string data.
    #'
    #' @param staticData The data to save. Has to be an object of the class
    #' 'StaticData'. If the unit is not specified, the data will be saved using
    #' the signal's storage unit.
    Save = function(staticData) {
      # create list from list of static data
      dl <- list()
      for (i in 1:length(staticData)) {
        dl[[i]] <- staticData[[i]]$toList()
      }

      # save the data
      ret <- httr::POST(
        paste0(
          private$url, "Data/String/Save"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL
  )
)

#' @title TimeNumeric
#'
#' @description Provides access to functionality related to time-dependent
#' numeric data.
#'
#' @details A new instance of this class will be created by the DataServices
#' when calling the method 'GetTimeNumeric'.
#'
#' @export TimeNumeric
TimeNumeric <- R6Class("TimeNumeric",
  public = list(
    #' @description Create a new TimeNumeric object instance. This is done by
    #' the DataServices automatically if needed.
    #'
    #' @param url The URL for the API calls.
    #' @param tokenType The type of the issued token.
    #' @param token The issued token.
    initialize = function(url, tokenType, token) {
      private$url <- url
      private$tokenType <- tokenType
      private$token <- token
    },

    #' @description Delete the time-dependent numeric data specified by the
    #' requests.
    #'
    #' @param start The start of the time-range for which the data shall be
    #' deleted.
    #' @param end The end of the time-range for which the data shall be deleted.
    #' @param dataSetRequests A list of data set requests specifying which data
    #' shall be deleted. The items in the list must be objects of the class
    #' 'DataSetRequest'.
    Delete = function(start, end, dataSetRequests) {
      # create list from list of dataset requests
      dl <- list()
      for (i in 1:length(dataSetRequests)) {
        dl[[i]] <- dataSetRequests[[i]]$toList()
      }

      # delete data
      ret <- httr::POST(
        paste0(
          private$url, "Data/Time/Delete"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        query = list(Start = start, End = end),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    },

    #' @description Load the time-dependent numeric data specified by the
    #' requests.
    #'
    #' @param start The start of the time-range for which the data shall be
    #' loaded.
    #' @param end The end of the time-range for which the data shall be loaded.
    #' @param increment The time increment in which the data shall be loaded.
    #' Allowed values are: \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
    #' \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
    #' \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}
    #' @param hierarchyName (Optional) If the name of an existing hierarchy is
    #' specified, the data will be aggregated according to the hierarchy
    #' relationships if needed.
    #' @param dataSetRequests A list of data set requests specifying which data
    #' shall be loaded. The items in the list must be objects of the class
    #' 'DataSetRequest'.
    #'
    #' @return An object of type list containing the data and other fields.
    Load = function(start,
                    end,
                    increment = c(
                      "EveryMinute", "EveryFiveMinutes",
                      "EveryFifteenMinutes", "Hourly", "Daily",
                      "Monthly", "Quarterly", "Yearly"
                    ),
                    hierarchyName = NULL,
                    dataSetRequests) {
      # check inputs
      increment <- match.arg(increment)

      # create list from list of dataset requests
      dl <- list()
      for (i in 1:length(dataSetRequests)) {
        dl[[i]] <- dataSetRequests[[i]]$toList()
      }

      # get return data
      ret <- httr::POST(
        paste0(
          private$url, "Data/Time/Load"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        query = list(
          Start = start,
          End = end,
          Increment = increment,
          Hierarchy = hierarchyName
        ),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
      cont <- httr::content(ret, as = "text")
      return(jsonlite::fromJSON(cont))
    },

    #' @description Save the specified time-dependent numeric data. Existing
    #' data in the range of the new data is deleted first. The range of the data
    #' is defined by the specified data to save.
    #'
    #' @param timeData The data to save. Has to be an object of the class
    #' 'TimeData'. If the unit is not specified, the data will be saved using
    #' the signal's storage unit.
    Save = function(timeData) {
      # create list from list of time data
      dl <- list()
      for (i in 1:length(timeData)) {
        dl[[i]] <- timeData[[i]]$toList()
      }

      # save the data
      ret <- httr::POST(
        paste0(
          private$url, "Data/Time/Save"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL
  )
)

#' @title TimeString
#'
#' @description Provides access to functionality related to time-dependent
#' string data.
#'
#' @details A new instance of this class will be created by the DataServices
#' when calling the method 'GetTimeString'.
#'
#' @export TimeString
TimeString <- R6Class("TimeString",
  public = list(
    #' @description Create a new TimeString object instance. This is done by
    #' the DataServices automatically if needed.
    #'
    #' @param url The URL for the API calls.
    #' @param tokenType The type of the issued token.
    #' @param token The issued token.
    initialize = function(url, tokenType, token) {
      private$url <- url
      private$tokenType <- tokenType
      private$token <- token
    },

    #' @description Delete the time-dependent string data specified by the
    #' requests.
    #'
    #' @param start The start of the time-range for which the data shall be
    #' deleted.
    #' @param end The end of the time-range for which the data shall be deleted.
    #' @param dataSetRequests A list of data set requests specifying which data
    #' shall be deleted. The items in the list must be objects of the class
    #' 'DataSetRequest'.
    Delete = function(start, end, dataSetRequests) {
      # create list from list of dataset requests
      dl <- list()
      for (i in 1:length(dataSetRequests)) {
        dl[[i]] <- dataSetRequests[[i]]$toList()
      }

      # delete data
      ret <- httr::POST(
        paste0(
          private$url, "Data/StringTime/Delete"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        query = list(Start = start, End = end),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    },

    #' @description Load the time-dependent string data specified by the
    #' requests.
    #'
    #' @param start The start of the time-range for which the data shall be
    #' loaded.
    #' @param end The end of the time-range for which the data shall be loaded.
    #' @param increment The time increment in which the data shall be loaded.
    #' Allowed values are: \code{"EveryMinute"}, \code{"EveryFiveMinutes"},
    #' \code{"EveryFifteenMinutes"}, \code{"Hourly"}, \code{"Daily"},
    #' \code{"Monthly"}, \code{"Quarterly"}, \code{"Yearly"}
    #' @param hierarchyName (Optional) If the name of an existing hierarchy is
    #' specified, the data will be aggregated according to the hierarchy
    #' relationships if needed.
    #' @param dataSetRequests A list of data set requests specifying which data
    #' shall be loaded. The items in the list must be objects of the class
    #' 'DataSetRequest'.
    #'
    #' @return An object of type list containing the data and other fields.
    Load = function(start,
                    end,
                    increment = c(
                      "EveryMinute", "EveryFiveMinutes",
                      "EveryFifteenMinutes", "Hourly", "Daily",
                      "Monthly", "Quarterly", "Yearly"
                    ),
                    hierarchyName = NULL,
                    dataSetRequests) {
      # check inputs
      increment <- match.arg(increment)

      # create list from list of dataset requests
      dl <- list()
      for (i in 1:length(dataSetRequests)) {
        dl[[i]] <- dataSetRequests[[i]]$toList()
      }

      # get return data
      ret <- httr::POST(
        paste0(
          private$url, "Data/StringTime/Load"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        query = list(
          Start = start,
          End = end,
          Increment = increment,
          Hierarchy = hierarchyName
        ),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
      cont <- httr::content(ret, as = "text")
      return(jsonlite::fromJSON(cont))
    },

    #' @description Save the specified time-dependent string data. Existing
    #' data in the range of the new data is deleted first. The range of the data
    #' is defined by the specified data to save.
    #'
    #' @param timeData The data to save. Has to be an object of the class
    #' 'TimeData'. If the unit is not specified, the data will be saved using
    #' the signal's storage unit.
    Save = function(timeData) {
      # create list from list of static data
      dl <- list()
      for (i in 1:length(timeData)) {
        dl[[i]] <- timeData[[i]]$toList()
      }

      # save the data
      ret <- httr::POST(
        paste0(
          private$url, "Data/StringTime/Save"
        ),
        body = jsonlite::toJSON(dl, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::add_headers(Authorization = paste(private$tokenType, private$token))
      )

      httr::stop_for_status(ret)
    }
  ),
  private = list(
    url = NULL,
    tokenType = NULL,
    token = NULL
  )
)
