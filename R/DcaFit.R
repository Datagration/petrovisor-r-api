library("R6")

#' @title DcaFit
#'
#' @description Class representing a PetroVisor DcaFit object.
#'
#' @export DcaFit
#'
#' @field name The name of the DCA fit.
#' @field entity The entity for which the DCA fit is created. Must be an
#'   instance of the class \code{Entity}.
#' @field rateSignal The signal giving the production rate. Must be an
#'   instance of the class \code{Signal}.
#' @field rateUnit The unit the rate signal is given in. Must be an
#'   instance of the class \code{Unit}.
#' @field cumulativeSignal The signal giving the cumulative data. Must be
#'   an instance of the class \code{Signal}.
#' @field cumulativeUnit The unit the cumulative signal is given in.
#'   Must be an instance of the class \code{Unit}.
#' @field timeScope The scope of the fit. Must be an instance of the class
#'   \code{Scope}.
#' @field dataPoints A list holding the data points for the fit. The items
#'   in the list must be instances of the class \code{DataPoint}.
#' @field excludedDataPoints A list of excluded data points. The items
#'   in the list must be instances of the class \code{DataPoint}.
#' @field cumulativeDataPoints A list holding the cumulative data points
#'   for the fit. The items in the list must be instances of the class
#'   \code{DataPoint}.
#' @field fitRangeStart The start date of the fit range (string).
#' @field fitRangeEnd The end date of the fit range (string).
#' @field fitType The type of the fit.
#' @field isAutomatic This flag specifies whether the fit was created
#'   automatically.
#' @field isReviewed This flag specifies whether the fit was reviewed.
#' @field isReviewNeeded This flag specifies whether a review of the fit
#'   is needed.
#' @field intercept The value of the fit's y-intercept.
#' @field slope The value of the fit's slope.
#' @field hyperbolicConstant The value of the fit's hyperbolic constant.
#' @field rootSquared The RS value of the fit.
#' @field meanAbsoluteError The fit's mean absolute error.
#' @field rootMeanSquareError The fit's RMS error.
#' @field estimatedUltimateRecovery The EUR calculated from the fit.
#' @field remainingReserves The remaining reserves based on the fit.
#' @field economicLimit The economic limit for the calculation of EUR and
#'   remaining reserves.
#' @field leftBehind The reserves that are left behind.
#' @field nowDate The current date.
#' @field forecastEndDate The end date of the forecast.
#' @field isLocked This flag specifies whether the DCA fit is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the DCA fit belongs to.
#' @field isFavorite This flag specifies whether the DCA fit is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the DCA fit.
#' @examples
#' \dontrun{
#' DcaFit$new( )
#'}
DcaFit <- R6Class("DcaFit",
  public = list(
    name = NULL,
    entity = NULL,
    rateSignal = NULL,
    rateUnit = NULL,
    cumulativeSignal = NULL,
    cumulativeUnit = NULL,
    timeScope = NULL,
    dataPoints = NULL,
    excludedDataPoints = NULL,
    cumulativeDataPoints = NULL,
    fitRangeStart = NULL,
    fitRangeEnd = NULL,
    fitType = NULL,
    isAutomatic = NULL,
    isReviewed = NULL,
    isReviewNeeded = NULL,
    intercept = NULL,
    slope = NULL,
    hyperbolicConstant = NULL,
    rootSquared = NULL,
    meanAbsoluteError = NULL,
    rootMeanSquareError = NULL,
    estimatedUltimateRecovery = NULL,
    remainingReserves = NULL,
    economicLimit = NULL,
    leftBehind = NULL,
    nowDate = NULL,
    forecastEndDate = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new GeoDataGrid instance.
    #'
    #' @param name The name of the DCA fit.
    #' @param entity The entity for which the DCA fit is created. Must be an
    #'   instance of the class \code{Entity}.
    #' @param rateSignal The signal giving the production rate. Must be an
    #'   instance of the class \code{Signal}.
    #' @param rateUnit The unit the rate signal is given in. Must be an
    #'   instance of the class \code{Unit}.
    #' @param cumulativeSignal The signal giving the cumulative data. Must be
    #'   an instance of the class \code{Signal}.
    #' @param cumulativeUnit The unit the cumulative signal is given in.
    #'   Must be an instance of the class \code{Unit}.
    #' @param timeScope The scope of the fit. Must be an instance of the class
    #'   \code{Scope}.
    #' @param dataPoints A list holding the data points for the fit. The items
    #'   in the list must be instances of the class \code{DataPoint}.
    #' @param excludedDataPoints A list of excluded data points. The items
    #'   in the list must be instances of the class \code{DataPoint}.
    #' @param cumulativeDataPoints A list holding the cumulative data points
    #'   for the fit. The items in the list must be instances of the class
    #'   \code{DataPoint}.
    #' @param fitRangeStart The start date of the fit range (string).
    #' @param fitRangeEnd The end date of the fit range (string).
    #' @param fitType The type of the fit.
    #' @param isAutomatic This flag specifies whether the fit was created
    #'   automatically.
    #' @param isReviewed This flag specifies whether the fit was reviewed.
    #' @param isReviewNeeded This flag specifies whether a review of the fit
    #'   is needed.
    #' @param intercept The value of the fit's y-intercept.
    #' @param slope The value of the fit's slope.
    #' @param hyperbolicConstant The value of the fit's hyperbolic constant.
    #' @param rootSquared The RS value of the fit.
    #' @param meanAbsoluteError The fit's mean absolute error.
    #' @param rootMeanSquareError The fit's RMS error.
    #' @param estimatedUltimateRecovery The EUR calculated from the fit.
    #' @param remainingReserves The remaining reserves based on the fit.
    #' @param economicLimit The economic limit for the calculation of EUR and
    #'   remaining reserves.
    #' @param leftBehind The reserves that are left behind.
    #' @param nowDate The current date.
    #' @param forecastEndDate The end date of the forecast.
    #' @param isLocked This flag specifies whether the DCA fit is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the DCA fit belongs to.
    #' @param isFavorite This flag specifies whether the DCA fit is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the DCA fit.
    initialize = function(name = NULL,
                          entity = NULL,
                          rateSignal = NULL,
                          rateUnit = NULL,
                          cumulativeSignal = NULL,
                          cumulativeUnit = NULL,
                          timeScope = NULL,
                          dataPoints = NULL,
                          excludedDataPoints = NULL,
                          cumulativeDataPoints = NULL,
                          fitRangeStart = NULL,
                          fitRangeEnd = NULL,
                          fitType = NULL,
                          isAutomatic = NULL,
                          isReviewed = NULL,
                          isReviewNeeded = NULL,
                          intercept = NULL,
                          slope = NULL,
                          hyperbolicConstant = NULL,
                          rootSquared = NULL,
                          meanAbsoluteError = NULL,
                          rootMeanSquareError = NULL,
                          estimatedUltimateRecovery = NULL,
                          remainingReserves = NULL,
                          economicLimit = NULL,
                          leftBehind = NULL,
                          nowDate = NULL,
                          forecastEndDate = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$entity <- entity
      self$rateSignal <- rateSignal
      self$rateUnit <- rateUnit
      self$cumulativeSignal <- cumulativeSignal
      self$cumulativeUnit <- cumulativeUnit
      self$timeScope <- timeScope
      self$dataPoints <- dataPoints
      self$excludedDataPoints <- excludedDataPoints
      self$cumulativeDataPoints <- cumulativeDataPoints
      self$fitRangeStart <- fitRangeStart
      self$fitRangeEnd <- fitRangeEnd
      self$fitType <- fitType
      self$isAutomatic <- isAutomatic
      self$isReviewed <- isReviewed
      self$isReviewNeeded <- isReviewNeeded
      self$intercept <- intercept
      self$slope <- slope
      self$hyperbolicConstant <- hyperbolicConstant
      self$rootSquared <- rootSquared
      self$meanAbsoluteError <- meanAbsoluteError
      self$rootMeanSquareError <- rootMeanSquareError
      self$estimatedUltimateRecovery <- estimatedUltimateRecovery
      self$remainingReserves <- remainingReserves
      self$economicLimit <- economicLimit
      self$leftBehind <- leftBehind
      self$nowDate <- nowDate
      self$forecastEndDate <- forecastEndDate
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of data points
      dataPointsList <- list()
      if(!is.null(self$dataPoints)){
        for (i in 1:length(self$dataPoints)){
          dataPointsList[[i]] <- self$dataPoints[[i]]$toList()
        }
      } else {
        dataPointsList[[1]] <- ""
      }

      # create list from list of excluded data points
      excludedDataPointsList <- list()
      if(!is.null(self$excludedDataPoints)){
        for (i in 1:length(self$excludedDataPoints)){
          excludedDataPointsList[[i]] <- self$excludedDataPoints[[i]]$toList()
        }
      } else {
        excludedDataPointsList[[1]] <- ""
      }

      # create list from list of cumulative data points
      cumulativeDataPointsList <- list()
      if(!is.null(self$cumulativeDataPoints)){
        for (i in 1:length(self$cumulativeDataPoints)){
          cumulativeDataPointsList[[i]] <- self$cumulativeDataPoints[[i]]$toList()
        }
      } else {
        cumulativeDataPointsList[[1]] <- ""
      }


      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Entity = if(is.null(self$entity)) "" else self$entity$toList(),
        RateSignal = if(is.null(self$rateSignal)) {
          ""
        } else {
          self$rateSignal$toList()
        },
        RateUnit = if(is.null(self$rateUnit)) {
          ""
        } else {
          self$rateUnit$toList()
        },
        CumulativeSignal = if(is.null(self$cumulativeSignal)) {
          ""
        } else {
          self$cumulativeSignal$toList()
        },
        CumulativeUnit = if(is.null(self$cumulativeUnit)) {
          ""
        } else {
          self$cumulativeUnit$toList()
        },
        TimeScope = if(is.null(self$timeScope)) {
          ""
        } else {
          self$timeScope$toList()
        },
        DataPoints = if(is.null(self$dataPoints)) "" else dataPointsList,
        ExcludedDataPoints = if(is.null(self$excludedDataPoints)) {
          ""
        } else {
          excludedDataPointsList
        },
        CumulativeDataPoints = if(is.null(self$cumulativeDataPoints)) {
          ""
        } else {
          cumulativeDataPointsList
        },
        FitRange = list(
          Start = if(is.null(self$fitRangeStart)) {
            ""
          } else {
            self$fitRangeStart
          },
          End = if(is.null(self$fitRangeEnd)) {
            ""
          } else {
            self$fitRangeEnd
          }
        ),
        FitType = if(is.null(self$fitType)) "" else self$fitType,
        IsAutomatic = if(is.null(self$isAutomatic)) "" else self$isAutomatic,
        IsReviewed = if(is.null(self$isReviewed)) "" else self$isReviewed,
        IsReviewNeeded = if(is.null(self$isReviewNeeded)) {
          ""
        } else {
          self$isReviewNeeded
        },
        Intercept = if(is.null(self$intercept)) "" else self$intercept,
        Slope = if(is.null(self$slope)) "" else self$slope,
        HyperbolicConstant = if(is.null(self$hyperbolicConstant)) {
          ""
        } else {
          self$hyperbolicConstant
        },
        RootSquared = if(is.null(self$rootSquared)) "" else self$rootSquared,
        MeanAbsoluteError = if(is.null(self$meanAbsoluteError)) {
          ""
        } else {
          self$meanAbsoluteError
        },
        RootMeanSquareError = if(is.null(self$rootMeanSquareError)) {
          ""
        } else {
          self$rootMeanSquareError
        },
        EstimatedUltimateRecovery =
          if(is.null(self$estimatedUltimateRecovery)) {
           ""
          } else {
          self$estimatedUltimateRecovery
          },
        RemainingReserves = if(is.null(self$remainingReserves)) {
          ""
        } else {
          self$remainingReserves
        },
        EconomicLimit = if(is.null(self$economicLimit)) {
          ""
        } else {
          self$economicLimit
        },
        LeftBehind = if(is.null(self$leftBehind)) "" else self$leftBehind,
        NowDate = if(is.null(self$nowDate)) "" else self$nowDate,
        ForecastEndDate = if(is.null(self$forecastEndDate)) {
          ""
        } else {
          self$forecastEndDate
        },
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)
