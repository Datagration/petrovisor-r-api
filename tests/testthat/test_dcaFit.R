##### DCA FIT #####
context("DCA fit instanciation and conversion to list")

test_that("DCA fit instanciation and conversion works",{
  entity <- Entity$new(name = "Well01",
                       alias = "Alias01",
                       entityTypeName = "Well")

  rateSignal <- Signal$new(name = "oil rate",
                           shortName = "or",
                           measurementName = "FlowRate",
                           storageUnitName = "m3/s",
                           aggregationType = "Average",
                           containerAggregationType = "Average",
                           signalType = "Time-dependent",
                           defaultColor = 0,
                           defaultLineType = "Solid",
                           settingName = NULL)

  rateUnit <- Unit$new(name = "m3/d",
                       measurementName = "FlowRate",
                       factor = 1,
                       summand = 0)

  cumSignal <- Signal$new(name = "oil",
                           shortName = "o",
                           measurementName = "Volume",
                           storageUnitName = "m3",
                           aggregationType = "Sum",
                           containerAggregationType = "Sum",
                           signalType = "Time-dependent",
                           defaultColor = 0,
                           defaultLineType = "Solid",
                           settingName = NULL)

  cumUnit <- Unit$new(name = "m3",
                       measurementName = "Volume",
                       factor = 1,
                       summand = 0)

  scope <- Scope$new(start = "2020-01-01T00:00:00.000Z",
                     end = "2020-03-01T00:00:00.000Z",
                     timeIncrement = "Daily")

  dp1 <- DataPoint$new(date = "2020-01-01T00:00:00.000Z",
                       value = 35)
  dp2 <- DataPoint$new(date = "2020-01-12T00:00:00.000Z",
                       value = 20)

  edp1 <- DataPoint$new(date = "2020-01-02T00:00:00.000Z",
                       value = 35)
  edp2 <- DataPoint$new(date = "2020-01-24T00:00:00.000Z",
                       value = 20)

  cdp1 <- DataPoint$new(date = "2020-01-01T00:00:00.000Z",
                       value = 70)
  cdp2 <- DataPoint$new(date = "2020-01-12T00:00:00.000Z",
                       value = 40)

  dcaFit <- DcaFit$new(name = "MyFit",
                       entity = entity,
                       rateSignal = rateSignal,
                       rateUnit = rateUnit,
                       cumulativeSignal = cumSignal,
                       cumulativeUnit = cumUnit,
                       timeScope = scope,
                       dataPoints = list(dp1, dp2),
                       excludedDataPoints = list(edp1, edp2),
                       cumulativeDataPoints = list(cdp1, cdp2),
                       fitRangeStart = "2020-01-01T00:00:00.000Z",
                       fitRangeEnd = "2020-04-01T00:00:00.000Z",
                       fitType = "linear",
                       isAutomatic =TRUE,
                       isReviewed = FALSE,
                       isReviewNeeded = TRUE,
                       intercept = 55,
                       slope = -0.5,
                       hyperbolicConstant = 0.1,
                       rootSquared = 0,
                       meanAbsoluteError = 0,
                       rootMeanSquareError = 0,
                       estimatedUltimateRecovery = 10000,
                       remainingReserves = 120000,
                       economicLimit = 150,
                       leftBehind = 15000,
                       nowDate = "2020-03-15T00:00:00.000Z",
                       forecastEndDate = "2020-05-01T00:00:00.000Z",
                       isLocked = FALSE,
                       user = "MyUser",
                       isFavorite = FALSE,
                       labels = list("label1", "label2"))

  listed <- dcaFit$toList()

  expect_equal(listed,
               list(Name = "MyFit",
                    Entity = entity$toList(),
                    RateSignal = rateSignal$toList(),
                    RateUnit = rateUnit$toList(),
                    CumulativeSignal = cumSignal$toList(),
                    CumulativeUnit = cumUnit$toList(),
                    TimeScope = scope$toList(),
                    DataPoints = list(dp1$toList(), dp2$toList()),
                    ExcludedDataPoints = list(edp1$toList(), edp2$toList()),
                    CumulativeDataPoints = list(cdp1$toList(), cdp2$toList()),
                    FitRange = list(Start = "2020-01-01T00:00:00.000Z",
                                    End = "2020-04-01T00:00:00.000Z"),
                    FitType = "linear",
                    IsAutomatic =TRUE,
                    IsReviewed = FALSE,
                    IsReviewNeeded = TRUE,
                    Intercept = 55,
                    Slope = -0.5,
                    HyperbolicConstant = 0.1,
                    RootSquared = 0,
                    MeanAbsoluteError = 0,
                    RootMeanSquareError = 0,
                    EstimatedUltimateRecovery = 10000,
                    RemainingReserves = 120000,
                    EconomicLimit = 150,
                    LeftBehind = 15000,
                    NowDate = "2020-03-15T00:00:00.000Z",
                    ForecastEndDate = "2020-05-01T00:00:00.000Z",
                    IsLocked = FALSE,
                    User = "MyUser",
                    IsFavorite = FALSE,
                    Labels = list("label1", "label2")))
})

test_that("DCA fit instanciation and conversion works (empty constructor)",{
  dcaFit <- DcaFit$new()

  listed <- dcaFit$toList()

  expect_equal(listed,
               list(Name = "",
                    Entity = "",
                    RateSignal = "",
                    RateUnit = "",
                    CumulativeSignal = "",
                    CumulativeUnit = "",
                    TimeScope = "",
                    DataPoints = "",
                    ExcludedDataPoints = "",
                    CumulativeDataPoints = "",
                    FitRange = list(Start = "",
                                    End = ""),
                    FitType = "",
                    IsAutomatic ="",
                    IsReviewed = "",
                    IsReviewNeeded = "",
                    Intercept = "",
                    Slope = "",
                    HyperbolicConstant = "",
                    RootSquared = "",
                    MeanAbsoluteError = "",
                    RootMeanSquareError = "",
                    EstimatedUltimateRecovery = "",
                    RemainingReserves = "",
                    EconomicLimit = "",
                    LeftBehind = "",
                    NowDate = "",
                    ForecastEndDate = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
