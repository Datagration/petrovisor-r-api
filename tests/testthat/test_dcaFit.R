##### DCA FIT #####
context("DCA fit instantiation and conversion to list")

test_that("DCA fit instantiation and conversion works", {
  entity <- Entity$new(name = "Well01",
                       alias = "Alias01",
                       entity_type_name = "Well")

  rate_signal <- Signal$new(name = "oil rate",
                            short_name = "or",
                            measurement_name = "FlowRate",
                            storage_unit_name = "m3/s",
                            aggregation_type = "Average",
                            container_aggregation_type = "Average",
                            signal_type = "TimeDependent",
                            default_color = 0,
                            default_line_type = "Solid",
                            setting_name = NULL)

  rate_unit <- Unit$new(name = "m3/d",
                        measurement_name = "FlowRate",
                        factor = 1,
                        summand = 0)

  cum_signal <- Signal$new(name = "oil",
                           short_name = "o",
                           measurement_name = "Volume",
                           storage_unit_name = "m3",
                           aggregation_type = "Sum",
                           container_aggregation_type = "Sum",
                           signal_type = "TimeDependent",
                           default_color = 0,
                           default_line_type = "Solid",
                           setting_name = NULL)

  cum_unit <- Unit$new(name = "m3",
                       measurement_name = "Volume",
                       factor = 1,
                       summand = 0)

  scope <- Scope$new(start = "2020-01-01T00:00:00.000Z",
                     end = "2020-03-01T00:00:00.000Z",
                     time_increment = "Daily")

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

  dca_fit <- DcaFit$new(name = "MyFit",
                        entity = entity,
                        rateSignal = rate_signal,
                        rateUnit = rate_unit,
                        cumulativeSignal = cum_signal,
                        cumulativeUnit = cum_unit,
                        timeScope = scope,
                        dataPoints = list(dp1, dp2),
                        excludedDataPoints = list(edp1, edp2),
                        cumulativeDataPoints = list(cdp1, cdp2),
                        fitRangeStart = "2020-01-01T00:00:00.000Z",
                        fitRangeEnd = "2020-04-01T00:00:00.000Z",
                        fitType = "linear",
                        isAutomatic = TRUE,
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

  listed <- dca_fit$toList()

  expect_equal(listed,
               list(Name = "MyFit",
                    Entity = entity$toList(),
                    RateSignal = rate_signal$toList(),
                    RateUnit = rate_unit$toList(),
                    CumulativeSignal = cum_signal$toList(),
                    CumulativeUnit = cum_unit$toList(),
                    TimeScope = scope$toList(),
                    DataPoints = list(dp1$to_list(), dp2$to_list()),
                    ExcludedDataPoints = list(edp1$to_list(), edp2$to_list()),
                    CumulativeDataPoints = list(cdp1$to_list(), cdp2$to_list()),
                    FitRange = list(Start = "2020-01-01T00:00:00.000Z",
                                    End = "2020-04-01T00:00:00.000Z"),
                    FitType = "linear",
                    IsAutomatic = TRUE,
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

test_that("DCA fit instantiation and conversion works (empty constructor)", {
  dca_fit <- DcaFit$new()

  listed <- dca_fit$toList()

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
                    IsAutomatic = "",
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
