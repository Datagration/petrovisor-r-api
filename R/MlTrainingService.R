library("R6")

#' @title MlTrainingService
#'
#' @description Provides access to PetroVisor model training functionalities.
#'
#' @details This service provides methods to train machine learning models,
#'   publish trained models, and make predictions. It uses the singleton
#'   AuthContext for authentication instead of requiring auth parameters.
#'
#' @export MlTrainingService
#'
#' @examples
#' \dontrun{
#' # The service is created automatically by ServiceProvider
#' sp <- ServiceProvider$new(url = "...", ...)
#'
#' # Create a model with training options
#' model <- MLModel$new(
#'   name = "Test Model",
#'   table_formula = "Table \"TestData\" ... End Table",
#'   label_column_name = "target"
#' )
#'
#' # Set scope and entity set for training
#' scope <- Scope$new(name = "TestScope", ...)
#' entity_set <- EntitySet$new(name = "TestEntities", ...)
#' model$set_training_options(scope, entity_set)
#'
#' # Train the model
#' request <- sp$ml$train(model)
#'
#' # Check training status
#' status <- sp$ml$get_training_status(request$id)
#'
#' # Get training results
#' results <- sp$ml$get_training_results(request$id)
#'
#' # Save the best model
#' best_models <- results$get_best_models()
#' sp$ml$save_best_model(model, best_models, request$id)
#'
#' # Make predictions
#' predictions <- sp$ml$predict(model$name, predictor_values)
#' }
MlTrainingService <- R6Class( # nolint: object_name_linter
  "MlTrainingService",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(

    #' @description Create a new MlTrainingService instance. This is done by
    #'   the ServiceProvider automatically. The service uses the singleton
    #'   AuthContext for authentication.
    initialize = function() {},

    #' @description Train machine learning models.
    #'
    #' @param model An MLModel object with configured training options.
    #'
    #' @return Training response from the API containing request ID.
    train = function(model) {
      workspace <- get_auth_context()$get_workspace()

      result <- super$post(
        body = list(
          IsModelPerEntity = model$is_model_per_entity,
          ModelName = model$name,
          Options = model$training_options$toList(),
          Source = "R SDK",
          WorkspaceName = workspace
        ),
        route = "ModelTraining/AddRequest",
        expect_data = TRUE
      )

      return(list(id = result$Id))
    },

    #' @description Get the status of a model training request.
    #'
    #' @param request_id The ID of the model training request.
    #'
    #' @return The status of the model training request.
    get_training_status = function(request_id) {
      result <- super$get(route = paste0("ModelTraining/", request_id))
      return(list(status = result$Status))
    },

    #' @description Get model training results.
    #'
    #' @param request_id The ID of the model training request.
    #'
    #' @return MLTrainingResults object containing structured training results.
    get_training_results = function(request_id) {
      raw_result <-
        super$get(route = paste0("ModelTraining/Results/", request_id))

      # Convert raw API response to structured R6 object
      training_result <- private$parse_training_results(raw_result, request_id)

      return(training_result)
    },

    #' @description Save the selected model.
    #'
    #' @param model The MLModel object to update and save. This should be an
    #'   instance of MLModel.
    #' @param best_models A list of best model results, typically from
    #'  MLTrainingResults$get_best_models().
    #'  Each item should contain model outcome, features, trainer name,
    #'  trained model, training context, and confusion matrix.
    #' @param request_id The ID of the model training request (character).
    #'
    #' @return The API response from updating the model training state
    #'   (typically a list).
    save_best_model = function(model, best_models, request_id) {
      if (length(best_models) == 0) {
        stop("No best model found in training results.",
             "Please check if training completed successfully.")
      }

      # Convert best_models to list of MLNetModel objects
      best_models <- lapply(
        best_models,
        function(item, model_name) {
          MLNetModel$new(
            name = if (!is.null(item$entity_name))
              item$entity_name else model_name,
            is_reviewed = TRUE,
            metrics = item$outcome$values,
            test_metrics = item$outcome$test_values,
            features = item$features,
            trainer_name = item$trainer_name,
            trained_mlnet_model = item$trained_model,
            training_context = item$training_context,
            confusion_matrix = item$confusion_matrix
          )
        },
        model_name = model$name
      )

      # Update model properties
      model$trained_models <- best_models
      model$trained <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z")
      model$is_automatic <- TRUE

      # Save the model using a put call to the API
      route <- paste0("MLModels/", model$name)
      put_body <- model$toList()
      result <- super$put(model$toList(), route = route)

      # Update training status
      result <- super$post(body = list(),
                           route = paste0("ModelTraining/UpdateState/",
                                          request_id),
                           expect_data = TRUE,
                           query = list(status = "Completed"))
      return(result)
    },

    #' @description Make predictions using trained models.
    #'
    #' @param model_name The name of the model to make predictions on
    #'   (character).
    #' @param prediction_data The data to make predictions on.
    #' @param entity_name The name of the entity to make predictions on.
    #'
    #' @return Prediction results from the API.
    predict = function(model_name, prediction_data, entity_name = NULL) {
      prediction_data <- as.list(prediction_data)
      source_data <- list()
      for (i in 1:length(prediction_data)) { # nolint: loop_index_linter
        source_data[[as.character(i - 1)]] <-
          list(values = list(predictor = prediction_data[[i]]))
      }

      body <- list(
        ModelName = model_name,
        SourceData = source_data
      )
      if (!is.null(entity_name)) {
        body$EntityName <- entity_name
      }

      result <- super$post(body = body,
                           route = "MLModels/Predict",
                           expect_data = TRUE)

      return(result)
    }
  ),

  private = list(
    #' description Parse raw training results into structured R6 objects
    #' param raw_result Raw API response from training results endpoint
    #' param request_id The training request ID
    #' return MLTrainingResults object
    parse_training_results = function(raw_result, request_id) {
      if (is.null(raw_result) || missing(raw_result)) {
        stop("raw_result is required")
      }

      if (is.null(request_id) || missing(request_id)) {
        stop("request_id is required")
      }

      # Parse JSON spec fields
      runs_metrics_group_by <- if (!is.null(raw_result$RunsMetricsGroupBy)) {
        raw_result$RunsMetricsGroupBy
      } else {
        NULL
      }

      label_unit_name <- if (!is.null(raw_result$LabelUnitName)) {
        raw_result$LabelUnitName
      } else {
        NULL
      }

      # Parse API wrapper fields
      status <- if (!is.null(raw_result$Status))
        raw_result$Status else "Unknown"

      # Parse results array
      result_items <- list()
      if (!is.null(raw_result$Results) && length(raw_result$Results) > 0) {
        # Handle both data.frame and list formats
        results_data <- raw_result$Results
        # Convert data.frame rows to list
        for (i in seq_len(nrow(results_data))) {
          result_items[[i]] <- private$parse_result_item(results_data[i, ])
        }
      }

      return(MLTrainingResults$new(
        runs_metrics_group_by = runs_metrics_group_by,
        results = result_items,
        label_unit_name = label_unit_name,
        status = status,
        request_id = request_id
      ))
    },

    #' description Parse a single training result item
    #' param raw_item Raw result item from API
    #' return MLTrainingResult object
    parse_result_item = function(raw_item) {
      if (is.null(raw_item)) {
        return(NULL)
      }

      # Extract fields according to JSON spec
      entity_name <- if (!is.null(raw_item$EntityName) &&
                           nchar(raw_item$EntityName) > 0) {
        raw_item$EntityName
      } else {
        NULL
      }

      trained_model <- raw_item$TrainedModel
      training_context <-
        private$parse_training_context(raw_item$TrainingContext)
      confusion_matrix <- raw_item$ConfusionMatrix

      error <- if (!is.null(raw_item$Error)) {
        raw_item$Error
      } else {
        NULL
      }

      trainer_name <- if (!is.null(raw_item$TrainerName)) {
        raw_item$TrainerName
      } else {
        ""
      }

      # Parse outcome
      outcome <- private$parse_outcome(raw_item$Outcome)

      # Parse features
      features <- private$parse_features(raw_item$Features[[1]])

      return(MLTrainingResult$new(
        entity_name = entity_name,
        trained_model = trained_model,
        training_context = training_context,
        confusion_matrix = confusion_matrix,
        outcome = outcome,
        error = error,
        trainer_name = trainer_name,
        features = features
      ))
    },

    #' description Parse outcome data
    #' param raw_outcome Raw outcome data from API
    #' return MLTrainingOutcome object
    parse_outcome = function(raw_outcome) {
      if (is.null(raw_outcome)) {
        return(NULL)
      }

      is_best_model <- if (!is.null(raw_outcome$IsBestModel)) {
        raw_outcome$IsBestModel
      } else {
        FALSE
      }

      is_best_run <- if (!is.null(raw_outcome$IsBestRun)) {
        raw_outcome$IsBestRun
      } else {
        FALSE
      }

      group_by <- if (!is.null(raw_outcome$GroupBy)) {
        raw_outcome$GroupBy
      } else {
        NULL
      }

      sequence <- if (!is.null(raw_outcome$Sequence)) {
        raw_outcome$Sequence
      } else {
        NULL
      }

      is_best_model_test <- if (!is.null(raw_outcome$IsBestModelTest)) {
        raw_outcome$IsBestModelTest
      } else {
        FALSE
      }

      is_best_run_test <- if (!is.null(raw_outcome$IsBestRunTest)) {
        raw_outcome$IsBestRunTest
      } else {
        FALSE
      }

      # Parse values
      values <- private$parse_metric_values(raw_outcome$Values[[1]])
      test_values <- private$parse_metric_values(raw_outcome$TestValues[[1]])

      return(MLTrainingOutcome$new(
        is_best_model = is_best_model,
        is_best_run = is_best_run,
        group_by = group_by,
        sequence = sequence,
        values = values,
        test_values = test_values,
        is_best_model_test = is_best_model_test,
        is_best_run_test = is_best_run_test
      ))
    },

    #' description Parse metric values array
    #' param raw_values Raw values array from API
    #' return List of MLTrainingNumericOutcome objects
    parse_metric_values = function(raw_values) {
      if (is.null(raw_values) || length(raw_values) == 0) {
        return(list())
      }

      values_list <- list()

      for (i in seq_len(nrow(raw_values))) {
        row_data <- raw_values[i, ]
        values_list[[i]] <- MLTrainingNumericOutcome$new(
          name = as.character(row_data$Name),
          value = if (!is.null(row_data$Value) && !is.na(row_data$Value)) {
            as.numeric(row_data$Value)
          } else {
            NULL
          }
        )
      }

      return(values_list)
    },

    #' description Parse features array
    #' param raw_features Raw features array from API
    #' return List of MLTrainingFeature objects
    parse_features = function(raw_features) {
      if (is.null(raw_features) || length(raw_features) == 0) {
        return(list())
      }

      features_list <- list()

      for (i in seq_len(nrow(raw_features))) {
        row_data <- raw_features[i, ]
        features_list[[i]] <- private$parse_single_feature(row_data)
      }

      return(features_list)
    },

    #' description Parse a single feature
    #' param raw_feature Raw feature data from API
    #' return MLTrainingFeature object
    parse_single_feature = function(raw_feature) {
      if (is.null(raw_feature)) {
        return(NULL)
      }

      name <- if (!is.null(raw_feature$Name)) {
        as.character(raw_feature$Name)
      } else {
        ""
      }

      importance <- if (!is.null(raw_feature$Importance) &&
                          !is.na(raw_feature$Importance)) {
        as.numeric(raw_feature$Importance)
      } else {
        NA
      }

      # Parse weights
      weights <- private$parse_feature_weights(raw_feature$Weights[[1]])

      return(MLTrainingFeature$new(
        name = name,
        importance = importance,
        weights = weights
      ))
    },

    #' description Parse feature weights array
    #' param raw_weights Raw weights array from API
    #' return List of MLTrainingNumericOutcome objects
    parse_feature_weights = function(raw_weights) {
      if (is.null(raw_weights) || length(raw_weights) == 0) {
        return(list())
      }

      weights_list <- list()

      for (i in seq_len(nrow(raw_weights))) {
        row_data <- raw_weights[i, ]
        weights_list[[i]] <- MLTrainingNumericOutcome$new(
          name = if (!is.null(row_data$Name))
            as.character(row_data$Name) else "",
          value = if (!is.null(row_data$Value) && !is.na(row_data$Value)) {
            if (is.character(row_data$Value) && row_data$Value == "NaN") {
              "NaN"
            } else {
              as.character(row_data$Value)
            }
          } else {
            "NaN"
          }
        )
      }

      return(weights_list)
    },

    #' description Parse training context data
    #' param raw_context Raw training context data from API
    #' return MLTrainingContext object or NULL
    parse_training_context = function(raw_context) {
      if (is.null(raw_context)) {
        return(NULL)
      }

      validation_data <-
        if (!is.null(raw_context$ValidationData)) {
          raw_context$ValidationData
        } else {
          NULL
        }

      test_data <-
        if (!is.null(raw_context$TestData)) {
          raw_context$TestData
        } else {
          NULL
        }

      test_data_fraction <-
        if (!is.null(raw_context$TestDataFraction)) {
          as.numeric(raw_context$TestDataFraction)
        } else {
          NULL
        }

      validation_data_fraction <-
        if (!is.null(raw_context$ValidationDataFraction)) {
          as.numeric(raw_context$ValidationDataFraction)
        } else {
          NULL
        }

      return(MLTrainingContext$new(
        validation_data = validation_data,
        test_data = test_data,
        test_data_fraction = test_data_fraction,
        validation_data_fraction = validation_data_fraction
      ))
    }
  )
)
