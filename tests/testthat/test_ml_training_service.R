context("ModelTrainingService Tests")

test_that("ModelTrainingService train method works", {
  skip_if_not(exists("sp") && !is.null(sp), "Service provider not available")

  # Create 50 entities for machine learning test
  ml_entities <- list()
  entity_names <- c()

  for (i in 1:50) {
    entity_name <- paste0("MLTestEntity_", sprintf("%02d", i))
    entity_names <- c(entity_names, entity_name)

    entity <- Entity$new(name = entity_name,
                         entity_type_name = "Well",
                         alias = paste0("MLE", sprintf("%02d", i)),
                         is_opportunity = FALSE)

    result <- sp$items$save("Entity", entity)
    ml_entities[[i]] <- entity
  }

  # Create predictor signal (input feature)
  predictor_signal <- Signal$new(name = "ml test predictor signal",
                                 short_name = "mlpred",
                                 measurement_name = "Length",
                                 storage_unit_name = "m",
                                 aggregation_type = "Sum",
                                 container_aggregation_type = "Sum",
                                 signal_type = "Static",
                                 default_color = 3711337,
                                 default_line_type = "Solid",
                                 setting_name = NULL,
                                 labels = list(),
                                 description = "ML test predictor feature")

  result <- sp$items$save("Signal", predictor_signal)

  # Create target signal (output to predict)
  target_signal <- Signal$new(name = "ml test target signal",
                              short_name = "mltarget",
                              measurement_name = "Volume",
                              storage_unit_name = "m3",
                              aggregation_type = "Sum",
                              container_aggregation_type = "Sum",
                              signal_type = "Static",
                              default_color = 16711680,
                              default_line_type = "Solid",
                              setting_name = NULL,
                              labels = list(),
                              description = "ML test target to predict")

  result <- sp$items$save("Signal", target_signal)

  # Generate correlated data (target = 2.5 * predictor + noise + offset)
  set.seed(42) # For reproducible results
  predictor_values <- runif(50, min = 10, max = 100)
  noise <- rnorm(50, mean = 0, sd = 5)
  target_values <- 2.5 * predictor_values + 50 + noise

  # Create data frame for static data
  static_data <- data.frame(
    scenario = rep("", 50),
    entity = entity_names,
    stringsAsFactors = FALSE
  )
  static_data[[predictor_signal$name]] <- predictor_values
  static_data[[target_signal$name]] <- target_values

  # Save the correlated data
  result <- sp$data$save_signals(
    "StaticNumeric",
    static_data,
    signals = lapply(
      c(paste0(predictor_signal$name,
               " [",
               predictor_signal$storage_unit_name,
               "]"),
        paste0(target_signal$name,
               " [",
               target_signal$storage_unit_name,
               "]")),
      function(x) {
        sp$parse_signal(x)
      }
    )
  )

  # Create table formula using the new signals
  table_formula <- paste0('Table "ML Test Table"
    Column "predictor" in "', predictor_signal$storage_unit_name, '"
      "', predictor_signal$name, '" in "', predictor_signal$storage_unit_name, '"
    End Column
    Column "target" in "', target_signal$storage_unit_name, '"
      "', target_signal$name, '" in "', target_signal$storage_unit_name, '"
    End Column
  End Table')

  # Create context formula using the new entities
  entity_list <- paste(sapply(entity_names,
                              function(x) paste0('"', x, '"')),
                       collapse = '\r\n\t')

  entity_set <- EntitySet$new(name = "ML Test Entities",
                              entities = ml_entities,
                              formula = paste0('Entity Set "ML Test Entities"
\t', entity_list, '
  End Set'))

  scope <- Scope$new(name = "ML Test Scope",
                     start = "2025-01-01T00:00:00.000Z",
                     end = "2025-01-01T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 5000,
                     formula = 'Scope "ML Test Scope"
\tBetween #01/01/2025#
\tAnd #01/01/2025#
\tStep Daily
  End Scope')

  model <- MLModel$new(
    name = paste0("R Real World Example_",
                  as.integer(Sys.time()),
                  "_",
                  sample(10000:99999, 1)),
    type = "Regression",
    table_formula = table_formula,
    label_column_name = "target"
  )

  # Set training options with scope and entity_set
  model$set_training_options(scope, entity_set,
                             test_fraction = 0.2,
                             validation_fraction = 0.2,
                             time_to_train = 60)

  # set outlier filters
  model$set_outlier_filtering("CooksDistance")

  # set preprocessing
  enabled_preprocessors <- c("MinMax", "BoxCox", "RobustScaling")
  model$set_preprocessing(enabled_preprocessors, FALSE)

  # save the model
  save_result <- sp$items$save("MLModel", model)
  expect_equal(save_result$status_code, 201)

  # Train the model
  request <- sp$ml$train(model)

  expect_type(request, "list")
  expect_type(request$id, "character")

  # Get the status of the model training request
  status <- sp$ml$get_training_status(request$id)
  expect_type(status, "list")
  expect_true(status$status %in% c("Unknown",
                                   "Waiting",
                                   "Training",
                                   "Canceled",
                                   "Failed",
                                   "Ready",
                                   "Completed",
                                   "Canceling",
                                   "Starting"))

  # Wait for the model training to complete
  # (create a counter to avoid infinite loops)
  counter <- 0
  while (status$status != "Ready") {
    status <- sp$ml$get_training_status(request$id)
    Sys.sleep(30)
    counter <- counter + 1
    if (counter > 10) {
      stop("Model training timed out")
    }
  }

  # Get the results of the model training request
  results <- sp$ml$get_training_results(request$id)
  expect_true(inherits(results, "MLTrainingResults"))

  # Save the best model
  best_models <- results$get_best_models()

  save_result <- sp$ml$save_best_model(model, best_models, request$id)
  expect_type(save_result, "list")

  # Perform predictions
  predictions <- sp$ml$predict(model$name, predictor_values)
  expect_type(predictions, "list")
  expect_length(predictions, 50)
  expect_type(predictions[[1]], "list")
  expect_length(predictions[[1]], 3)
  expect_true("Value" %in% names(predictions[[1]]))
  expect_true("Score" %in% names(predictions[[1]]))
  expect_true("Probability" %in% names(predictions[[1]]))


  # Clean up - delete all created entities and signals
  # Delete signal data first
  result <- sp$data$delete_signals(
    ml_entities,
    c(predictor_signal$name, target_signal$name)
  )

  # Delete all created entities
  for (entity in ml_entities) {
    sp$items$delete("Entity", entity$name)
  }

  # Delete the signals
  sp$items$delete("Signal", predictor_signal$name)
  sp$items$delete("Signal", target_signal$name)

  # Delete the model
  sp$items$delete("MLModel", model$name)
})
