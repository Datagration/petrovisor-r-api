context("Machine Learning Tests")

test_that("MLModel instantiation and conversion works", {
  table_formula <- 'Table "TestData"
    Column "feature1" in "unit"
      "test feature 1" in "unit"
    End Column
    Column "target" in "value"
      "target value" in "value" 
    End Column
  End Table'

  model <- MLModel$new(
    name = "Test Simple Model",
    table_formula = table_formula,
    label_column_name = "target",
    description = "Simple test model"
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "TestScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "TestScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "TestEntities",
                              entities = list(),
                              formula = 'Entity Set "TestEntities"
  End Set')

  # Set training options with scope and entity_set
  model$set_training_options(scope, entity_set)

  # Test toList conversion
  listed <- model$toList()

  expect_true(is.list(listed))
  expect_equal(listed$Name, "Test Simple Model")
  expect_equal(listed$Type, "Regression")
  expect_equal(listed$TableFormula, table_formula)
  # Context formula should be generated
  expect_true(!is.null(listed$ContextFormula))
  expect_equal(listed$LabelColumnName, "target")
  expect_equal(listed$Description, "Simple test model")
  expect_equal(listed$IsModelPerEntity, FALSE)
  expect_equal(listed$TestDataMode, "Union")
  expect_equal(listed$ValidationDataMode, "Union")
  expect_true(is.list(listed$TrainingOptions))
  expect_equal(listed$TrainingOptions$OptimizationMetric, "RSquared")
})


test_that("MLModel neural network options work", {
  model <- MLModel$new(
    name = "Neural Network Test",
    table_formula = "Table \"NN\" ... End Table",
    label_column_name = "output"
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "NNScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "NNScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "NNEntities",
                              entities = list(),
                              formula = 'Entity Set "NNEntities"
  End Set')

  # Set training options first
  model$set_training_options(scope, entity_set)

  # Configure neural network
  model$set_neural_network_options(
    library = "TensorFlow",
    optimizer = "Adam",
    learning_rate = 0.001,
    epochs = 100,
    batch_size = 64,
    dense_layers = 3,
    neurons = 128
  )

  expect_equal(model$training_options$custom_training_type, "NeuralNetwork")
  expect_true(!is.null(model$training_options$custom_training_options))
  expect_true(!is.null(model$
                         training_options$
                         custom_training_options$
                         neural_network_training_options))

  nn_opts <- model$training_options$
    custom_training_options$
    neural_network_training_options
  expect_equal(nn_opts$library, "TensorFlow")
  expect_equal(nn_opts$optimizer, "Adam")
  expect_equal(nn_opts$learning_rate, 0.001)
  expect_equal(nn_opts$epochs, 100)
  expect_equal(nn_opts$batch_size, 64)
  expect_equal(nn_opts$number_of_dense_layers, 3)
  expect_equal(nn_opts$number_of_neurons, 128)

  # Test default values for new parameters
  expect_null(nn_opts$model_path)
  expect_equal(nn_opts$transfer_learning, FALSE)
  expect_equal(nn_opts$hyperparameter_tuning, FALSE)
  expect_null(nn_opts$features_mapping)
  expect_null(nn_opts$sequence_window_size)
})

test_that("MLModel neural network options with all new parameters work", {
  model <- MLModel$new(
    name = "NN Full Test",
    type = "Regression",
    table_formula = "Table \"Production\" ... End Table",
    label_column_name = "output"
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "FullNNScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "FullNNScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "FullNNEntities",
                              entities = list(),
                              formula = 'Entity Set "FullNNEntities"
  End Set')

  # Set training options first
  model$set_training_options(scope, entity_set)

  # Configure neural network with all 18 parameters including new ones
  model$set_neural_network_options(
    library = "PyTorch",
    model_path = "/models/pretrained.pth",
    transfer_learning = TRUE,
    hyperparameter_tuning = TRUE,
    features_mapping = list(pressure = "input_pressure",
                            temperature = "input_temp"),
    optimizer = "AdaGrad",
    learning_rate = 0.01,
    epochs = 150,
    batch_size = 128,
    sequence_window_size = 30,
    activation_function = "LeakyReLU",
    dense_layers = 4,
    neurons = 256,
    conv1d_layers = 2,
    conv1d_filters = 64,
    conv1d_kernel_size = 5,
    lstm_layers = 3,
    lstm_neurons = 128
  )

  expect_equal(model$training_options$custom_training_type, "NeuralNetwork")
  nn_opts <-
    model$
    training_options$
    custom_training_options$
    neural_network_training_options

  # Test all 18 fields
  expect_equal(nn_opts$library, "PyTorch")
  expect_equal(nn_opts$model_path, "/models/pretrained.pth")
  expect_equal(nn_opts$transfer_learning, TRUE)
  expect_equal(nn_opts$hyperparameter_tuning, TRUE)
  expect_equal(nn_opts$features_mapping$pressure, "input_pressure")
  expect_equal(nn_opts$features_mapping$temperature, "input_temp")
  expect_equal(nn_opts$optimizer, "AdaGrad")
  expect_equal(nn_opts$learning_rate, 0.01)
  expect_equal(nn_opts$epochs, 150)
  expect_equal(nn_opts$batch_size, 128)
  expect_equal(nn_opts$sequence_window_size, 30)
  expect_equal(nn_opts$activation_function, "LeakyReLU")
  expect_equal(nn_opts$number_of_dense_layers, 4)
  expect_equal(nn_opts$number_of_neurons, 256)
  expect_equal(nn_opts$number_of_conv1d_layers, 2)
  expect_equal(nn_opts$number_of_conv1d_filters, 64)
  expect_equal(nn_opts$conv1d_kernel_size, 5)
  expect_equal(nn_opts$number_of_lstm_layers, 3)
  expect_equal(nn_opts$number_of_lstm_neurons, 128)
})

test_that("MLModel custom training options work", {
  model <- MLModel$new(
    name = "Custom Training Full Test",
    type = "BinaryClassification",
    table_formula = "Table \"CustomTest\" ... End Table",
    label_column_name = "category"
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "CustomScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "CustomScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "CustomEntities",
                              entities = list(),
                              formula = 'Entity Set "CustomEntities"
  End Set')

  # Set training options first
  model$set_training_options(scope, entity_set)

  # Test GAM with comprehensive options
  model$set_custom_training(
    training_type = "Gam",
    enable_pruning = TRUE,
    entropy_coefficient = 0.25,
    learning_rate = 0.05,
    maximum_bin_count_per_feature = 200,
    minimum_example_count_per_leaf = 10,
    max_iterations = 1500,
    pruning_metrics = "LeastAbsoluteDeviation"
  )

  expect_equal(model$training_options$custom_training_type, "Gam")
  custom_opts <- model$training_options$custom_training_options

  # Test that custom_training_options is an MLCustomTrainingOptions R6 instance
  expect_true(inherits(custom_opts, "MLCustomTrainingOptions"))

  # Test all GAM-specific parameters using R6 field names
  expect_equal(custom_opts$enable_pruning, TRUE)
  expect_equal(custom_opts$entropy_coefficient, 0.25)
  expect_equal(custom_opts$learning_rate, 0.05)
  expect_equal(custom_opts$maximum_bin_count_per_feature, 200)
  expect_equal(custom_opts$minimum_example_count_per_leaf, 10)
  expect_equal(custom_opts$number_of_iterations, 1500)
  expect_equal(custom_opts$pruning_metrics, "LeastAbsoluteDeviation")

  # Test Randomized PCA with comprehensive options
  model$set_custom_training(
    training_type = "RandomizedPca",
    rank = 75,
    oversampling = 15,
    ensure_zero_mean = FALSE
  )

  expect_equal(model$training_options$custom_training_type, "RandomizedPca")
  custom_opts_pca <- model$training_options$custom_training_options

  expect_true(inherits(custom_opts_pca, "MLCustomTrainingOptions"))

  # Test PCA-specific parameters using R6 field names
  expect_equal(custom_opts_pca$rank, 75)
  expect_equal(custom_opts_pca$oversampling, 15)
  expect_equal(custom_opts_pca$ensure_zero_mean, FALSE)
})

test_that("MLModel hyperparameter configuration works", {
  model <- MLModel$new(
    name = "Hyperparameter Test",
    table_formula = "Table \"HP\" ... End Table",
    label_column_name = "target"
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "HPScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "HPScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "HPEntities",
                              entities = list(),
                              formula = 'Entity Set "HPEntities"
  End Set')

  # Set training options first
  model$set_training_options(scope, entity_set)

  # Set custom hyperparameters
  model$set_trainer_hyperparameters("LightGbm", list(
    NumberOfLeaves = list(Init = 10, Min = 4, Max = 100),
    LearningRate = list(Init = 0.1, Min = 0.01, Max = 0.3),
    NumberOfTrees = list(Init = 50, Min = 10, Max = 200)
  ))

  expect_true(!is.null(model$training_options$trainer_hyperparameters$LightGbm))
  expect_equal(
    model$training_options$trainer_hyperparameters$LightGbm$NumberOfLeaves$Init,
    10
  )
  expect_equal(
    model$training_options$trainer_hyperparameters$LightGbm$LearningRate$Init,
    0.1
  )
  expect_equal(
    model$training_options$trainer_hyperparameters$LightGbm$NumberOfTrees$Init,
    50
  )

  # Test summary includes configured trainers
  summary <- model$get_summary()
  expect_true("LightGbm" %in% summary$configured_trainers)
})

test_that("MLModel preprocessing configuration works", {
  model <- MLModel$new(
    name = "Preprocessing Test",
    table_formula = "Table \"PP\" ... End Table",
    label_column_name = "target"
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "PPScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "PPScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "PPEntities",
                              entities = list(),
                              formula = 'Entity Set "PPEntities"
  End Set')

  # Set training options first
  model$set_training_options(scope, entity_set)

  # Enable specific preprocessing methods
  model$set_preprocessing(c("MinMax", "MeanVariance", "RobustScaling"))

  # Check that specified methods are enabled
  enabled_methods <- c()
  for (p in model$training_options$pre_processors) {
    if (p$is_enabled) {
      enabled_methods <- c(enabled_methods, p$normalization_type)
    }
  }

  expect_true("MinMax" %in% enabled_methods)
  expect_true("MeanVariance" %in% enabled_methods)
  expect_true("RobustScaling" %in% enabled_methods)
  expect_false("Binning" %in% enabled_methods)  # Should not be enabled

  # Test summary shows enabled preprocessing
  summary <- model$get_summary()
  expect_true("MinMax" %in% summary$preprocessing_enabled)
  expect_true("MeanVariance" %in% summary$preprocessing_enabled)
  expect_true("RobustScaling" %in% summary$preprocessing_enabled)
})

test_that("MLModel can be saved using service provider", {
  skip_if_not(exists("sp") && !is.null(sp), "Service provider not available")

  table_formula <- 'Table "SaveTest"
    Column "input1" in "m"
      "input feature 1" in "m"
    End Column
    Column "output1" in "bbl"
      "output target" in "bbl"
    End Column
  End Table'

  model <- MLModel$new(
    name = "Test Simple Save Model",
    table_formula = table_formula,
    label_column_name = "output1",
    description = "Test model for save operation"
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "SaveTestScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "SaveTestScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "SaveTestEntities",
                              entities = list(),
                              formula = 'Entity Set "SaveTestEntities"
  End Set')

  # Set training options with scope and entity_set
  model$set_training_options(scope, entity_set,
                             test_fraction = 0.2,
                             validation_fraction = 0.2,
                             time_to_train = 300)

  model$set_preprocessing(c("MinMax", "MeanVariance"))
  model$set_outlier_filtering("IQR")

  # Attempt to save - using "MLModel" type since that's what the API expects
  result <- sp$items$save("MLModel", model)

  expect_equal(result$status_code, 201)
})

test_that("MLModel can be retrieved using service provider", {
  skip_if_not(exists("sp") && !is.null(sp), "Service provider not available")

  # Create and save a model first
  table_formula <- 'Table "LoadTest"
    Column "load_input" in "unit"
      "load input feature" in "unit" 
    End Column
    Column "load_target" in "value"
      "load target value" in "value"
    End Column
  End Table'

  original_model <- MLModel$new(
    name = "Test Simple Load Model",
    table_formula = table_formula,
    label_column_name = "load_target",
    description = "Test model for load operation"
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "LoadTestScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "LoadTestScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "LoadTestEntities",
                              entities = list(),
                              formula = 'Entity Set "LoadTestEntities"
  End Set')

  # Configure some advanced options
  original_model$set_training_options(
    scope, entity_set,
    test_fraction = 0.2,
    validation_fraction = 0.2,
    time_to_train = 300
  )

  # Save the model using "MLModel" type (standard API type)
  save_result <- sp$items$save("MLModel", original_model)
  expect_equal(save_result$status_code, 201)

  # Load the model back using "MLModel" type
  retrieved_model <- sp$items$load("MLModel", "Test Simple Load Model")

  # The retrieved model will be an MLModel
  # (as that's what RepositoryService creates for "MLModel" type)
  expect_true(inherits(retrieved_model, "MLModel"))

  # For comparison, we'll compare the original toList with retrieved toList
  original_data <- original_model$toList()
  retrieved_data <- retrieved_model$toList()

  # Test key properties by comparing the list representations
  expect_equal(retrieved_data$Name, original_data$Name)
  expect_equal(retrieved_data$Type, original_data$Type)
  expect_equal(retrieved_data$TableFormula, original_data$TableFormula)
  expect_equal(retrieved_data$ContextFormula, original_data$ContextFormula)
  expect_equal(retrieved_data$LabelColumnName, original_data$LabelColumnName)
  expect_equal(retrieved_data$Description, original_data$Description)
})

test_that("MLModel native save/load using MLModel type works", {
  skip_if_not(exists("sp") && !is.null(sp), "Service provider not available")

  # Create model with advanced features
  table_formula <- 'Table "NativeTest"
    Column "native_input" in "kg"
      "native input feature" in "kg"
    End Column
    Column "native_output" in "m3"
      "native output target" in "m3"
    End Column
  End Table'

  original_model <- MLModel$new(
    name = "Test Native SimpleML Model",
    type = "BinaryClassification",
    table_formula = table_formula,
    label_column_name = "native_output",
    description = "Test native MLModel save/load",
    labels = list("native", "test", "binary")
  )

  # Create test scope and entity_set
  scope <- Scope$new(name = "NativeTestScope",
                     start = "2023-01-01T00:00:00.000Z",
                     end = "2023-12-31T00:00:00.000Z",
                     time_increment = "Daily",
                     depth_increment = "Meter",
                     start_depth = 0,
                     end_depth = 0,
                     formula = 'Scope "NativeTestScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

  entity_set <- EntitySet$new(name = "NativeTestEntities",
                              entities = list(),
                              formula = 'Entity Set "NativeTestEntities"
  End Set')

  # Configure advanced options
  original_model$set_training_options(
    scope, entity_set,
    test_fraction = 0.3,
    validation_fraction = 0.2,
    time_to_train = 900
  )

  original_model$set_preprocessing(c("RobustScaling", "BoxTidwell"))
  original_model$set_outlier_filtering("CooksDistance")

  original_model$set_custom_training(
    training_type = "Gam",
    enable_pruning = TRUE,
    learning_rate = 0.005
  )

  # Save using "MLModel" type to test the new RepositoryService case
  save_result <- sp$items$save("MLModel", original_model)
  expect_equal(save_result$status_code, 201)

  # Load using "MLModel" type to test the new case
  retrieved_model <- sp$items$load("MLModel", "Test Native SimpleML Model")

  # This should return a MLModel instance directly
  expect_true(inherits(retrieved_model, "MLModel"))

  # Test comprehensive properties
  expect_equal(retrieved_model$name, original_model$name)
  expect_equal(retrieved_model$type, original_model$type)
  expect_equal(retrieved_model$table_formula, original_model$table_formula)
  expect_equal(retrieved_model$context_formula, original_model$context_formula)
  expect_equal(retrieved_model$label_column_name,
               original_model$label_column_name)
  expect_equal(retrieved_model$description, original_model$description)

  # Compare labels as lists (handle potential type conversion)
  original_labels <- if (is.list(original_model$labels)) {
    original_model$labels
  } else {
    as.list(original_model$labels)
  }
  retrieved_labels <- if (is.list(retrieved_model$labels)) {
    retrieved_model$labels
  } else {
    as.list(retrieved_model$labels)
  }
  expect_equal(retrieved_labels, original_labels)
  expect_equal(retrieved_model$outlier_filters, original_model$outlier_filters)

  # Test training options
  expect_equal(retrieved_model$training_options$test_fraction, 0.3)
  expect_equal(retrieved_model$training_options$validation_fraction, 0.2)
  expect_equal(retrieved_model$training_options$time_to_train, 900)
  expect_equal(retrieved_model$training_options$custom_training_type, "Gam")
  expect_equal(retrieved_model$training_options$optimization_metric, "Accuracy")
})


test_that("MLModel handles all model types correctly", {
  # Test each supported model type
  model_types <- c("Regression", "BinaryClassification",
                   "MultipleClassification", "Clustering", "NaiveBayes",
                   "NaiveBayesCategorical", "SurvivalAnalysis")

  for (model_type in model_types) {
    if (model_type == "Clustering") {
      # Clustering doesn't need label column
      model <- MLModel$new(
        name = paste("Test", model_type, "Model"),
        type = model_type,
        table_formula = "Table \"Test\" ... End Table"
      )
      expect_null(model$label_column_name)
    } else if (model_type == "SurvivalAnalysis") {
      # Skip survival analysis for this basic test (requires survival_data)
      next
    } else {
      # Other types need label column
      model <- MLModel$new(
        name = paste("Test", model_type, "Model"),
        type = model_type,
        table_formula = "Table \"Test\" ... End Table",
        label_column_name = "target"
      )
      expect_equal(model$label_column_name, "target")
    }

    expect_equal(model$type, model_type)
    # Training options are NULL until set_training_options() is called
    expect_null(model$training_options)

    # Test that training options work when properly initialized
    scope <- Scope$new(name = "TypeTestScope",
                       start = "2023-01-01T00:00:00.000Z",
                       end = "2023-12-31T00:00:00.000Z",
                       time_increment = "Daily",
                       depth_increment = "Meter",
                       start_depth = 0,
                       end_depth = 0,
                       formula = 'Scope "TypeTestScope"
\tBetween #01/01/2023#
\tAnd #31/12/2023#
\tStep Daily
  End Scope')

    entity_set <- EntitySet$new(name = "TypeTestEntities",
                                entities = list(),
                                formula = 'Entity Set "TypeTestEntities"
  End Set')

    # Skip clustering models that don't need label column for set_training
    # options test
    if (model_type != "Clustering") {
      model$set_training_options(scope, entity_set)
      expect_true(inherits(model$training_options, "MLTrainingOptions"))

      # Check optimization metric is set correctly for each type
      expected_metrics <- list(
        "Regression" = "RSquared",
        "BinaryClassification" = "Accuracy",
        "MultipleClassification" = "Accuracy",
        "NaiveBayes" = "Accuracy",
        "NaiveBayesCategorical" = "Accuracy"
      )

      if (model_type %in% names(expected_metrics)) {
        expect_equal(model$training_options$optimization_metric,
                     expected_metrics[[model_type]])
      }
    }
  }
})

test_that("MLModel error handling works correctly", {
  # Test missing required parameters
  expect_error(MLModel$new(), "name is required")
  expect_error(MLModel$new(name = "Test"), "table_formula is required")

  # Test invalid model type
  expect_error(
    MLModel$new(
      name = "Test",
      type = "InvalidType",
      table_formula = "Table \"Test\" ... End Table",
      context_formula = "Context \"Test\" ... End Context"
    ),
    "type must be one of:"
  )

  # Test missing label column for regression
  expect_error(
    MLModel$new(
      name = "Test",
      type = "Regression",
      table_formula = "Table \"Test\" ... End Table"
    ),
    "label_column_name is required for model type 'Regression'"
  )

  # Test error when trying to use methods without setting scope/entity_set first
  model <- MLModel$new(
    name = "Test",
    table_formula = "Table \"Test\" ... End Table",
    label_column_name = "target"
  )

  expect_error(
    model$set_preprocessing(c("MinMax")),
    "You must call set_training_options"
  )

  expect_error(
    model$set_custom_training(training_type = "Gam"),
    "You must call set_training_options"
  )

  expect_error(
    model$set_neural_network_options(library = "TensorFlow"),
    "You must call set_training_options"
  )
})

test_that("Machine Learning real world example works", {
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
        paste0(target_signal$name, " [", target_signal$storage_unit_name, "]")),
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
                     formula = 'Scope "At Date"
\tBetween #01/01/2025#
\tAnd #01/01/2025#
\tStep Daily
  End Scope')

  model <- MLModel$new(
    name = "R Real World Example",
    type = "Regression",
    table_formula = table_formula,
    label_column_name = "target"
  )

  # set training options
  model$set_training_options(scope, entity_set,
                             test_fraction = 0.2,
                             validation_fraction = 0.2,
                             time_to_train = 300)

  # set outlier filters
  model$set_outlier_filtering("CooksDistance")

  # set preprocessing
  enabled_preprocessors <- c("MinMax", "BoxCox", "RobustScaling")
  model$set_preprocessing(enabled_preprocessors)

  # save the model
  save_result <- sp$items$save("MLModel", model)
  expect_equal(save_result$status_code, 201)

  # load the model
  retrieved_model <- sp$items$load("MLModel", "R Real World Example")
  expect_true(inherits(retrieved_model, "MLModel"))

  # Check all expected fields from the API response
  expect_equal(retrieved_model$name, "R Real World Example")
  expect_equal(retrieved_model$type, "Regression")
  expect_equal(retrieved_model$table_formula, table_formula)
  expect_equal(retrieved_model$is_model_per_entity, FALSE)
  expect_equal(retrieved_model$label_column_name, "target")
  expect_equal(retrieved_model$outlier_filters, "CooksDistance")
  expect_equal(retrieved_model$labels, list())
  expect_equal(retrieved_model$test_data_mode, "Union")
  expect_equal(retrieved_model$validation_data_mode, "Union")
  expect_type(retrieved_model$trained_models, "list")
  expect_length(retrieved_model$trained_models, 0)
  expect_equal(retrieved_model$trained_models, list())

  # Check TrainingOptions structure and some key values
  expect_type(retrieved_model$training_options, "environment")
  expect_equal(retrieved_model$training_options$time_to_train, 300)
  expect_equal(retrieved_model$training_options$num_clusters, 1)
  expect_equal(retrieved_model$training_options$num_cv_folds, 1)
  expect_equal(retrieved_model$training_options$trainers_to_exclude, list())
  expect_equal(retrieved_model$training_options$maximum_models_to_train, 1000)
  expect_type(retrieved_model$training_options$trainer_hyperparameters, "list")

  # Check PreProcessors structure and some key values
  preprocs <- retrieved_model$training_options$pre_processors
  expect_type(preprocs, "list")

  # Check that all preprocessor entries are proper MLPreProcessor instances
  for (pp in preprocs) {
    expect_true(inherits(pp, "MLPreProcessor"))
    expect_type(pp$normalization_type, "character")
    expect_true(inherits(pp$transformer_options, "MLTransformerOptions"))
    expect_type(pp$is_enabled, "logical")
  }

  # Check enabled pre-processors
  if (length(preprocs) > 0) {
    lapply(preprocs, function(pp) {
      if (pp$normalization_type %in% enabled_preprocessors) {
        expect_true(pp$is_enabled)
      } else {
        expect_false(pp$is_enabled)
      }
    })
  } else {
    skip("No preprocessors returned from API to check NormalizationType")
  }

  expect_type(retrieved_model$toList(), "list")

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
})

test_that("All created models are removed and removal is confirmed", {
  skip_if_not(exists("sp") && !is.null(sp), "Service provider not available")

  # Model names that were created in previous tests
  model_names <- c(
    "Test Simple Save Model",
    "Test Simple Load Model",
    "Test Native SimpleML Model",
    "R Real World Example"
  )

  # Delete all models created in previous tests
  deletion_results <- list()
  for (model_name in model_names) {
    # Capture any errors during deletion
    deletion_result <- tryCatch({
      sp$items$delete("MLModel", model_name)
      "success"
    }, error = function(e) {
      paste("Error:", e$message)
    })

    deletion_results[[model_name]] <- deletion_result
  }

  # Verify all deletions were successful (no errors)
  for (model_name in model_names) {
    expect_equal(deletion_results[[model_name]], "success",
                 info = paste("Failed to delete model:", model_name))
  }

  # Confirm removal by attempting to load each deleted model -
  # should fail with API error
  for (model_name in model_names) {
    expect_error(
      sp$items$load("MLModel", model_name),
      info = paste("Model should not be loadable after deletion:", model_name)
    )
  }
})
