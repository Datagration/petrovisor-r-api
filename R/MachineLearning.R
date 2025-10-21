library("R6")

#' @title MLModel
#'
#' @description Class representing a PetroVisor ML model with minimal
#' required inputs and comprehensive API field handling.
#'
#' @export MLModel
#'
#' @field name The name of the ML model (required).
#' @field type The model's type (defaults to "Regression").
#' @field table_formula The model's table formula in P# language (required).
#' @field context_formula The model's context formula in P# language
#'   (optional - can be reconstructed from scope and entity_set).
#' @field description Optional description of the model.
#' @field label_column_name The label/target column name.
#'   Required for: "Regression", "BinaryClassification",
#'   "MultipleClassification", "NaiveBayes", "NaiveBayesCategorical".
#'   Not used for: "Clustering". Not required for: "SurvivalAnalysis".
#' @field survival_data Survival data configuration
#'   (required for SurvivalAnalysis).
#' @field labels List of string labels for the model.
#' @field is_model_per_entity Whether to create model per entity
#'   (default FALSE).
#' @field test_data_mode Test data mode (default "Union").
#' @field validation_data_mode Validation data mode (default "Union").
#' @field outlier_filters Outlier filtering method (default "None").
#' @field trained_models List of trained models.
#' @field is_depth_data Whether model uses depth data.
#' @field is_automatic Whether model was trained automatically.
#' @field trained Training timestamp.
#' @field is_reviewed Whether model has been reviewed.
#' @field include_incomplete_cases Whether to include incomplete cases.
#' @field data_provider_config Data provider configuration.
#' @field training_options Complete training options configuration with all
#'   sub-fields.
#' @field validation_scope_formula Validation scope formula.
#' @field validation_entity_set_formula Validation entity set formula.
#' @field test_scope_formula Test scope formula.
#' @field test_entity_set_formula Test entity set formula.
#'
#' @seealso
#' * [MlTrainingService] for training and managing ML models
#' * [MLTrainingResult] for training results
#' * [MLTrainingResults] for multiple training results
#' * [MLTrainingOptions] for training configuration
#' * [MLPreProcessor] for preprocessing methods
#' * [Scope] for defining training context
#' * [EntitySet] for entity filtering
#' * `vignette("machine-learning")` for ML workflow examples
#'
#' @examples
#' \dontrun{
#' # Create a minimal ML model (Regression)
#' table_formula <- 'Table "WellData"
#'   Column "feature1" in "m"
#'     "Production Feature" in "m"
#'   End Column
#'   Column "target" in "bbl"
#'     "Production Target" in "bbl"
#'   End Column
#' End Table'
#'
#' model <- MLModel$new(
#'   name = "My Production Model",
#'   table_formula = table_formula,
#'   label_column_name = "target",
#'   description = "Production prediction model"
#' )
#'
#' # Create scope and entity set
#' scope <- Scope$new(
#'   name = "TestScope",
#'   start = "2023-01-01T00:00:00.000Z",
#'   end = "2023-12-31T00:00:00.000Z",
#'   time_increment = "Daily",
#'   depth_increment = "Meter",
#'   start_depth = 0,
#'   end_depth = 0,
#'   formula = 'Scope "TestScope"
#'     Between #01/01/2023#
#'     And #31/12/2023#
#'     Step Daily
#'   End Scope'
#' )
#'
#' entity_set <- EntitySet$new(
#'   name = "TestEntities",
#'   entities = list(),
#'   formula = 'Entity Set "TestEntities"
#'   End Set'
#' )
#'
#' # Set training options with scope and entity_set (required)
#' model$set_training_options(scope, entity_set,
#'                           test_fraction = 0.2,
#'                           validation_fraction = 0.2,
#'                           time_to_train = 300)
#'
#' # Configure preprocessing
#' model$set_preprocessing(c("MinMax", "MeanVariance", "RobustScaling"))
#'
#' # Configure outlier filtering
#' model$set_outlier_filtering("CooksDistance")
#'
#' # Configure neural network (optional)
#' model$set_neural_network_options(
#'   library = "TensorFlow",
#'   optimizer = "Adam",
#'   learning_rate = 0.001,
#'   epochs = 100,
#'   batch_size = 64,
#'   dense_layers = 3,
#'   neurons = 128
#' )
#'
#' # Convert to list for API submission
#' api_data <- model$toList()
#'
#' # Save model using service provider
#' sp$items$save("MLModel", model)
#'
#' # Load model from API
#' retrieved_model <- sp$items$load("MLModel", "My Production Model")
#'
#' # Configure custom training (alternative to neural network)
#' model$set_custom_training(
#'   training_type = "Gam",
#'   enable_pruning = TRUE,
#'   learning_rate = 0.05,
#'   max_iterations = 1500
#' )
#' }
MLModel <- R6Class( # nolint: object_name_linter
  "MLModel",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(
    # Core required fields
    name = NULL,
    type = NULL,
    table_formula = NULL,
    context_formula = NULL,

    # Conditionally required fields
    label_column_name = NULL,
    survival_data = NULL,

    # Optional fields with defaults
    description = NULL,
    labels = NULL,
    is_model_per_entity = NULL,
    test_data_mode = NULL,
    validation_data_mode = NULL,
    outlier_filters = NULL,
    trained_models = NULL,
    is_depth_data = NULL,
    is_automatic = NULL,
    trained = NULL,
    is_reviewed = NULL,
    include_incomplete_cases = FALSE,
    data_provider_config = NULL,
    training_options = NULL,
    validation_scope_formula = NULL,
    validation_entity_set_formula = NULL,
    test_scope_formula = NULL,
    test_entity_set_formula = NULL,

    #' @description Create a new MLModel instance with minimal required
    #'   parameters.
    #'
    #' @param name The name of the ML model (required).
    #' @param type The model's type. One of "Regression",
    #'   "BinaryClassification", "MultipleClassification", "Clustering",
    #'   "NaiveBayes", "NaiveBayesCategorical", "SurvivalAnalysis".
    #'   Defaults to "Regression".
    #' @param table_formula The model's table formula in P# language (required).
    #' @param context_formula The model's context formula in P# language
    #'   (optional - can be reconstructed from scope and entity_set).
    #' @param description Optional description of the model.
    #' @param label_column_name The label/target column name. Required for most
    #'   model types except Clustering.
    #' @param survival_data Survival data configuration. Required for
    #'   SurvivalAnalysis.
    #' @param labels List of string labels for the model (default empty list).
    initialize = function(name,
                          table_formula,
                          type = "Regression",
                          context_formula = NULL,
                          description = NULL,
                          label_column_name = NULL,
                          survival_data = NULL,
                          labels = list()) {

      # Validate required parameters
      if (missing(name) || is.null(name) || !nzchar(name)) {
        stop("name is required and cannot be empty")
      }
      if (missing(table_formula) ||
            is.null(table_formula) ||
            !nzchar(table_formula)) {
        stop("table_formula is required and cannot be empty")
      }

      # Validate and set type
      allowed_types <- c("Regression", "BinaryClassification",
                         "MultipleClassification", "Clustering",
                         "NaiveBayes", "NaiveBayesCategorical",
                         "SurvivalAnalysis")
      if (!type %in% allowed_types) {
        stop(
          sprintf(
            "type must be one of: %s", paste(allowed_types, collapse = ", ")
          )
        )
      }

      # Set core fields
      self$name <- name
      self$type <- type
      self$table_formula <- table_formula
      self$context_formula <- context_formula
      self$description <- description
      self$labels <- labels

      # Validate type-specific requirements
      types_requiring_label <- c("Regression", "BinaryClassification",
                                 "MultipleClassification", "NaiveBayes",
                                 "NaiveBayesCategorical")

      if (type %in% types_requiring_label) {
        if (is.null(label_column_name) || !nzchar(label_column_name)) {
          stop(sprintf(
            "label_column_name is required for model type '%s'", type
          ))
        }
      }

      if (type == "Clustering") {
        if (!is.null(label_column_name) && nzchar(label_column_name)) {
          stop("label_column_name should not be provided for Clustering models")
        }
        if (!is.null(survival_data)) {
          stop("survival_data should not be provided for Clustering models")
        }
      }

      if (type == "SurvivalAnalysis") {
        if (is.null(survival_data)) {
          stop("survival_data is required for SurvivalAnalysis models")
        }
      }

      self$label_column_name <- label_column_name
      self$survival_data <- survival_data

      # Set default values for API compatibility
      self$is_model_per_entity <- FALSE
      self$test_data_mode <- "Union"
      self$validation_data_mode <- "Union"
      self$outlier_filters <- "None"
      self$trained_models <- list()

      # Initialize training options to NULL - will be set when scope and entity
      # set are provided
      self$training_options <- NULL

      # Update optimization metric based on model type
      private$update_optimization_metric_for_type(type)
    },

    #' @description Convert the MLModel instance to a list suitable for
    #'   API submission.
    #' @return A list representation of the MLModel object compatible
    #'   with the PetroVisor API.
    toList = function() {
      # Ensure context_formula is available
      context_formula <- self$context_formula
      if (is.null(context_formula) || !nzchar(context_formula)) {
        if (!is.null(self$training_options) &&
              !is.null(self$training_options$scope) &&
              !is.null(self$training_options$entity_set)) {
          context_formula <- private$generate_context_formula(
            self$training_options$scope,
            self$training_options$entity_set
          )
        } else {
          stop("context_formula is not set and cannot be reconstructed. ",
               "Please call set_training_options() first.")
        }
      }

      trained_models <- list()
      for (model in self$trained_models) {
        if (inherits(model, "MLNetModel")) {
          trained_models <- append(trained_models, list(model$toList()))
        } else {
          stop("trained_models must be a list of MLNetModel objects")
        }
      }

      result <- list(
        Name = self$name,
        Type = self$type,
        TableFormula = self$table_formula,
        ContextFormula = context_formula,
        IsModelPerEntity = self$is_model_per_entity,
        TestDataMode = self$test_data_mode,
        ValidationDataMode = self$validation_data_mode,
        OutlierFilters =
          if (self$outlier_filters == "None" || is.null(self$outlier_filters)) {
            list()
          } else {
            list(self$outlier_filters)
          },
        TrainedModels = trained_models,
        Labels = self$labels,
        LabelColumnName = self$label_column_name %||% ""
      )

      # Add optional fields if they are set
      if (!is.null(self$description)) {
        result$Description <- self$description
      }

      if (!is.null(self$is_depth_data)) {
        result$IsDepthData <- self$is_depth_data
      }

      if (!is.null(self$survival_data)) {
        result$SurvivalData <- self$survival_data
      }

      if (!is.null(self$validation_scope_formula)) {
        result$ValidationScopeFormula <- self$validation_scope_formula
      }

      if (!is.null(self$validation_entity_set_formula)) {
        result$ValidationEntitySetFormula <- self$validation_entity_set_formula
      }

      if (!is.null(self$test_scope_formula)) {
        result$TestScopeFormula <- self$test_scope_formula
      }

      if (!is.null(self$test_entity_set_formula)) {
        result$TestEntitySetFormula <- self$test_entity_set_formula
      }

      if (!is.null(self$training_options)) {
        result$TrainingOptions <- self$training_options$toList()
      }

      if (!is.null(self$is_automatic)) {
        result$IsAutomatic <- self$is_automatic
      }

      if (!is.null(self$trained)) {
        result$Trained <- self$trained
      }

      if (!is.null(self$is_reviewed)) {
        result$IsReviewed <- self$is_reviewed
      }

      if (!is.null(self$include_incomplete_cases)) {
        result$IncludeIncompleteCases <- self$include_incomplete_cases
      }

      if (!is.null(self$data_provider_config)) {
        result$DataProviderConfig <- if (is.list(self$data_provider_config)) {
          self$data_provider_config
        } else if (is.function(self$data_provider_config$toList)) {
          self$data_provider_config$toList()
        } else {
          self$data_provider_config
        }
      }

      return(result)
    },

    #' @description Configure training options for model validation and testing
    #' @param scope The training scope (Scope object, required)
    #' @param entity_set The training entity set (EntitySet object, required)
    #' @param test_fraction Fraction of data to use for testing (0.0-1.0)
    #' @param validation_fraction Fraction of data to use for validation
    #'   (0.0-1.0)
    #' @param time_to_train Maximum time to train in seconds (default 60)
    #' @param max_models Maximum number of models to train (default 1000)
    set_training_options = function(scope,
                                    entity_set,
                                    test_fraction = 0.0,
                                    validation_fraction = 0.0,
                                    time_to_train = 60,
                                    max_models = 1000) {
      if (is.null(scope) || is.null(entity_set)) {
        stop("Both scope and entity_set are required")
      }

      # Initialize training options with scope and entity_set if not already
      # created
      if (is.null(self$training_options)) {
        self$training_options <-
          private$create_default_training_options(scope, entity_set)
        # Update optimization metric for the current model type
        private$update_optimization_metric_for_type(self$type)
        # Generate context_formula from scope and entity_set if not already
        # provided
        if (is.null(self$context_formula) || !nzchar(self$context_formula)) {
          self$context_formula <-
            private$generate_context_formula(scope, entity_set)
        }
      }

      # Update training options
      self$training_options$test_fraction <- test_fraction
      self$training_options$validation_fraction <- validation_fraction
      self$training_options$time_to_train <- time_to_train
      self$training_options$maximum_models_to_train <- max_models
    },

    #' @description Enable specific data preprocessing methods
    #' @param methods Vector of preprocessing method names to enable.
    #'   Options: "MinMax", "MeanVariance", "LogMeanVariance", "Binning",
    #'   "SupervisedBinning", "RobustScaling", "LpNorm", "GlobalContrast",
    #'   "ProjectToPrincipalComponents", "ApproximatedKernelMap", "BoxCox",
    #'   "BoxTidwell"
    #' @param apply_pre_processors_before_training Whether to apply
    #'   preprocessors before training (default TRUE)
    set_preprocessing = function(methods = c(),
                                 apply_pre_processors_before_training = TRUE) {
      if (is.null(self$training_options)) {
        stop("You must call set_training_options() first",
             "to initialize training options")
      }

      if (!is.null(self$training_options$pre_processors)) {
        # Disable all preprocessors first
        for (i in seq_along(self$training_options$pre_processors)) {
          self$training_options$pre_processors[[i]]$is_enabled <- FALSE
        }

        # Enable specified methods
        if (length(methods) > 0) {
          for (i in seq_along(self$training_options$pre_processors)) {
            if (self$
                  training_options$
                  pre_processors[[i]]$
                  normalization_type %in% methods) {
              self$training_options$pre_processors[[i]]$is_enabled <- TRUE
            }
          }
        }
      }

      if (!is.null(apply_pre_processors_before_training)) {
        self$training_options$apply_pre_processors_before_training <-
          apply_pre_processors_before_training
      }
    },

    #' @description Set outlier filtering method
    #' @param method Outlier filtering method. Options: "None", "IQR",
    #'   "CooksDistance"
    set_outlier_filtering = function(method = c("None",
                                                "IQR",
                                                "CooksDistance")) {
      method <- match.arg(method)
      self$outlier_filters <- method
    },

    #' @description Configure neural network training options
    #' @param library Neural network library name (e.g., "TensorFlow")
    #' @param model_path Path to the neural network model file
    #' @param transfer_learning Whether to use transfer learning
    #' @param hyperparameter_tuning Whether to enable hyperparameter tuning
    #' @param features_mapping Features and label names mapping from internal
    #'   names to trained model names
    #' @param optimizer Optimizer to use (e.g. "Adam", "SGD")
    #' @param learning_rate Learning rate (0-1)
    #' @param epochs Number of training epochs
    #' @param batch_size Batch size for training
    #' @param sequence_window_size Series sequence window size
    #' @param activation_function Activation function
    #' @param dense_layers Number of dense layers
    #' @param neurons Number of neurons per layer
    #' @param conv1d_layers Number of Conv1D layers (for time series)
    #' @param conv1d_filters Number of Conv1D filters
    #' @param conv1d_kernel_size Conv1D kernel size
    #' @param lstm_layers Number of LSTM layers
    #' @param lstm_neurons Number of LSTM neurons
    set_neural_network_options = function(library = "TensorFlow",
                                          model_path = NULL,
                                          transfer_learning = FALSE,
                                          hyperparameter_tuning = FALSE,
                                          features_mapping = NULL,
                                          optimizer = "Adam",
                                          learning_rate = 0.001,
                                          epochs = 100,
                                          batch_size = 32,
                                          sequence_window_size = NULL,
                                          activation_function = "ReLU",
                                          dense_layers = 2,
                                          neurons = 64,
                                          conv1d_layers = NULL,
                                          conv1d_filters = NULL,
                                          conv1d_kernel_size = NULL,
                                          lstm_layers = NULL,
                                          lstm_neurons = NULL) {

      # Set custom training type to NeuralNetwork and create
      # custom training options
      if (is.null(self$training_options)) {
        stop("You must call set_training_options()",
             "first to initialize training options")
      }

      self$training_options$custom_training_type <- "NeuralNetwork"

      # Create neural network options using the proper R6 class
      neural_network_options <- MLNeuralNetworkTrainingOptions$new(
        library = library,
        model_path = model_path,
        transfer_learning = transfer_learning,
        hyperparameter_tuning = hyperparameter_tuning,
        features_mapping = features_mapping,
        optimizer = optimizer,
        learning_rate = learning_rate,
        epochs = epochs,
        batch_size = batch_size,
        sequence_window_size = sequence_window_size,
        activation_function = activation_function,
        number_of_dense_layers = dense_layers,
        number_of_neurons = neurons,
        number_of_conv1d_layers = conv1d_layers,
        number_of_conv1d_filters = conv1d_filters,
        conv1d_kernel_size = conv1d_kernel_size,
        number_of_lstm_layers = lstm_layers,
        number_of_lstm_neurons = lstm_neurons
      )

      # Create custom training options if they don't exist
      # (since we're moving from Auto)
      if (is.null(self$training_options$custom_training_options)) {
        self$training_options$custom_training_options <-
          MLCustomTrainingOptions$new(
            neural_network_training_options = neural_network_options
          )
      } else {
        # Update existing custom training options with neural network settings
        self$
          training_options$
          custom_training_options$
          neural_network_training_options <- neural_network_options
      }
    },

    #' @description Set custom training options for specific algorithms
    #' @param training_type Training type: "Auto", "Ols", "Gam",
    #'   "RandomizedPca", "NeuralNetwork", "OnnxModel"
    #' @param l2_regularization OLS: L2 regularization weight (0-1) for ridge
    #'   regression
    #' @param enable_pruning GAM: Enable post-training tree pruning to avoid
    #'   overfitting
    #' @param entropy_coefficient GAM: The entropy (regularization)
    #'   coefficient (0-1)
    #' @param learning_rate GAM: The learning rate (0-1)
    #' @param maximum_bin_count_per_feature GAM: Maximum number of distinct
    #'   values (bins) per feature (2-500)
    #' @param minimum_example_count_per_leaf GAM: Minimal number of data points
    #'   required to form a new tree leaf (1-100)
    #' @param max_iterations GAM: Total number of passes over the training data
    #'   (100-20000)
    #' @param pruning_metrics GAM: Metric to use for pruning
    #'   ("LeastAbsoluteDeviation" or "LeastSquares")
    #' @param rank Randomized PCA: The number of components in the PCA
    #' @param oversampling Randomized PCA: Oversampling parameter for randomized
    #'   PCA training (1-200)
    #' @param ensure_zero_mean Randomized PCA: If TRUE, data is centered to
    #'   have zero mean (defaults to TRUE)
    #' @param onnx_model ONNX model string (for ONNX models)
    set_custom_training = function(training_type = "Auto",
                                   l2_regularization = NULL,
                                   enable_pruning = NULL,
                                   entropy_coefficient = NULL,
                                   learning_rate = NULL,
                                   maximum_bin_count_per_feature = NULL,
                                   minimum_example_count_per_leaf = NULL,
                                   max_iterations = NULL,
                                   pruning_metrics = NULL,
                                   rank = NULL,
                                   oversampling = NULL,
                                   ensure_zero_mean = TRUE,
                                   onnx_model = NULL) {

      allowed_types <- private$get_custom_training_types(self$type)
      if (!training_type %in% allowed_types) {
        stop(
          sprintf(
            "training_type must be one of: %s",
            paste(allowed_types, collapse = ", ")
          )
        )
      }

      if (is.null(self$training_options)) {
        stop("You must call set_training_options()",
             "first to initialize training options")
      }

      self$training_options$custom_training_type <- training_type

      # Handle custom training options based on type
      if (training_type == "Auto") {
        # For Auto type, clear any existing custom training options
        self$training_options$custom_training_options <- NULL
      } else {
        # For non-Auto types, create or update custom training options

        # Preserve existing neural network training options if they exist
        existing_nn_options <- NULL
        if (!is.null(self$training_options$custom_training_options) &&
              inherits(self$training_options$custom_training_options,
                       "MLCustomTrainingOptions")) {
          existing_nn_options <-
            self$
            training_options$
            custom_training_options$
            neural_network_training_options
        }

        # Create new custom training options with specified parameters
        custom_options <- MLCustomTrainingOptions$new(
          l2_regularization = l2_regularization,
          enable_pruning = enable_pruning,
          entropy_coefficient = entropy_coefficient,
          learning_rate = learning_rate,
          maximum_bin_count_per_feature = maximum_bin_count_per_feature,
          minimum_example_count_per_leaf = minimum_example_count_per_leaf,
          number_of_iterations = max_iterations,
          pruning_metrics = pruning_metrics,
          rank = rank,
          oversampling = oversampling,
          ensure_zero_mean = ensure_zero_mean,
          onnx_model = onnx_model,
          neural_network_training_options = existing_nn_options
        )

        self$training_options$custom_training_options <- custom_options
      }

    },

    #' @description Configure hyperparameters for specific trainers
    #' @param trainer_name Name of the trainer (e.g. "FastForest", "LightGbm")
    #' @param hyperparameters Named list of hyperparameter values
    set_trainer_hyperparameters = function(trainer_name, hyperparameters) {
      if (is.null(self$training_options)) {
        stop("You must call set_training_options()",
             "first to initialize training options")
      }

      if (is.null(self$training_options$trainer_hyperparameters)) {
        self$training_options$trainer_hyperparameters <- list()
      }

      # Ensure hyperparameters have the required structure with all fields
      formatted_hyperparameters <- list()
      for (param_name in names(hyperparameters)) {
        param_value <- hyperparameters[[param_name]]

        # If the parameter is a simple list, ensure it has all required fields
        if (is.list(param_value)) {
          formatted_hyperparameters[[param_name]] <- list(
            Name = param_value$Name %||% param_name,
            DisplayName = param_value$DisplayName %||% param_name,
            Description = param_value$Description %||% "",
            Category = param_value$Category %||% "",
            Init = param_value$Init %||% 1,
            Min = param_value$Min %||% 0,
            Max = param_value$Max %||% 100,
            LogBase = param_value$LogBase %||% FALSE,
            ValueType = param_value$ValueType %||% "Float",
            DefaultValue =
              param_value$DefaultValue %||% (param_value$Init %||% 1),
            Candidates = param_value$Candidates %||% list()
          )
        } else {
          # If it's a simple value, create a basic structure
          formatted_hyperparameters[[param_name]] <- list(
            Name = param_name,
            DisplayName = param_name,
            Description = "",
            Category = "",
            Init = param_value,
            Min = 0,
            Max = 100,
            LogBase = FALSE,
            ValueType = if (is.numeric(param_value)) "Float" else "String",
            DefaultValue = param_value,
            Candidates = list()
          )
        }
      }

      self$training_options$trainer_hyperparameters[[trainer_name]] <-
        formatted_hyperparameters

    },

    #' @description Get a summary of the model configuration
    #' @return List containing key model configuration details
    get_summary = function() {
      summary <- list(
        name = self$name,
        type = self$type,
        has_label_column =
          !is.null(self$label_column_name) && nzchar(self$label_column_name),
        has_survival_data = !is.null(self$survival_data),
        test_fraction =
          if (!is.null(self$training_options)) {
            self$training_options$test_fraction
          } else {
            "Not set - call set_training_options() first"
          },
        validation_fraction =
          if (!is.null(self$training_options)) {
            self$training_options$validation_fraction
          } else {
            "Not set - call set_training_options() first"
          },
        optimization_metric =
          if (!is.null(self$training_options)) {
            self$training_options$optimization_metric
          } else {
            "Not set - call set_training_options() first"
          },
        outlier_filtering = self$outlier_filters,
        custom_training_type =
          if (!is.null(self$training_options)) {
            self$training_options$custom_training_type
          } else {
            "Not set - call set_training_options() first"
          },
        preprocessing_enabled =
          if (!is.null(self$training_options) &&
              !is.null(self$training_options$pre_processors)) {
            enabled_methods <- c()
            for (p in self$training_options$pre_processors) {
              if (p$is_enabled) {
                enabled_methods <- c(enabled_methods, p$normalization_type)
              }
            }
            enabled_methods
          } else {
            "Not set - call set_training_options() first"
          }
      )

      # Add neural network info if configured
      if (!is.null(self$training_options) &&
            inherits(self$training_options$custom_training_options,
                     "MLCustomTrainingOptions") &&
            !is.null(self$
                       training_options$
                       custom_training_options$
                       neural_network_training_options)) {
        nn_opts <-
          self$
          training_options$
          custom_training_options$
          neural_network_training_options
        summary$neural_network <- list(
          library = nn_opts$library,
          optimizer = nn_opts$optimizer,
          learning_rate = nn_opts$learning_rate,
          epochs = nn_opts$epochs,
          batch_size = nn_opts$batch_size,
          activation_function = nn_opts$activation_function,
          dense_layers = nn_opts$number_of_dense_layers,
          neurons = nn_opts$number_of_neurons
        )
      }

      # Add trainer hyperparameters info if configured
      if (!is.null(self$training_options) &&
            !is.null(self$training_options$trainer_hyperparameters) &&
            length(self$training_options$trainer_hyperparameters) > 0) {
        summary$configured_trainers <-
          names(self$training_options$trainer_hyperparameters)
      }

      return(summary)
    }
  ),

  private = list(
    # Cache for trainers and metrics from API
    trainers_metrics = NULL,

    #' description Create default training options with comprehensive field
    #'   structure using MLTrainingOptions R6 class
    #' param scope The training scope (required)
    #' param entity_set The training entity set (required)
    #' return MLTrainingOptions instance representing default training options
    create_default_training_options = function(scope, entity_set) {
      MLTrainingOptions$new(
        scope = scope,
        entity_set = entity_set,
        test_fraction = 0.0,
        test_latin_hypercube = FALSE,
        validation_fraction = 0.0,
        optimization_metric = "RSquared", # Default for Regression
        time_to_train = 60,
        num_clusters = 1,
        num_cv_folds = 1,
        trainers_to_exclude = list(),
        include_incomplete_cases = FALSE,
        custom_training_type = "Auto",
        custom_training_options = NULL, # No custom options for Auto type
        pre_processors = private$create_default_preprocessors(),
        apply_pre_processors_before_training = TRUE,
        tuner = "EciCostFrugal",
        maximum_models_to_train = 1000,
        survival_data = NULL,
        trainer_hyperparameters =
          private$create_default_trainer_hyperparameters()
      )
    },

    #' description Generate context formula from scope and entity_set
    #' param scope The training scope
    #' param entity_set The training entity set
    #' return Context formula string
    generate_context_formula = function(scope, entity_set) {
      # Generate a basic context formula using scope and entity_set formulas
      context_name <- paste0("GeneratedContext_",
                             gsub("[^A-Za-z0-9]",
                                  "_",
                                  self$name))

      context_formula <- paste0(
        'Context "', context_name, '"\n',
        '  Entity Set "', entity_set$name, '"\n',
        '  Scope "', scope$name, '"\n',
        'End Context\n',
        entity_set$formula, '\n',
        scope$formula
      )

      return(context_formula)
    },

    #' description Create default preprocessors structure
    #' return List of MLPreProcessor instances
    create_default_preprocessors = function() {
      list(
        MLPreProcessor$new(
          normalization_type = "MinMax",
          transformer_options = MLTransformerOptions$new(
            fix_zero = TRUE,
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 8
        ),
        MLPreProcessor$new(
          normalization_type = "MeanVariance",
          transformer_options = MLTransformerOptions$new(
            fix_zero = TRUE,
            use_cdf = FALSE,
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 7
        ),
        MLPreProcessor$new(
          normalization_type = "LogMeanVariance",
          transformer_options = MLTransformerOptions$new(
            fix_zero = TRUE,
            use_cdf = TRUE,
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 5
        ),
        MLPreProcessor$new(
          normalization_type = "Binning",
          transformer_options = MLTransformerOptions$new(
            fix_zero = TRUE,
            maximum_bin_count = 1024,
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 1
        ),
        MLPreProcessor$new(
          normalization_type = "SupervisedBinning",
          transformer_options = MLTransformerOptions$new(
            fix_zero = TRUE,
            maximum_bin_count = 1024,
            minimum_examples_per_bin = 10,
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 11
        ),
        MLPreProcessor$new(
          normalization_type = "RobustScaling",
          transformer_options = MLTransformerOptions$new(
            center_data = TRUE,
            quantile_min = 25,
            quantile_max = 75,
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 10
        ),
        MLPreProcessor$new(
          normalization_type = "LpNorm",
          transformer_options = MLTransformerOptions$new(
            ensure_zero_mean = TRUE,
            norm = "L1"
          ),
          is_enabled = FALSE,
          order = 6
        ),
        MLPreProcessor$new(
          normalization_type = "GlobalContrast",
          transformer_options = MLTransformerOptions$new(
            ensure_zero_mean = TRUE,
            ensure_unit_standard_deviation = TRUE,
            scale = 1,
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 4
        ),
        MLPreProcessor$new(
          normalization_type = "ProjectToPrincipalComponents",
          transformer_options = MLTransformerOptions$new(
            rank = 1,
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 9
        ),
        MLPreProcessor$new(
          normalization_type = "ApproximatedKernelMap",
          transformer_options = MLTransformerOptions$new(
            rank = 1,
            norm = "L2"
          ),
          is_enabled = FALSE
        ),
        MLPreProcessor$new(
          normalization_type = "BoxCox",
          transformer_options = MLTransformerOptions$new(
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 2
        ),
        MLPreProcessor$new(
          normalization_type = "BoxTidwell",
          transformer_options = MLTransformerOptions$new(
            norm = "L2"
          ),
          is_enabled = FALSE,
          order = 3
        )
      )
    },

    #' description Update optimization metric based on model type
    #' param type Model type
    update_optimization_metric_for_type = function(type) {
      metric_map <- list(
        "Regression" = "RSquared",
        "BinaryClassification" = "Accuracy",
        "MultipleClassification" = "Accuracy",
        "Clustering" = "DaviesBouldinIndex",
        "NaiveBayes" = "Accuracy",
        "NaiveBayesCategorical" = "Accuracy",
        "SurvivalAnalysis" = "ConcordanceIndex"
      )

      if (!is.null(self$training_options)) {
        self$training_options$optimization_metric <-
          if (!is.null(metric_map[[type]])) {
            metric_map[[type]]
          } else {
            "RSquared"
          }
      }
    },

    #' description Merge API training options with default structure
    #' param default_options Default training options structure
    #' param api_options Training options from API
    #' return Merged training options list
    merge_training_options = function(default_options, api_options) {
      if (is.null(api_options)) return(default_options)
      if (is.null(default_options)) return(api_options)

      # Start with defaults and override with API values
      result <- default_options

      # Map API field names to expected structure
      field_mappings <- list(
        TestFraction = "TestFraction",
        TestLatinHypercube = "TestLatinHypercube",
        ValidationFraction = "ValidationFraction",
        OptimizationMetric = "OptimizationMetric",
        TimeToTrain = "TimeToTrain",
        NumberOfClusters = "NumberOfClusters",
        NumberOfCrossValidationFolds = "NumberOfCrossValidationFolds",
        TrainersToExclude = "TrainersToExclude",
        Scope = "Scope",
        EntitySet = "EntitySet",
        IncludeIncompleteCases = "IncludeIncompleteCases",
        CustomTrainingType = "CustomTrainingType",
        CustomTrainingOptions = "CustomTrainingOptions",
        PreProcessors = "PreProcessors",
        ApplyPreProcessorsBeforeTraining = "ApplyPreProcessorsBeforeTraining",
        Tuner = "Tuner",
        MaximumModelsToTrain = "MaximumModelsToTrain",
        SurvivalData = "SurvivalData",
        TrainerHyperparameters = "TrainerHyperparameters"
      )

      # Update result with API values where present
      for (api_name in names(field_mappings)) {
        if (!is.null(api_options[[api_name]])) {
          result[[field_mappings[[api_name]]]] <- api_options[[api_name]]
        }
      }

      return(result)
    },

    #' description Create default trainer hyperparameters structure
    #' return List of default trainer hyperparameters
    create_default_trainer_hyperparameters = function() {
      list(
        FastForest = list(
          NumberOfTrees = list(
            Name = "NumberOfTrees",
            DisplayName = "Number Of Trees",
            Description =
              "Total number of decision trees to create in the ensemble.",
            Category = "",
            Init = 4,
            Min = 4,
            Max = 32768,
            LogBase = TRUE,
            ValueType = "Int",
            DefaultValue = 4,
            Candidates = list()
          ),
          NumberOfLeaves = list(
            Name = "NumberOfLeaves",
            DisplayName = "Number Of Leaves",
            Description =
              "The maxium number of leaves in each regression tree.",
            Category = "",
            Init = 4,
            Min = 4,
            Max = 32768,
            LogBase = TRUE,
            ValueType = "Int",
            DefaultValue = 4,
            Candidates = list()
          ),
          FeatureFraction = list(
            Name = "FeatureFraction",
            DisplayName = "Feature Fraction",
            Description =
              paste("The fraction of features (chosen randomly)",
                    "to use on each iteration."),
            Category = "",
            Init = 1,
            Min = 2e-10,
            Max = 1,
            LogBase = FALSE,
            ValueType = "Float",
            DefaultValue = 1,
            Candidates = list()
          )
        ),
        LightGbm = list(
          NumberOfLeaves = list(
            Name = "NumberOfLeaves",
            DisplayName = "Number Of Leaves",
            Description = "The maximum number of leaves per tree.",
            Category = "",
            Init = 4,
            Min = 4,
            Max = 32768,
            LogBase = FALSE,
            ValueType = "Int",
            DefaultValue = 4,
            Candidates = list()
          ),
          LearningRate = list(
            Name = "LearningRate",
            DisplayName = "Learning Rate",
            Description =
              "Learning rate, use smaller numbers to prevent overfitting.",
            Category = "",
            Init = 1,
            Min = 2e-10,
            Max = 1,
            LogBase = TRUE,
            ValueType = "Float",
            DefaultValue = 1,
            Candidates = list()
          ),
          NumberOfTrees = list(
            Name = "NumberOfTrees",
            DisplayName = "Number Of Trees",
            Description = "Number of tress or boosting iterations.",
            Category = "",
            Init = 4,
            Min = 4,
            Max = 32768,
            LogBase = FALSE,
            ValueType = "Int",
            DefaultValue = 4,
            Candidates = list()
          )
        ),
        Ols = list(
          L2Regularization = list(
            Name = "L2Regularization",
            DisplayName = "L2 Regularization",
            Description =
              paste("L2 regularization weight. Adding L2 regularization",
                    "turns this algorithm into a form of ridge regression,",
                    "rather than ordinary least squares.",
                    "Suggested values = 1e-6, 0.1, 1."),
            Category = "",
            Init = 0.000001,
            Min = 0,
            Max = 1,
            LogBase = FALSE,
            ValueType = "Float",
            DefaultValue = 0.000001,
            Candidates = list()
          )
        )
      )
    },

    #' description Get trainers for a given type
    #' param type Model type
    #' return List of trainers for the given type
    get_trainers_for_type = function(type) {
      if (is.null(private$trainers_metrics)) {
        trainers_metrics <- super$get(route = "MLModels/TrainersAndMetrics",
                                      query = list(ModelType = type))
        private$trainers_metrics <- trainers_metrics
      }

      return(private$trainers_metrics)
    },

    get_custom_training_types = function(type) {
      if (type == "Regression") {
        return(c(
          "Auto",
          "Ols",
          "Gam",
          "NeuralNetwork",
          "OnnxModel"
        ))
      } else if (type == "BinaryClassification") {
        return(c(
          "Auto",
          "Gam",
          "RandomizedPca",
          "NeuralNetwork",
          "OnnxModel"
        ))
      } else if (type == "MultipleClassification") {
        return(c(
          "Auto",
          "GamOva",
          "BernoulliNaiveBayes",
          "NeuralNetwork",
          "OnnxModel"
        ))
      } else {
        return(character(0))
      }
    }
  )
)

#' @title MLDataProviderConfig
#'
#' @description Configuration for ML data providers.
#'
#' @export MLDataProviderConfig
#'
#' @field provider_type The data provider type (PSharp or ReferenceTable).
#' @field source_name The name of the data source.
#' @field storage_name The name of the data storage.
#' @field columns The configuration of the data columns. List of MLDataColumns.
#' @examples
#' \dontrun{
#' # Create a MLDataProviderConfig
#' MLDataProviderConfig$new(
#'   provider_type = "ReferenceTable",
#'   source_name = "My Source Table",
#'   storage_name = "My Target Table",
#'   columns = list()
#' )
#'}
MLDataProviderConfig <- R6Class( # nolint: object_name_linter
  "MLDataProviderConfig",
  public = list(
    provider_type = NULL,
    source_name = NULL,
    storage_name = NULL,
    columns = NULL,

    #' @description Create a new MLDataProviderConfig instance.
    #'
    #' @param provider_type The data provider type (PSharp or ReferenceTable).
    #' @param source_name The name of the data source.
    #' @param storage_name The name of the data storage.
    #' @param columns The configuration of the data columns. List of
    #' MLDataColumns.
    initialize = function(provider_type = c("PSharp", "ReferenceTable"),
                          source_name = NULL,
                          storage_name = NULL,
                          columns = list()) {
      self$provider_type <- match.arg(provider_type)
      self$source_name <- source_name
      self$storage_name <- storage_name
      self$columns <- columns
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    #' @return A list representation of the MLDataProviderConfig object
    #' compatible with the API.
    toList = function() {
      # create list from list of columns
      column_list <- list()
      if (!is.null(self$columns)) {
        for (i in seq_along(self$columns)){
          column_list[[i]] <- self$columns[[i]]$toList()
        }
      } else {
        column_list[[1]] <- ""
      }

      dl <- list(
        ProviderType = self$provider_type,
        SourceName = if (is.null(self$source_name)) "" else self$source_name,
        StorageName = if (is.null(self$storage_name)) "" else self$storage_name,
        Columns = column_list
      )

      return(dl)
    }
  )
)

#' @title MLTransformerOptions
#'
#' @description Options for ML transformers.
#'
#' @export MLTransformerOptions
#'
#' @field rank The dimension of the feature space to map the input to.
#' @field fix_zero Whether to map zero to zero, preserving sparsity.
#' @field maximum_bin_count Maximum number of bins (power of 2 recommended).
#' @field minimum_examples_per_bin Minimum number of examples per bin.
#' @field ensure_zero_mean If true, subtract mean from each value before
#' normalizing and use the raw input otherwise. Defaults to false for
#' MLNormalizationType.LpNorm, true for MLNormalizationType.GlobalContrast.
#' @field norm Type of norm to use to normalize each sample. The indicated norm
#' of the resulting vector will be normalized to one. One of \code{L2},
#' \code{StandardDeviation}, \code{L1}, \code{Infinity}.
#' @field center_data Whether to center the data around 0 by removing the
#' median.
#' @field quantile_min Quantile min used to scale the data.
#' @field quantile_max Quantile max used to scale the data.
#' @field ensure_unit_standard_deviation If true, the resulting vector's
#' standard deviation would be one. Otherwise, the resulting vector's L2-norm
#' would be one.
#' @field scale Scale features by this value.
#' @field use_cdf Whether to use CDF as the output. Defaults to false for
#' MLNormalizationType.MeanVariance, true for
#' MLNormalizationType.LogMeanVariance.
MLTransformerOptions <- R6Class( # nolint: object_name_linter
  "MLTransformerOptions",
  public = list(
    rank = NULL,
    fix_zero = NULL,
    maximum_bin_count = NULL,
    minimum_examples_per_bin = NULL,
    ensure_zero_mean = NULL,
    norm = NULL,
    center_data = NULL,
    quantile_min = NULL,
    quantile_max = NULL,
    ensure_unit_standard_deviation = NULL,
    scale = NULL,
    use_cdf = NULL,

    #' @description Create a new MLTransformerOptions instance.
    #'
    #' @param rank The dimension of the feature space to map the input to.
    #' @param fix_zero Whether to map zero to zero, preserving sparsity.
    #' @param maximum_bin_count Maximum number of bins (power of 2 recommended).
    #' @param minimum_examples_per_bin Minimum number of examples per bin.
    #' @param ensure_zero_mean If true, subtract mean from each value before
    #' normalizing and use the raw input otherwise. Defaults to false for
    #' MLNormalizationType.LpNorm, true for MLNormalizationType.GlobalContrast.
    #' @param norm Type of norm to use to normalize each sample. The indicated
    #' norm of the resulting vector will be normalized to one. One of \code{L2},
    #' \code{StandardDeviation}, \code{L1}, \code{Infinity}.
    #' @param center_data Whether to center the data around 0 by removing the
    #' median.
    #' @param quantile_min Quantile min used to scale the data.
    #' @param quantile_max Quantile max used to scale the data.
    #' @param ensure_unit_standard_deviation If true, the resulting vector's
    #' standard deviation would be one. Otherwise, the resulting vector's
    #' L2-norm would be one.
    #' @param scale Scale features by this value.
    #' @param use_cdf Whether to use CDF as the output. Defaults to false for
    #' MLNormalizationType.MeanVariance, true for
    #' MLNormalizationType.LogMeanVariance.
    initialize = function(rank = NULL,
                          fix_zero = NULL,
                          maximum_bin_count = NULL,
                          minimum_examples_per_bin = NULL,
                          ensure_zero_mean = NULL,
                          norm = c(NULL,
                                   "L2",
                                   "StandardDeviation",
                                   "L1",
                                   "Infinity"),
                          center_data = NULL,
                          quantile_min = NULL,
                          quantile_max = NULL,
                          ensure_unit_standard_deviation = NULL,
                          scale = NULL,
                          use_cdf = NULL) {
      self$rank <- rank
      self$fix_zero <- fix_zero
      self$maximum_bin_count <- maximum_bin_count
      self$minimum_examples_per_bin <- minimum_examples_per_bin
      self$ensure_zero_mean <- ensure_zero_mean
      self$norm <- match.arg(norm)
      self$center_data <- center_data
      self$quantile_min <- quantile_min
      self$quantile_max <- quantile_max
      self$ensure_unit_standard_deviation <- ensure_unit_standard_deviation
      self$scale <- scale
      self$use_cdf <- use_cdf
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    #' @return A list representation of the MLTransformerOptions object
    #' compatible with the API.
    toList = function() {
      dl <- list()

      if (!is.null(self$rank)) {
        dl$Rank <- self$rank
      }

      if (!is.null(self$fix_zero)) {
        dl$FixZero <- self$fix_zero
      }

      if (!is.null(self$maximum_bin_count)) {
        dl$MaximumBinCount <- self$maximum_bin_count
      }

      if (!is.null(self$minimum_examples_per_bin)) {
        dl$MinimumExamplesPerBin <- self$minimum_examples_per_bin
      }

      if (!is.null(self$ensure_zero_mean)) {
        dl$EnsureZeroMean <- self$ensure_zero_mean
      }

      if (!is.null(self$ensure_unit_standard_deviation)) {
        dl$EnsureUnitStandardDeviation <- self$ensure_unit_standard_deviation
      }

      if (!is.null(self$scale)) {
        dl$Scale <- self$scale
      }

      if (!is.null(self$use_cdf)) {
        dl$UseCdf <- self$use_cdf
      }

      if (!is.null(self$center_data)) {
        dl$CenterData <- self$center_data
      }

      if (!is.null(self$quantile_min)) {
        dl$QuantileMin <- self$quantile_min
      }

      if (!is.null(self$quantile_max)) {
        dl$QuantileMax <- self$quantile_max
      }

      if (!is.null(self$norm)) {
        dl$Norm <- self$norm
      }

      return(dl)
    }
  )
)

#' @title MLPreProcessor
#'
#' @description Represents a ML data pre-processor.
#'
#' @export MLPreProcessor
#'
#' @field normalization_type The type of the pre-processor.
#' @field transformer_options Options for the pre-processor. Object of type
#' MLTransformerOptions.
#' @field order Ordinal number.
#' @field is_enabled Whether the pre-processor is enabled.
MLPreProcessor <- R6Class( # nolint: object_name_linter
  "MLPreProcessor",
  public = list(
    normalization_type = NULL,
    transformer_options = NULL,
    order = NULL,
    is_enabled = NULL,

    #' @description Create a new MLPreProcessor instance.
    #'
    #' @param normalization_type The type of the pre-processor.
    #' @param transformer_options Options for the pre-processor. Object of type
    #' MLTransformerOptionsation.
    #' @param order Ordinal number.
    #' @param is_enabled Whether the pre-processor is enabled.
    initialize = function(normalization_type = c("MinMax", "MeanVariance",
                                                 "LogMeanVariance", "Binning",
                                                 "SupervisedBinning",
                                                 "RobustScaling", "LpNorm",
                                                 "GlobalContrast",
                                                 "ProjectToPrincipalComponents",
                                                 "ApproximatedKernelMap",
                                                 "BoxCox", "BoxTidwell"),
                          transformer_options = NULL,
                          order = NULL,
                          is_enabled = FALSE) {
      self$normalization_type <- match.arg(normalization_type)
      self$transformer_options <- transformer_options
      self$order <- order
      self$is_enabled <- is_enabled
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    #' @return A list representation of the MLPreProcessor object compatible
    #' with the API.
    toList = function() {
      dl <- list(
        NormalizationType = self$normalization_type,
        TransformerOptions = if (!is.null(self$transformer_options)) {
          self$transformer_options$toList()
        } else {
          list()
        },
        IsEnabled = self$is_enabled
      )

      if (!is.null(self$order)) {
        dl$Order <- self$order
      }

      return(dl)
    }
  )
)

#' @title MLNeuralNetworkTrainingOptions
#'
#' @description Represents ML neural network training options.
#'
#' @export MLNeuralNetworkTrainingOptions
#'
#' @field library The name of the neural network library.
#' @field model_path The path to the model.
#' @field transfer_learning Whether to use transfer learning.
#' @field hyperparameter_tuning Whether to tune hyperparameters.
#' @field features_mapping Features and label names mapping from internal names
#' to trained model names.
#' @field optimizer The optimizer to use.
#' @field learning_rate The learning rate used (value between 0 and 1).
#' @field epochs The number of training iterations.
#' @field batch_size The number of samples to use for mini-batch training.
#' @field sequence_window_size Series sequence window size.
#' @field activation_function The activation function.
#' @field number_of_dense_layers Number of dense layers.
#' @field number_of_neurons Number of neurons.
#' @field number_of_conv1d_layers Number of Conv1D layers.
#' @field number_of_conv1d_filters Number of Conv1D filters.
#' @field conv1d_kernel_size Conv1D kernel size.
#' @field number_of_lstm_layers Number of LSTM layers.
#' @field number_of_lstm_neurons Number of LSTM neurons.
MLNeuralNetworkTrainingOptions <- R6Class( # nolint: object_name_linter
  "MLNeuralNetworkTrainingOptions",
  public = list(
    library = NULL,
    model_path = NULL,
    transfer_learning = NULL,
    hyperparameter_tuning = NULL,
    features_mapping = NULL,
    optimizer = NULL,
    learning_rate = NULL,
    epochs = NULL,
    batch_size = NULL,
    sequence_window_size = NULL,
    activation_function = NULL,
    number_of_dense_layers = NULL,
    number_of_neurons = NULL,
    number_of_conv1d_layers = NULL,
    number_of_conv1d_filters = NULL,
    conv1d_kernel_size = NULL,
    number_of_lstm_layers = NULL,
    number_of_lstm_neurons = NULL,

    #' @description Create a new MLNeuralNetworkTrainingOptions instance.
    #'
    #' @param library The name of the neural network library.
    #' @param model_path The path to the model.
    #' @param transfer_learning Whether to use transfer learning.
    #' @param hyperparameter_tuning Whether to tune hyperparameters.
    #' @param features_mapping Features and label names mapping from internal
    #' names to trained model names.
    #' @param optimizer The optimizer to use.
    #' @param learning_rate The learning rate used (value between 0 and 1).
    #' @param epochs The number of training iterations.
    #' @param batch_size The number of samples to use for mini-batch training.
    #' @param sequence_window_size Series sequence window size.
    #' @param activation_function The activation function.
    #' @param number_of_dense_layers Number of dense layers.
    #' @param number_of_neurons Number of neurons.
    #' @param number_of_conv1d_layers Number of Conv1D layers.
    #' @param number_of_conv1d_filters Number of Conv1D filters.
    #' @param conv1d_kernel_size Conv1D kernel size.
    #' @param number_of_lstm_layers Number of LSTM layers.
    #' @param number_of_lstm_neurons Number of LSTM neurons.
    initialize = function(library = NULL,
                          model_path = NULL,
                          transfer_learning = NULL,
                          hyperparameter_tuning = NULL,
                          features_mapping = NULL,
                          optimizer = NULL,
                          learning_rate = NULL,
                          epochs = NULL,
                          batch_size = NULL,
                          sequence_window_size = NULL,
                          activation_function = NULL,
                          number_of_dense_layers = NULL,
                          number_of_neurons = NULL,
                          number_of_conv1d_layers = NULL,
                          number_of_conv1d_filters = NULL,
                          conv1d_kernel_size = NULL,
                          number_of_lstm_layers = NULL,
                          number_of_lstm_neurons = NULL) {
      self$library <- library
      self$model_path <- model_path
      self$transfer_learning <- transfer_learning
      self$hyperparameter_tuning <- hyperparameter_tuning
      self$features_mapping <- features_mapping
      self$optimizer <- optimizer
      self$learning_rate <- learning_rate
      self$epochs <- epochs
      self$batch_size <- batch_size
      self$sequence_window_size <- sequence_window_size
      self$activation_function <- activation_function
      self$number_of_dense_layers <- number_of_dense_layers
      self$number_of_neurons <- number_of_neurons
      self$number_of_conv1d_layers <- number_of_conv1d_layers
      self$number_of_conv1d_filters <- number_of_conv1d_filters
      self$conv1d_kernel_size <- conv1d_kernel_size
      self$number_of_lstm_layers <- number_of_lstm_layers
      self$number_of_lstm_neurons <- number_of_lstm_neurons
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    #' @return A list representation of the MLNeuralNetworkTrainingOptions
    #' object compatible with the API.
    toList = function() {
      dl <- list()

      # Only include non-NULL values to avoid API conversion errors
      if (!is.null(self$library)) {
        dl$Library <- self$library
      }
      if (!is.null(self$model_path)) {
        dl$ModelPath <- self$model_path
      }
      if (!is.null(self$transfer_learning)) {
        dl$TransferLearning <- self$transfer_learning
      }
      if (!is.null(self$hyperparameter_tuning)) {
        dl$HyperParameterTuning <- self$hyperparameter_tuning
      }
      if (!is.null(self$features_mapping)) {
        dl$FeaturesMapping <- self$features_mapping
      }
      if (!is.null(self$optimizer)) {
        dl$Optimizer <- self$optimizer
      }
      if (!is.null(self$learning_rate)) {
        dl$LearningRate <- self$learning_rate
      }
      if (!is.null(self$epochs)) {
        dl$Epochs <- self$epochs
      }
      if (!is.null(self$batch_size)) {
        dl$BatchSize <- self$batch_size
      }
      if (!is.null(self$sequence_window_size)) {
        dl$SequenceWindowSize <- self$sequence_window_size
      }
      if (!is.null(self$activation_function)) {
        dl$ActivationFunction <- self$activation_function
      }
      if (!is.null(self$number_of_dense_layers)) {
        dl$NumberOfDenseLayers <- self$number_of_dense_layers
      }
      if (!is.null(self$number_of_neurons)) {
        dl$NumberOfNeurons <- self$number_of_neurons
      }
      if (!is.null(self$number_of_conv1d_layers)) {
        dl$NumberOfConv1DLayers <- self$number_of_conv1d_layers
      }
      if (!is.null(self$number_of_conv1d_filters)) {
        dl$NumberOfConv1DFilters <- self$number_of_conv1d_filters
      }
      if (!is.null(self$conv1d_kernel_size)) {
        dl$Conv1DKernelSize <- self$conv1d_kernel_size
      }
      if (!is.null(self$number_of_lstm_layers)) {
        dl$NumberOfLstmLayers <- self$number_of_lstm_layers
      }
      if (!is.null(self$number_of_lstm_neurons)) {
        dl$NumberOfLstmNeurons <- self$number_of_lstm_neurons
      }

      return(dl)
    }
  )
)

#' @title MLCustomTrainingOptions
#'
#' @description Options for custom trainers.
#'
#' @export MLCustomTrainingOptions
#'
#' @field l2_regularization OLS: L2 regularization weight (0-1). Adding L2
#' regularization turns this algorithm into a form of ridge regression.
#' @field enable_pruning GAM: Enable post-training tree pruning to avoid
#' overfitting. Requires a validation set.
#' @field entropy_coefficient GAM: The entropy (regularization) coefficient
#' between 0 and 1.
#' @field learning_rate GAM: The learning rate (0-1).
#' @field maximum_bin_count_per_feature GAM: The maximum number of distinct
#' values (bins) per feature (2-500).
#' @field minimum_example_count_per_leaf GAM: The minimal number of data points
#' required to form a new tree leaf (1-100).
#' @field number_of_iterations GAM: Total number of passes over the training
#' data (100-20000).
#' @field pruning_metrics GAM: Determines what metric to use for pruning.
#' One of \code{"LeastAbsoluteDeviation"}, \code{"LeastSquares"}.
#' @field rank Randomized PCA: The number of components in the PCA.
#' @field oversampling Randomized PCA: Oversampling parameter for randomized
#' PCA training (1-200).
#' @field ensure_zero_mean Randomized PCA: If true, data is centered to have
#' zero mean. Defaults to true.
#' @field onnx_model Gets or sets the ONNX model (string, nullable).
#' @field neural_network_training_options Neural Network Training Options.
#' Object of type MLNeuralNetworkTrainingOptions.
MLCustomTrainingOptions <- R6Class( # nolint: object_name_linter
  "MLCustomTrainingOptions",
  public = list(
    l2_regularization = NULL,
    enable_pruning = NULL,
    entropy_coefficient = NULL,
    learning_rate = NULL,
    maximum_bin_count_per_feature = NULL,
    minimum_example_count_per_leaf = NULL,
    number_of_iterations = NULL,
    pruning_metrics = NULL,
    rank = NULL,
    oversampling = NULL,
    ensure_zero_mean = NULL,
    onnx_model = NULL,
    neural_network_training_options = NULL,

    #' @description Create a new MLCustomTrainingOptions instance.
    #'
    #' @param l2_regularization OLS: L2 regularization weight (0-1). Adding L2
    #' regularization turns this algorithm into a form of ridge regression.
    #' @param enable_pruning GAM: Enable post-training tree pruning to avoid
    #' overfitting. Requires a validation set.
    #' @param entropy_coefficient GAM: The entropy (regularization) coefficient
    #' between 0 and 1.
    #' @param learning_rate GAM: The learning rate (0-1).
    #' @param maximum_bin_count_per_feature GAM: The maximum number of distinct
    #' values (bins) per feature (2-500).
    #' @param minimum_example_count_per_leaf GAM: The minimal number of data
    #' points required to form a new tree leaf (1-100).
    #' @param number_of_iterations GAM: Total number of passes over the training
    #' data (100-20000).
    #' @param pruning_metrics GAM: Determines what metric to use for pruning.
    #' One of \code{"LeastAbsoluteDeviation"}, \code{"LeastSquares"}.
    #' @param rank Randomized PCA: The number of components in the PCA.
    #' @param oversampling Randomized PCA: Oversampling parameter for randomized
    #' PCA training (1-200).
    #' @param ensure_zero_mean Randomized PCA: If true, data is centered to have
    #' zero mean. Defaults to true.
    #' @param onnx_model Gets or sets the ONNX model (string, nullable).
    #' @param neural_network_training_options Neural Network Training Options.
    #' Object of type MLNeuralNetworkTrainingOptions.
    initialize = function(l2_regularization = NULL,
                          enable_pruning = NULL,
                          entropy_coefficient = NULL,
                          learning_rate = NULL,
                          maximum_bin_count_per_feature = NULL,
                          minimum_example_count_per_leaf = NULL,
                          number_of_iterations = NULL,
                          pruning_metrics = NULL,
                          rank = NULL,
                          oversampling = NULL,
                          ensure_zero_mean = TRUE,
                          onnx_model = NULL,
                          neural_network_training_options = NULL) {
      self$l2_regularization <- l2_regularization
      self$enable_pruning <- enable_pruning
      self$entropy_coefficient <- entropy_coefficient
      self$learning_rate <- learning_rate
      self$maximum_bin_count_per_feature <- maximum_bin_count_per_feature
      self$minimum_example_count_per_leaf <- minimum_example_count_per_leaf
      self$number_of_iterations <- number_of_iterations
      self$pruning_metrics <- pruning_metrics
      self$rank <- rank
      self$oversampling <- oversampling
      self$ensure_zero_mean <- ensure_zero_mean
      self$onnx_model <- onnx_model
      self$neural_network_training_options <- neural_network_training_options
    },

    #' @description Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    #' @return A list representation of the MLCustomTrainingOptions object
    #' compatible with the API.
    toList = function() {
      dl <- list()

      # Only include non-NULL values to avoid API conversion errors
      if (!is.null(self$l2_regularization)) {
        dl$L2Regularization <- self$l2_regularization
      }
      if (!is.null(self$enable_pruning)) {
        dl$EnablePruning <- self$enable_pruning
      }
      if (!is.null(self$entropy_coefficient)) {
        dl$EntropyCoefficient <- self$entropy_coefficient
      }
      if (!is.null(self$learning_rate)) {
        dl$LearningRate <- self$learning_rate
      }
      if (!is.null(self$maximum_bin_count_per_feature)) {
        dl$MaximumBinCountPerFeature <- self$maximum_bin_count_per_feature
      }
      if (!is.null(self$minimum_example_count_per_leaf)) {
        dl$MinimumExampleCountPerLeaf <- self$minimum_example_count_per_leaf
      }
      if (!is.null(self$number_of_iterations)) {
        dl$NumberOfIterations <- self$number_of_iterations
      }
      if (!is.null(self$pruning_metrics)) {
        dl$PruningMetrics <- self$pruning_metrics
      }
      if (!is.null(self$rank)) {
        dl$Rank <- self$rank
      }
      if (!is.null(self$oversampling)) {
        dl$Oversampling <- self$oversampling
      }
      if (!is.null(self$ensure_zero_mean)) {
        dl$EnsureZeroMean <- self$ensure_zero_mean
      }
      if (!is.null(self$onnx_model)) {
        dl$OnnxModel <- self$onnx_model
      }
      if (!is.null(self$neural_network_training_options)) {
        dl$NeuralNetworkTrainingOptions <-
          self$neural_network_training_options$toList()
      }

      return(dl)
    }
  )
)

#' @title MLTrainingOptions
#'
#' @description ML model training options.
#'
#' @export MLTrainingOptions
#'
#' @field test_fraction Test set fraction.
#' @field test_latin_hypercube Logical indicating whether the test set sampling
#'   is latin hypercube (if true) or uniform (otherwise).
#' @field validation_fraction Validation set fraction.
#' @field optimization_metric Name of the optimization metric.
#' @field time_to_train Maximal time to train in seconds.
#' @field num_clusters Number of clusters (when applicable).
#' @field num_cv_folds Number of cross validation folds.
#' @field trainers_to_exclude The names of the trainers (algorithms) to exclude.
#' @field scope The training scope. Object of type Scope.
#' @field entity_set The training entity set. Object of type EntitySet.
#' @field include_incomplete_cases Logical whether to include incomplete cases
#'   (rows with some features equal to null) for training.
#' @field custom_training_type Customized training type. One of Auto, Ols, Gam,
#'   RandomizedPca, GamOva, BernoulliNaiveBayes, OnnxModel, NeuralNetwork.
#' @field custom_training_options Options for custom trainer. Object of type
#'   MLCustomTrainingOptions.
#' @field pre_processors Data pre-processors. List of MLPreProcessor.
#' @field apply_pre_processors_before_training When to apply pre-processors:
#'   before (true) or after (false) training.
#' @field tuner Tuner name.
#' @field maximum_models_to_train Maximum models to train.
#' @field survival_data Names of the columns containing survival data.
#' @field trainer_hyperparameters Trainer hyperparameters.
MLTrainingOptions <- R6Class( # nolint: object_name_linter
  "MLTrainingOptions",
  public = list(
    test_fraction = NULL,
    test_latin_hypercube = NULL,
    validation_fraction = NULL,
    optimization_metric = NULL,
    time_to_train = NULL,
    num_clusters = NULL,
    num_cv_folds = NULL,
    trainers_to_exclude = NULL,
    scope = NULL,
    entity_set = NULL,
    include_incomplete_cases = FALSE,
    custom_training_type = NULL,
    custom_training_options = NULL,
    pre_processors = NULL,
    apply_pre_processors_before_training = NULL,
    tuner = NULL,
    maximum_models_to_train = NULL,
    survival_data = NULL,
    trainer_hyperparameters = NULL,

    #' @description Create a new MLTrainingOptions instance.
    #'
    #' @param test_fraction Test set fraction.
    #' @param test_latin_hypercube Logical indicating whether the test set
    #'   sampling is latin hypercube (if true) or uniform (otherwise).
    #' @param validation_fraction Validation set fraction.
    #' @param optimization_metric Name of the optimization metric.
    #' @param time_to_train Maximal time to train in seconds.
    #' @param num_clusters Number of clusters (when applicable).
    #' @param num_cv_folds Number of cross validation folds.
    #' @param trainers_to_exclude The names of the trainers (algorithms) to
    #'   exclude.
    #' @param scope The training scope. Object of type Scope (required).
    #' @param entity_set The training entity set. Object of type EntitySet
    #'   (required).
    #' @param include_incomplete_cases Logical whether to include incomplete
    #'   cases (rows with some features equal to null) for training.
    #' @param custom_training_type Customized training type. One of Auto, Ols,
    #'   Gam, RandomizedPca, GamOva, BernoulliNaiveBayes, OnnxModel,
    #'   NeuralNetwork.
    #' @param custom_training_options Options for custom trainer. Object of type
    #'   MLCustomTrainingOptions.
    #' @param pre_processors Data pre-processors. List of MLPreProcessor.
    #' @param apply_pre_processors_before_training When to apply pre-processors:
    #'   before (true) or after (false) training.
    #' @param tuner Tuner name.
    #' @param maximum_models_to_train Maximum models to train.
    #' @param survival_data Names of the columns containing survival data.
    #' @param trainer_hyperparameters Trainer hyperparameters.
    initialize = function(scope,
                          entity_set,
                          test_fraction = NULL,
                          test_latin_hypercube = FALSE,
                          validation_fraction = NULL,
                          optimization_metric = NULL,
                          time_to_train = 60,
                          num_clusters = 1,
                          num_cv_folds = 1,
                          trainers_to_exclude = NULL,
                          include_incomplete_cases = FALSE,
                          custom_training_type = "Auto",
                          custom_training_options = NULL,
                          pre_processors = NULL,
                          apply_pre_processors_before_training = TRUE,
                          tuner = "EciCostFrugal",
                          maximum_models_to_train = 1000,
                          survival_data = NULL,
                          trainer_hyperparameters = NULL) {

      # Validate required parameters
      if (missing(scope) || is.null(scope)) {
        stop("scope is required and cannot be NULL")
      }
      if (missing(entity_set) || is.null(entity_set)) {
        stop("entity_set is required and cannot be NULL")
      }

      # Validate numeric parameters
      if (!is.null(test_fraction) &&
            (!is.numeric(test_fraction) ||
               test_fraction < 0 || test_fraction > 1)) {
        stop("test_fraction must be a numeric value between 0 and 1")
      }
      if (!is.null(validation_fraction) &&
            (!is.numeric(validation_fraction) ||
               validation_fraction < 0 || validation_fraction > 1)) {
        stop("validation_fraction must be a numeric value between 0 and 1")
      }
      if (!is.numeric(time_to_train) || time_to_train <= 0) {
        stop("time_to_train must be a positive numeric value")
      }
      if (!is.numeric(num_clusters) || num_clusters < 1) {
        stop("num_clusters must be a positive integer")
      }
      if (!is.numeric(num_cv_folds) || num_cv_folds < 1) {
        stop("num_cv_folds must be a positive integer")
      }
      if (!is.numeric(maximum_models_to_train) || maximum_models_to_train < 1) {
        stop("maximum_models_to_train must be a positive integer")
      }

      # Validate logical parameters
      if (!is.logical(test_latin_hypercube)) {
        stop("test_latin_hypercube must be TRUE or FALSE")
      }
      if (!is.logical(include_incomplete_cases)) {
        stop("include_incomplete_cases must be TRUE or FALSE")
      }
      if (!is.logical(apply_pre_processors_before_training)) {
        stop("apply_pre_processors_before_training must be TRUE or FALSE")
      }

      # Validate custom training type
      allowed_custom_training_types <- c(
        "Auto", "Ols", "Gam", "RandomizedPca", "GamOva",
        "BernoulliNaiveBayes", "OnnxModel", "NeuralNetwork"
      )
      if (!is.null(custom_training_type)) {
        custom_training_type <-
          match.arg(custom_training_type, allowed_custom_training_types)
      }

      # Validate tuner
      allowed_tuners <- c(
        "SMAC", "GridSearch", "RandomSearch", "CostFrugal", "EciCostFrugal",
        "AutoZeroTuner"
      )
      if (!is.null(tuner)) {
        tuner <-
          match.arg(tuner, allowed_tuners)
      }

      # Validate custom_training_options
      if (!is.null(custom_training_options) &&
            !inherits(custom_training_options, "MLCustomTrainingOptions")) {
        stop("custom_training_options must be an MLCustomTrainingOptions ",
             "R6 object or NULL")
      }

      # Validate pre_processors
      if (!is.null(pre_processors)) {
        if (!is.list(pre_processors)) {
          stop("pre_processors must be a list or NULL")
        }
        for (i in seq_along(pre_processors)) {
          if (!inherits(pre_processors[[i]], "MLPreProcessor")) {
            stop(paste("pre_processors[[",
                       i,
                       "]] must be an MLPreProcessor R6 object"))
          }
        }
      }

      # Validate trainers_to_exclude
      if (!is.null(trainers_to_exclude) && !is.list(trainers_to_exclude)) {
        stop("trainers_to_exclude must be a list or NULL")
      }

      # Validate trainer_hyperparameters
      if (!is.null(trainer_hyperparameters) &&
            !is.list(trainer_hyperparameters)) {
        stop("trainer_hyperparameters must be a list or NULL")
      }

      self$test_fraction <- test_fraction
      self$test_latin_hypercube <- test_latin_hypercube
      self$validation_fraction <- validation_fraction
      self$optimization_metric <- optimization_metric
      self$time_to_train <- time_to_train
      self$num_clusters <- num_clusters
      self$num_cv_folds <- num_cv_folds
      self$trainers_to_exclude <- trainers_to_exclude
      self$scope <- scope
      self$entity_set <- entity_set
      self$include_incomplete_cases <- include_incomplete_cases
      self$custom_training_type <- custom_training_type
      self$custom_training_options <- custom_training_options
      self$pre_processors <- pre_processors
      self$apply_pre_processors_before_training <-
        apply_pre_processors_before_training
      self$tuner <- tuner
      self$maximum_models_to_train <- maximum_models_to_train
      self$survival_data <- survival_data
      self$trainer_hyperparameters <- trainer_hyperparameters
    },

    #' @description Convert the object to a list. This function is mainly used
    #'   by the RepositoryService to convert the objects to lists and then
    #'   call the web API.
    #' @return A list representation of the MLTrainingOptions object compatible
    #'   with the API.
    toList = function() {
      dl <- list(
        TestFraction = self$test_fraction,
        TestLatinHypercube = self$test_latin_hypercube,
        ValidationFraction = self$validation_fraction,
        OptimizationMetric = self$optimization_metric,
        TimeToTrain = self$time_to_train,
        NumberOfClusters = self$num_clusters,
        NumberOfCrossValidationFolds = self$num_cv_folds,
        TrainersToExclude = self$trainers_to_exclude,
        Scope = if (!is.null(self$scope)) self$scope$toList() else NULL,
        EntitySet = if (!is.null(self$entity_set))
          self$entity_set$toList() else NULL,
        IncludeIncompleteCases = self$include_incomplete_cases,
        CustomTrainingType = self$custom_training_type,
        CustomTrainingOptions = if (!is.null(self$custom_training_options))
          self$custom_training_options$toList() else NULL,
        PreProcessors = if (!is.null(self$pre_processors))
          lapply(self$pre_processors, function(x) x$toList()) else NULL,
        ApplyPreProcessorsBeforeTraining =
          self$apply_pre_processors_before_training,
        Tuner = self$tuner,
        MaximumModelsToTrain = self$maximum_models_to_train,
        SurvivalData = if (!is.null(self$survival_data))
          self$survival_data else NULL,
        TrainerHyperparameters = self$trainer_hyperparameters
      )

      return(dl)
    }
  )
)

#' @title MLTrainingNumericOutcome
#'
#' @description Class representing a numeric training outcome for machine
#'   learning models. Contains the name and value of a training metric.
#'
#' @field name The name of the outcome (required, string, max 255 chars).
#' @field value The value of the metric (numeric, optional).
#'
#' @export MLTrainingNumericOutcome
#'
MLTrainingNumericOutcome <- R6::R6Class( # nolint: object_name_linter
  "MLTrainingNumericOutcome",
  public = list(
    name = NULL,
    value = NULL,

    #' @description Create a new MLTrainingNumericOutcome instance.
    #' @param name The name of the outcome (required, string, max 255 chars).
    #' @param value The value of the metric (numeric, optional).
    initialize = function(name, value = NULL) {
      if (missing(name) ||
            is.null(name) ||
            !is.character(name) ||
            (nchar(name) == 0)) {
        stop("name is required and must be a non-empty string.")
      }
      if (nchar(name) > 255) {
        stop("name must not exceed 255 characters.")
      }
      self$name <- name
      if (!is.null(value)) {
        if (!is.numeric(value)) {
          value <- as.numeric(value)
        }
        self$value <- value
      }
    },

    #' @description Convert the object to a list for API compatibility.
    toList = function() {
      list(
        Name = self$name,
        Value = self$value
      )
    }
  )
)


#' @title MLTrainingFeature
#'
#' @description Represents a feature with importance and weights in ML training
#'   results.
#'
#' @export MLTrainingFeature
#'
#' @field name The name of the feature.
#' @field importance The importance value of the feature.
#' @field weights List of MLTrainingNumericOutcome objects.
MLTrainingFeature <- R6Class( # nolint: object_name_linter
  "MLTrainingFeature",
  public = list(
    name = NULL,
    importance = NULL,
    weights = NULL,

    #' @description Create a new MLTrainingFeature instance.
    #' @param name The name of the feature.
    #' @param importance The importance value of the feature.
    #' @param weights List of MLTrainingNumericOutcome objects.
    initialize = function(name = NULL, importance = NULL, weights = list()) {
      self$name <- name
      self$importance <- importance
      self$weights <- weights
    },

    #' @description Convert the object to a list for API compatibility.
    toList = function() {
      weights_list <- list()
      if (length(self$weights) > 0) {
        for (i in seq_along(self$weights)) {
          if (inherits(self$weights[[i]], "MLTrainingNumericOutcome")) {
            weights_list[[i]] <- self$weights[[i]]$toList()
          } else {
            weights_list[[i]] <- self$weights[[i]]
          }
        }
      }

      list(
        Name = self$name,
        Importance = self$importance,
        Weights = weights_list
      )
    }
  )
)

#' @title MLTrainingContext
#'
#' @description Represents ML training context information.
#'
#' @export MLTrainingContext
#'
#' @field validation_data Serialized validation data (byte format).
#' @field test_data Serialized test data (byte format).
#' @field test_data_fraction Test data fraction (nullable).
#' @field validation_data_fraction Validation data fraction (nullable).
MLTrainingContext <- R6Class( # nolint: object_name_linter
  "MLTrainingContext",
  public = list(
    validation_data = NULL,
    test_data = NULL,
    test_data_fraction = NULL,
    validation_data_fraction = NULL,

    #' @description Create a new MLTrainingContext instance.
    #' @param validation_data Serialized validation data (byte format).
    #' @param test_data Serialized test data (byte format).
    #' @param test_data_fraction Test data fraction (nullable).
    #' @param validation_data_fraction Validation data fraction (nullable).
    initialize = function(validation_data = NULL,
                          test_data = NULL,
                          test_data_fraction = NULL,
                          validation_data_fraction = NULL) {
      self$validation_data <- validation_data
      self$test_data <- test_data
      self$test_data_fraction <- test_data_fraction
      self$validation_data_fraction <- validation_data_fraction
    },

    #' @description Convert the object to a list for API compatibility.
    toList = function() {
      result <- list()

      if (!is.null(self$validation_data)) {
        result$ValidationData <- self$validation_data
      }

      if (!is.null(self$test_data)) {
        result$TestData <- self$test_data
      }

      if (!is.null(self$test_data_fraction)) {
        result$TestDataFraction <- self$test_data_fraction
      }

      if (!is.null(self$validation_data_fraction)) {
        result$ValidationDataFraction <- self$validation_data_fraction
      }

      return(result)
    }
  )
)

#' @title MLTrainingOutcome
#'
#' @description Represents the outcome metrics from ML training.
#'
#' @export MLTrainingOutcome
#'
#' @field is_best_model Whether this is the best model among others.
#' @field is_best_run Whether this is the best run (for cross validation folds).
#' @field group_by Group TrainingOutcome instances by the value of this
#'   property (for cross validation folds).
#' @field sequence Sequential number (the order this model was trained by
#'   auto-ML).
#' @field values List of training metric values measured on the validation
#'   set.
#' @field test_values List of test metric values measured on the test set.
#' @field is_best_model_test Whether this is the best among others, measured
#'   on the test set.
#' @field is_best_run_test Whether this is the best run (for cross validation
#'   folds), measured on the test set.
MLTrainingOutcome <- R6Class( # nolint: object_name_linter
  "MLTrainingOutcome",
  public = list(
    is_best_model = NULL,
    is_best_run = NULL,
    group_by = NULL,
    sequence = NULL,
    values = NULL,
    test_values = NULL,
    is_best_model_test = NULL,
    is_best_run_test = NULL,

    #' @description Create a new MLTrainingOutcome instance.
    #' @param is_best_model Whether this is the best model among others.
    #' @param is_best_run Whether this is the best run (for cross validation
    #'   folds).
    #' @param group_by Group TrainingOutcome instances by the value of this
    #'   property (for cross validation folds).
    #' @param sequence Sequential number (the order this model was trained by
    #'   auto-ML).
    #' @param values List of training metric values measured on the validation
    #'   set.
    #' @param test_values List of test metric values measured on the test set.
    #' @param is_best_model_test Whether this is the best among others,
    #'   measured on the test set.
    #' @param is_best_run_test Whether this is the best run (for cross
    #'   validation folds), measured on the test set.
    initialize = function(is_best_model = FALSE,
                          is_best_run = FALSE,
                          group_by = NULL,
                          sequence = NULL,
                          values = list(),
                          test_values = list(),
                          is_best_model_test = FALSE,
                          is_best_run_test = FALSE) {
      self$is_best_model <- is_best_model
      self$is_best_run <- is_best_run
      self$group_by <- group_by
      self$sequence <- sequence
      self$values <- values
      self$test_values <- test_values
      self$is_best_model_test <- is_best_model_test
      self$is_best_run_test <- is_best_run_test
    },

    #' @description Convert the object to a list for API compatibility.
    toList = function() {
      values_list <- list()
      if (length(self$values) > 0) {
        for (i in seq_along(self$values)) {
          if (inherits(self$values[[i]], "MLTrainingNumericOutcome")) {
            values_list[[i]] <- self$values[[i]]$toList()
          } else {
            values_list[[i]] <- self$values[[i]]
          }
        }
      }

      test_values_list <- list()
      if (length(self$test_values) > 0) {
        for (i in seq_along(self$test_values)) {
          if (inherits(self$test_values[[i]], "MLTrainingNumericOutcome")) {
            test_values_list[[i]] <- self$test_values[[i]]$toList()
          } else {
            test_values_list[[i]] <- self$test_values[[i]]
          }
        }
      }

      result <- list(
        IsBestModel = self$is_best_model,
        IsBestRun = self$is_best_run,
        Values = values_list,
        TestValues = test_values_list,
        IsBestModelTest = self$is_best_model_test,
        IsBestRunTest = self$is_best_run_test
      )

      if (!is.null(self$group_by)) {
        result$GroupBy <- self$group_by
      }

      if (!is.null(self$sequence)) {
        result$Sequence <- self$sequence
      }

      return(result)
    }
  )
)

#' @title MLTrainingResult
#'
#' @description Represents a single training result.
#'
#' @export MLTrainingResult
#'
#' @field entity_name Entity name (nullable).
#' @field trained_model Trained model (byte format).
#' @field training_context Model training context (MLTrainingContext object).
#' @field confusion_matrix Confusion matrix for classification models.
#' @field outcome Training metrics (MLTrainingOutcome object).
#' @field error Training error (nullable).
#' @field trainer_name Trainer name.
#' @field features List of MLTrainingFeature objects.
MLTrainingResult <- R6Class( # nolint: object_name_linter
  "MLTrainingResult",
  public = list(
    entity_name = NULL,
    trained_model = NULL,
    training_context = NULL,
    confusion_matrix = NULL,
    outcome = NULL,
    error = NULL,
    trainer_name = NULL,
    features = NULL,

    #' @description Create a new MLTrainingResult instance.
    #' @param entity_name Entity name (nullable).
    #' @param trained_model Trained model (byte format).
    #' @param training_context Model training context
    #'   (MLTrainingContext object).
    #' @param confusion_matrix Confusion matrix for classification models.
    #' @param outcome Training metrics (MLTrainingOutcome object).
    #' @param error Training error (nullable).
    #' @param trainer_name Trainer name.
    #' @param features List of MLTrainingFeature objects.
    initialize = function(entity_name = NULL,
                          trained_model = NULL,
                          training_context = NULL,
                          confusion_matrix = NULL,
                          outcome = NULL,
                          error = NULL,
                          trainer_name = NULL,
                          features = list()) {
      self$entity_name <- entity_name
      self$trained_model <- trained_model
      self$training_context <- training_context
      self$confusion_matrix <- confusion_matrix
      self$outcome <- outcome
      self$error <- error
      self$trainer_name <- trainer_name
      self$features <- features
    },

    #' @description Convert the object to a list for API compatibility.
    toList = function() {
      features_list <- list()
      if (length(self$features) > 0) {
        for (i in seq_along(self$features)) {
          if (inherits(self$features[[i]], "MLTrainingFeature")) {
            features_list[[i]] <- self$features[[i]]$toList()
          } else {
            features_list[[i]] <- self$features[[i]]
          }
        }
      }

      result <- list(
        TrainerName = self$trainer_name,
        TrainedModel = self$trained_model,
        Features = features_list
      )

      if (!is.null(self$entity_name)) {
        result$EntityName <- self$entity_name
      }

      if (!is.null(self$training_context)) {
        if (inherits(self$training_context, "MLTrainingContext")) {
          result$TrainingContext <- self$training_context$toList()
        } else if (is.list(self$training_context)) {
          result$TrainingContext <- self$training_context
        } else {
          result$TrainingContext <- self$training_context
        }
      }

      if (!is.null(self$confusion_matrix)) {
        result$ConfusionMatrix <- if (is.list(self$confusion_matrix)) {
          self$confusion_matrix
        } else if (is.function(self$confusion_matrix$toList)) {
          self$confusion_matrix$toList()
        } else {
          self$confusion_matrix
        }
      }

      if (!is.null(self$outcome)) {
        if (inherits(self$outcome, "MLTrainingOutcome")) {
          result$Outcome <- self$outcome$toList()
        } else {
          result$Outcome <- self$outcome
        }
      }

      if (!is.null(self$error)) {
        result$Error <- self$error
      }

      return(result)
    }
  )
)

#' @title MLTrainingResults
#'
#' @description Represents the complete ML training results collection.
#'
#' @export MLTrainingResults
#'
#' @field runs_metrics_group_by Training runs grouping (nullable).
#' @field results List of MLTrainingResult objects.
#' @field label_unit_name Unit name for the label.
#' @field status The training status (added by API wrapper).
#' @field request_id The training request ID (added by API wrapper).
MLTrainingResults <- R6Class( # nolint: object_name_linter
  "MLTrainingResults",
  public = list(
    runs_metrics_group_by = NULL,
    results = NULL,
    label_unit_name = NULL,
    status = NULL,
    request_id = NULL,

    #' @description Create a new MLTrainingResults instance.
    #' @param runs_metrics_group_by Training runs grouping (nullable).
    #' @param results List of MLTrainingResult objects.
    #' @param label_unit_name Unit name for the label.
    #' @param status The training status (added by API wrapper).
    #' @param request_id The training request ID (added by API wrapper).
    initialize = function(runs_metrics_group_by = NULL,
                          results = list(),
                          label_unit_name = NULL,
                          status = NULL,
                          request_id = NULL) {
      self$runs_metrics_group_by <- runs_metrics_group_by
      self$results <- results
      self$label_unit_name <- label_unit_name
      self$status <- status
      self$request_id <- request_id
    },

    #' @description Get the best models from the results.
    #' @return List of MLTrainingResult objects that are marked as best models.
    get_best_models = function() {
      best_models <- list()
      if (length(self$results) > 0) {
        for (result in self$results) {
          if (inherits(result, "MLTrainingResult") &&
                !is.null(result$outcome) &&
                !is.na(result$outcome$is_best_model) &&
                result$outcome$is_best_model) {
            best_models <- append(best_models, list(result))
          }
        }
      }
      return(best_models)
    },

    #' @description Convert the object to a list for API compatibility.
    toList = function() {
      results_list <- list()
      if (length(self$results) > 0) {
        for (i in seq_along(self$results)) {
          if (inherits(self$results[[i]], "MLTrainingResult")) {
            results_list[[i]] <- self$results[[i]]$toList()
          } else {
            results_list[[i]] <- self$results[[i]]
          }
        }
      }

      result <- list(
        Results = results_list
      )

      if (!is.null(self$runs_metrics_group_by)) {
        result$RunsMetricsGroupBy <- self$runs_metrics_group_by
      }

      if (!is.null(self$label_unit_name)) {
        result$LabelUnitName <- self$label_unit_name
      }

      # These fields are added by the API wrapper, not in the core JSON spec
      if (!is.null(self$status)) {
        result$Status <- self$status
      }

      if (!is.null(self$request_id)) {
        result$RequestId <- self$request_id
      }

      return(result)
    }
  )
)

#' @title MLNetModel
#'
#' @description Class representing a .NET ML model with comprehensive
#' API field handling for trained models.
#'
#' @export MLNetModel
#'
#' @field name The name of the ML.NET model (required, max 255 chars).
#' @field is_reviewed Whether trained model was reviewed.
#' @field metrics Training metrics measured on the validation set
#'   (list of MLTrainingNumericOutcome).
#' @field test_metrics Training metrics measured on the test set
#'   (list of MLTrainingNumericOutcome).
#' @field features Feature contributions, weights, biases for trained models
#'   (list of MLModelFeature).
#' @field trainer_name Algorithm name used for training (nullable).
#' @field trained_mlnet_model Trained model in byte format.
#' @field training_context Model training context (MLTrainingContext object).
#' @field confusion_matrix Confusion matrix for classification models.
#'
#' @examples
#' \dontrun{
#' # Create a MLNetModel instance
#' net_model <- MLNetModel$new(
#'   name = "My NetML Model"
#' )
#'
#' # Set metrics
#' net_model$set_metrics("Accuracy", 0.95, "F1Score", 0.92)
#'
#' # Set features
#' net_model$set_features(
#'   list(
#'     MLModelFeature$new(name = "feature1", importance = 0.8),
#'     MLModelFeature$new(name = "feature2", importance = 0.6)
#'   )
#' )
#'
#' # Convert to list for API submission
#' api_data <- net_model$toList()
#' }
MLNetModel <- R6Class( # nolint: object_name_linter
  "MLNetModel",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(
    # Core required fields
    name = NULL,

    # Optional fields
    is_reviewed = NULL,
    metrics = NULL,
    test_metrics = NULL,
    features = NULL,
    trainer_name = NULL,
    trained_mlnet_model = NULL,
    training_context = NULL,
    confusion_matrix = NULL,

    #' @description Create a new MLNetModel instance.
    #'
    #' @param name The name of the ML.NET model (required, max 255 chars).
    #' @param is_reviewed Whether trained model was reviewed.
    #' @param metrics Training metrics measured on the validation set.
    #' @param test_metrics Training metrics measured on the test set.
    #' @param features Feature contributions, weights, biases for trained
    #'   models.
    #' @param trainer_name Algorithm name used for training (nullable).
    #' @param trained_mlnet_model Trained model in byte format.
    #' @param training_context Model training context
    #'   (MLTrainingContext object).
    #' @param confusion_matrix Confusion matrix for classification models.
    initialize = function(name,
                          is_reviewed = NULL,
                          metrics = list(),
                          test_metrics = list(),
                          features = list(),
                          trainer_name = NULL,
                          trained_mlnet_model = NULL,
                          training_context = NULL,
                          confusion_matrix = NULL) {

      # Validate required parameters
      if (missing(name) || is.null(name) || !nzchar(name)) {
        stop("name is required and cannot be empty")
      }
      if (nchar(name) > 255) {
        stop("name must not exceed 255 characters")
      }

      # Validate logical parameters
      if (!is.null(is_reviewed) && !is.logical(is_reviewed)) {
        stop("is_reviewed must be TRUE, FALSE, or NULL")
      }

      # Validate that metrics are proper MLTrainingNumericOutcome objects
      if (!is.list(metrics)) {
        stop("metrics must be a list")
      }
      self$metrics <- metrics

      if (!is.list(test_metrics)) {
        stop("test_metrics must be a list")
      }
      self$test_metrics <- test_metrics

      # Validate features list (should contain MLModelFeature objects)
      if (!is.list(features)) {
        stop("features must be a list")
      }

      # Set core fields
      self$name <- name
      self$is_reviewed <- is_reviewed
      self$features <- features
      self$trainer_name <- trainer_name
      self$trained_mlnet_model <- trained_mlnet_model
      self$training_context <- training_context
      self$confusion_matrix <- confusion_matrix
    },

    #' @description Convert the MLNetModel instance to a list suitable for
    #'   API submission.
    #' @return A list representation of the MLNetModel object compatible with
    #'   the PetroVisor API.
    toList = function() {
      # Convert metrics to list format
      metrics_list <- list()
      for (metric in self$metrics) {
        if (inherits(metric, "MLTrainingNumericOutcome")) {
          metrics_list <- append(metrics_list, list(metric$toList()))
        } else {
          stop("metric must be a MLTrainingNumericOutcome object")
        }
      }

      # Convert test metrics to list format
      test_metrics_list <- list()
      for (metric in self$test_metrics) {
        if (inherits(metric, "MLTrainingNumericOutcome")) {
          test_metrics_list <- append(test_metrics_list, list(metric$toList()))
        } else {
          stop("test_metric must be a MLTrainingNumericOutcome object")
        }
      }

      # Convert features to list format
      features_list <- list()
      for (feature in self$features) {
        if (inherits(feature, "MLTrainingFeature")) {
          features_list <- append(features_list, list(feature$toList()))
        } else {
          stop("feature must be a MLTrainingFeature object")
        }
      }

      result <- list(
        Name = self$name,
        Metrics = metrics_list,
        TestMetrics = test_metrics_list,
        Features = features_list
      )

      # Add optional fields if they are set
      if (!is.null(self$is_reviewed)) {
        result$IsReviewed <- self$is_reviewed
      }

      if (!is.null(self$trainer_name)) {
        result$TrainerName <- self$trainer_name
      }

      if (!is.null(self$trained_mlnet_model)) {
        result$TrainedMLNETModel <- self$trained_mlnet_model
      }

      if (!is.null(self$training_context)) {
        result$TrainingContext <- self$training_context$toList()
      }

      if (!is.null(self$confusion_matrix)) {
        result$ConfusionMatrix <- self$confusion_matrix$toList()
      }

      return(result)
    }
  )
)


# Example usage code demonstrating all three main use cases

#' @examples
#' \dontrun{
#' # ===========================================================================
#' # EXAMPLE 1: Create a minimal regression model
#' # ===========================================================================
#'
#' regression_model <- MLModel$new(
#'   name = "Oil Production Prediction",
#'   table_formula = 'Table "WellData"
#'     Column "lateral_length" in "m"
#'       "lateral length" in "m"
#'     End Column
#'     Column "production" in "bbl"
#'       "oil production" in "bbl"
#'     End Column
#'   End Table',
#'   context_formula = 'Context "PredictionContext"
#'     Entity Set "ActiveWells"
#'     Scope "LastYear"
#'   End Context',
#'   label_column_name = "production",
#'   description = "Predicts daily oil production based on well characteristics"
#' )
#'
#' # Configure training options
#' regression_model$set_training_options(
#'   test_fraction = 0.2,
#'   validation_fraction = 0.2,
#'   time_to_train = 300,
#'   max_models = 50
#' )
#'
#' # Enable preprocessing
#' regression_model$set_preprocessing(c("MinMax", "MeanVariance"))
#'
#' # Set outlier filtering
#' regression_model$set_outlier_filtering("IQR")
#'
#' # Configure custom training for GAM algorithm with full options
#' regression_model$set_custom_training(
#'   training_type = "Gam",
#'   enable_pruning = TRUE,
#'   entropy_coefficient = 0.1,
#'   learning_rate = 0.01,
#'   maximum_bin_count_per_feature = 100,
#'   minimum_example_count_per_leaf = 5,
#'   max_iterations = 1000,
#'   pruning_metrics = "LeastSquares"
#' )
#'
#' # Alternative: Configure for Randomized PCA
#' # regression_model$set_custom_training(
#' #   training_type = "RandomizedPca",
#' #   rank = 50,
#' #   oversampling = 10,
#' #   ensure_zero_mean = TRUE
#' # )
#'
#' # Configure LightGBM hyperparameters
#' regression_model$set_trainer_hyperparameters("LightGbm", list(
#'   NumberOfLeaves = list(Init = 10, Min = 4, Max = 100),
#'   LearningRate = list(Init = 0.1, Min = 0.01, Max = 0.3),
#'   NumberOfTrees = list(Init = 100, Min = 10, Max = 1000)
#' ))
#'
#' # Get model summary
#' summary <- regression_model$get_summary()
#' print(summary)
#'
#' # Convert to list for API POST/PUT request
#' api_payload <- regression_model$toList()
#'
#' # The api_payload can now be sent to PetroVisor API endpoints:
#' # POST /PetroVisor/API/{workspace}/MLModels/{name}
#' # PUT /PetroVisor/API/{workspace}/MLModels/{name}
#'
#'
#' # ===========================================================================
#' # EXAMPLE 2: Neural Network Model with Deep Learning Configuration
#' # ===========================================================================
#'
#' neural_model <- MLModel$new(
#'   name = "Deep Learning Production Forecast",
#'   type = "Regression",
#'   table_formula = 'Table "TimeSeries"
#'     Column "pressure" in "psi"
#'       "reservoir pressure" in "psi"
#'     End Column
#'     Column "flow_rate" in "bbl/day"
#'       "oil flow rate" in "bbl/day"
#'     End Column
#'   End Table',
#'   context_formula = 'Context "DeepLearningContext"
#'     Entity Set "ProductionWells"
#'     Scope "LastTwoYears"
#'   End Context',
#'   label_column_name = "flow_rate",
#'   description =
#'     "Deep learning model for production forecasting with time series"
#' )
#'
#' # Configure neural network architecture
#' neural_model$set_neural_network_options(
#'   library = "TensorFlow",
#'   model_path = "/path/to/model",
#'   transfer_learning = TRUE,
#'   hyperparameter_tuning = FALSE,
#'   features_mapping = list(feature1 = "input_1"),
#'   optimizer = "Adam",
#'   learning_rate = 0.001,
#'   epochs = 200,
#'   batch_size = 64,
#'   sequence_window_size = 30,
#'   activation_function = "ReLU",
#'   dense_layers = 3,
#'   neurons = 128,
#'   conv1d_layers = 2,
#'   conv1d_filters = 64,
#'   conv1d_kernel_size = 3,
#'   lstm_layers = 2,
#'   lstm_neurons = 64
#' )
#'
#' # Set training options for neural network
#' neural_model$set_training_options(
#'   test_fraction = 0.2,
#'   validation_fraction = 0.2,
#'   time_to_train = 3600  # 1 hour for neural network training
#' )
#'
#' api_payload_neural <- neural_model$toList()
#'
#'
#' # ===========================================================================
#' # EXAMPLE 3: Create a clustering model (no label column needed)
#' # ===========================================================================
#'
#' clustering_model <- MLModel$new(
#'   name = "Well Performance Clustering",
#'   type = "Clustering",
#'   table_formula = 'Table "WellMetrics"
#'     Column "depth" in "ft"
#'       "total depth" in "ft"
#'     End Column
#'     Column "pressure" in "psi"
#'       "reservoir pressure" in "psi"
#'     End Column
#'   End Table',
#'   context_formula = 'Context "ClusteringContext"
#'     Entity Set "AllWells"
#'   End Context',
#'   description = "Groups wells by performance characteristics",
#'   labels = list("clustering", "wells", "performance")
#' )
#'
#' # Configure clustering-specific options
#' clustering_model$set_training_options(
#'   test_fraction = 0.0,  # No test set needed for clustering
#'   validation_fraction = 0.0,  # No validation set needed
#'   time_to_train = 120
#' )
#'
#' api_payload_clustering <- clustering_model$toList()
#'
#'
#' # ===========================================================================
#' # EXAMPLE 4: Load an existing model from the API
#' # ===========================================================================
#'
#' # Load an existing model from the API
#' retrieved_model <- sp$items$load("MLModel", "Retrieved Production Model")
#'
#' # Now you can work with the retrieved model
#' cat("Retrieved model:", retrieved_model$name, "\n")
#' cat("Model type:", retrieved_model$type, "\n")
#' cat("Is trained:", !is.null(retrieved_model$trained), "\n")
#'
#' # Get detailed summary of the retrieved model
#' model_summary <- retrieved_model$get_summary()
#' print(model_summary)
#'
#' # Switch to neural network training with more comprehensive options
#' retrieved_model$set_neural_network_options(
#'   library = "TensorFlow",
#'   transfer_learning = FALSE,
#'   hyperparameter_tuning = TRUE,
#'   optimizer = "AdaGrad",
#'   learning_rate = 0.01,
#'   epochs = 150,
#'   batch_size = 32,
#'   sequence_window_size = 20,
#'   dense_layers = 2,
#'   neurons = 96
#' )
#'
#' # Configure FastForest hyperparameters as alternative
#' retrieved_model$set_trainer_hyperparameters("FastForest", list(
#'   NumberOfTrees = list(Init = 50, Min = 10, Max = 200),
#'   FeatureFraction = list(Init = 0.8, Min = 0.5, Max = 1.0)
#' ))
#'
#' # Update preprocessing and training options
#' retrieved_model$set_preprocessing(c("RobustScaling", "BoxCox"))
#' retrieved_model$set_training_options(validation_fraction = 0.3,
#'                                      time_to_train = 1800)
#'
#' # Update description and resubmit if needed
#' retrieved_model$description <-
#'   "Enhanced model with neural network and custom hyperparameters"
#' updated_payload <- retrieved_model$toList()
#'
#' # Get final summary showing neural network configuration
#' final_summary <- retrieved_model$get_summary()
#' cat("Neural Network Config:", str(final_summary$neural_network), "\n")
#' cat("Configured Trainers:",
#'     paste(final_summary$configured_trainers, collapse = ", "), "\n")
#'
#' }