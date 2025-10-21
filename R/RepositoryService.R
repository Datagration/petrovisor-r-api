library("R6")

#' @title RepositoryService
#'
#' @description Provides access to PetroVisor item related functionalities.
#'
#' @details A new instance of this class will be created by the ServiceProvider
#' automatically.
#'
#' @seealso
#' Related classes:
#' * [ServiceProvider] for creating a service provider instance
#' * [DataServices] for loading and saving data using repository items
#'
#' Item classes that can be managed:
#' * [Entity] for well, field, reservoir entities
#' * [EntityType] for entity type definitions
#' * [EntitySet] for entity collections
#' * [Signal] for signal definitions
#' * [Unit] and [UnitMeasurement] for unit definitions
#' * [Hierarchy] for organizational structures
#' * [Scope] for entity and signal selections
#' * [Scenario] for data versions
#' * [Tag] and [TagEntry] for metadata
#' * [Workflow] for automated processes
#' * [RScript] and [PSharpScript] for scripts
#' * [MLModel] for machine learning models
#'
#' Vignettes:
#' * `vignette("repository-service")` for comprehensive repository operations
#' * `vignette("getting-started")` for basic usage
#'
#' @export RepositoryService
#'
#' @examples \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new("Host", 8095, "WorkspaceA", "UserX", "Password")
#'
#' # get the names of all available entities
#' entityNames <- sp$items$load_names("Entity")
#'
#' # delete an item (delete the hierarchy with the name "test")
#' sp$items$delete("Hierarchy", "test")
#'
#' # get an item by name
#' well01 <- sp$items$load("Entity", "Well01")
#'
#' # add or edit an item
#' entity <- Entity$new(
#'   name = "TestWell01",
#'   entity_type_name = "Well",
#'   alias = "TestAlias01"
#' )
#' sp$items$save("Entity", entity)
#' }
RepositoryService <- R6Class( # nolint: object_name_linter
  "RepositoryService",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(

    #' @description Create a new RepositoryService instance. This is done by the
    #' [ServiceProvider] automatically.
    initialize = function() {},

    #' @description Get the names of all items of the given type available in
    #' PetroVisor.
    #'
    #' @param type The type of the item.
    #'
    #' @return A character array containing the names of the items.
    load_names = function(type) {
      # Get route
      route <- private$get_url_type(type)

      # Get item names
      item_names <- super$get(route = route)
      return(item_names)
    },

    #' @description Delete an item by name.
    #'
    #' @param type The type of the item.
    #' @param name Name of the item to delete.
    delete = function(type, name) {
      # Get route
      route <- paste0(private$get_url_type(type), "/")

      # Delete item
      super$delete(name = name,
                   route = route)
    },

    #' @description  Get an item by name.
    #'
    #' @param type The type of the item.
    #' @param name Name of the item to retrieve.
    #'
    #' @return An object of the specified type class.
    load = function(type, name) {
      # get the url_type
      route <- paste0(private$get_url_type(type), "/", name)

      # Retrieve the item
      cont <- super$get(route = route)

      # parse to object
      switch(type,
        ConfigurationSetting = return(
          ConfigurationSetting$new(
            name = cont$Name,
            numeric_value = cont$NumericValue,
            string_value = cont$StringValue,
            list_value = cont$ListValue,
            enumeration_value = if (length(cont$EnumerationValue)) {
              cont$EnumerationValue
            } else {
              list()
            },
            dictionary_value = if (length(cont$DictionaryValue)) {
              cont$DictionaryValue
            } else {
              list()
            },
            value_type = cont$ValueType,
            unit_name = cont$UnitName,
            possible_values = cont$PossibleValues,
            is_system = cont$IsSystem,
            description = cont$Description,
            labels = cont$Labels
          )
        ),
        Context = return(
          Context$new(
            name = cont$Name,
            entity_set = private$get_entity_set_from_content(cont$EntitySet),
            scope = private$get_scope_from_content(cont$Scope),
            hierarchy = private$get_hierarchy_from_content(cont$Hierarchy),
            loading_scenario_name = cont$LoadScenarioName,
            saving_scenario_name = cont$SavingScenarioName,
            scenario_data_only = cont$ScenarioDataOnly,
            formula = cont$Formula,
            description = cont$Description,
            labels = cont$Labels
          )
        ),
        Entity = return(
          Entity$new(
            name = cont$Name,
            entity_type_name = cont$EntityTypeName,
            alias = cont$Alias,
            is_opportunity = cont$IsOpportunity
          )
        ),
        EntitySet = return(private$get_entity_set_from_content(cont)),
        EntityType = return(
          EntityType$new(
            name = cont$Name,
            image = cont$Image
          )
        ),
        Hierarchy = return(private$get_hierarchy_from_content(cont)),
        MLModel = {
          # Parse training options scope if present
          training_scope <- NULL
          if (!is.null(cont$TrainingOptions$Scope) &&
                cont$TrainingOptions$Scope$Name != "") {
            training_scope <-
              private$get_scope_from_content(cont$TrainingOptions$Scope)
          }

          # Parse training options entity set if present
          training_entity_set <- NULL
          if (!is.null(cont$TrainingOptions$EntitySet) &&
                cont$TrainingOptions$EntitySet$Name != "") {
            training_entity_set <-
              private$get_entity_set_from_content(
                cont$TrainingOptions$EntitySet
              )
          }

          # Parse custom training options if present
          custom_training_options <- NULL
          if (!is.null(cont$TrainingOptions$CustomTrainingOptions)) {
            custom_opts <- cont$TrainingOptions$CustomTrainingOptions

            # Neural network options if present
            nn_options <- NULL
            if (!is.null(custom_opts$NeuralNetworkTrainingOptions)) {
              nn_opts <- custom_opts$NeuralNetworkTrainingOptions
              nn_options <- MLNeuralNetworkTrainingOptions$new(
                library = nn_opts$Library,
                model_path = nn_opts$ModelPath,
                transfer_learning = nn_opts$TransferLearning,
                hyperparameter_tuning = nn_opts$HyperParameterTuning,
                features_mapping = nn_opts$FeaturesMapping,
                optimizer = nn_opts$Optimizer,
                learning_rate = nn_opts$LearningRate,
                epochs = nn_opts$Epochs,
                batch_size = nn_opts$BatchSize,
                sequence_window_size = nn_opts$SequenceWindowSize,
                activation_function = nn_opts$ActivationFunction,
                number_of_dense_layers = nn_opts$NumberOfDenseLayers,
                number_of_neurons = nn_opts$NumberOfNeurons,
                number_of_conv1d_layers = nn_opts$NumberOfConv1DLayers,
                number_of_conv1d_filters = nn_opts$NumberOfConv1DFilters,
                conv1d_kernel_size = nn_opts$Conv1DKernelSize,
                number_of_lstm_layers = nn_opts$NumberOfLstmLayers,
                number_of_lstm_neurons = nn_opts$NumberOfLstmNeurons
              )
            }

            custom_training_options <- MLCustomTrainingOptions$new(
              l2_regularization = custom_opts$L2Regularization,
              enable_pruning = custom_opts$EnablePruning,
              entropy_coefficient = custom_opts$EntropyCoefficient,
              learning_rate = custom_opts$LearningRate,
              maximum_bin_count_per_feature =
                custom_opts$MaximumBinCountPerFeature,
              minimum_example_count_per_leaf =
                custom_opts$MinimumExampleCountPerLeaf,
              number_of_iterations = custom_opts$NumberOfIterations,
              pruning_metrics = custom_opts$PruningMetrics,
              rank = custom_opts$Rank,
              oversampling = custom_opts$Oversampling,
              ensure_zero_mean = custom_opts$EnsureZeroMean,
              onnx_model = custom_opts$OnnxModel,
              neural_network_training_options = nn_options
            )
          }

          # Parse pre-processors from API response (data.frame format)
          pre_processors <- list()
          if (!is.null(cont$TrainingOptions$PreProcessors) &&
                is.data.frame(cont$TrainingOptions$PreProcessors) &&
                nrow(cont$TrainingOptions$PreProcessors) > 0) {

            pp_df <- cont$TrainingOptions$PreProcessors
            for (i in seq_len(nrow(pp_df))) {
              # Get the preprocessor row
              pp_row <- pp_df[i, ]

              # Skip if no valid normalization type
              if (is.null(pp_row$NormalizationType) ||
                    is.na(pp_row$NormalizationType) ||
                    pp_row$NormalizationType == "") {
                next
              }

              # Create MLTransformerOptions from data.frame columns
              # with validation
              transformer_opts <- tryCatch({
                MLTransformerOptions$new(
                  rank = if ("Rank" %in% names(pp_row) && !is.na(pp_row$Rank)) {
                    pp_row$Rank
                  },
                  fix_zero = if ("FixZero" %in% names(pp_row) &&
                                   !is.na(pp_row$FixZero)) {
                    pp_row$FixZero
                  },
                  maximum_bin_count =
                    if ("MaximumBinCount" %in% names(pp_row) &&
                        !is.na(pp_row$MaximumBinCount)) {
                      pp_row$MaximumBinCount
                    },
                  minimum_examples_per_bin =
                    if ("MinimumExamplesPerBin" %in% names(pp_row) &&
                        !is.na(pp_row$MinimumExamplesPerBin)) {
                      pp_row$MinimumExamplesPerBin
                    },
                  ensure_zero_mean =
                    if ("EnsureZeroMean" %in% names(pp_row) &&
                        !is.na(pp_row$EnsureZeroMean)) {
                      pp_row$EnsureZeroMean
                    },
                  norm = if ("Norm" %in% names(pp_row) && !is.na(pp_row$Norm)) {
                    as.character(pp_row$Norm)
                  },
                  center_data =
                    if ("CenterData" %in% names(pp_row) &&
                        !is.na(pp_row$CenterData)) {
                      pp_row$CenterData
                    },
                  quantile_min =
                    if ("QuantileMin" %in% names(pp_row) &&
                        !is.na(pp_row$QuantileMin)) {
                      pp_row$QuantileMin
                    },
                  quantile_max =
                    if ("QuantileMax" %in% names(pp_row) &&
                        !is.na(pp_row$QuantileMax)) {
                      pp_row$QuantileMax
                    },
                  ensure_unit_standard_deviation =
                    if ("EnsureUnitStandardDeviation" %in% names(pp_row) &&
                        !is.na(pp_row$EnsureUnitStandardDeviation)) {
                      pp_row$EnsureUnitStandardDeviation
                    },
                  scale =
                    if ("Scale" %in% names(pp_row) && !is.na(pp_row$Scale)) {
                      pp_row$Scale
                    },
                  use_cdf =
                    if ("UseCdf" %in% names(pp_row) && !is.na(pp_row$UseCdf)) {
                      pp_row$UseCdf
                    }
                )
              }, error = function(e) {
                warning(
                  paste(
                    "Error creating MLTransformerOptions for preprocessor row",
                    i,
                    ":",
                    e$message,
                    ". Using default."
                  )
                )
                # Return a minimal working MLTransformerOptions
                MLTransformerOptions$new(fix_zero = TRUE, norm = "L2")
              })

              # Validate MLTransformerOptions was created successfully
              if (!inherits(transformer_opts, "MLTransformerOptions")) {
                warning(paste("Invalid MLTransformerOptions created for row",
                              i,
                              ". Skipping this preprocessor."))
                next
              }

              # Create MLPreProcessor instance with validation
              preprocessor <- tryCatch({
                MLPreProcessor$new(
                  normalization_type = as.character(pp_row$NormalizationType),
                  transformer_options = transformer_opts,
                  order = if ("Order" %in% names(pp_row) &&
                                !is.na(pp_row$Order)) {
                    pp_row$Order
                  } else {
                    NULL
                  },
                  is_enabled = if ("IsEnabled" %in% names(pp_row) &&
                                     !is.na(pp_row$IsEnabled)) {
                    pp_row$IsEnabled
                  } else {
                    FALSE
                  }
                )
              }, error = function(e) {
                warning(paste("Failed to create MLPreProcessor for row",
                              i,
                              ":",
                              e$message,
                              ". Skipping."))
                return(NULL)
              })

              # Only add valid preprocessors
              if (!is.null(preprocessor) &&
                    inherits(preprocessor, "MLPreProcessor")) {
                pre_processors[[length(pre_processors) + 1]] <- preprocessor
              }
            }
          }

          # Get the custom training type from API
          training_type <- cont$TrainingOptions$CustomTrainingType

          # If no custom training type is given or it's "Auto", don't create
          # custom training options
          if (is.null(training_type) ||
                training_type == "" ||
                training_type == "Auto") {
            training_type <- "Auto"
            custom_training_options <- NULL
          } else {
            # Only validate custom_training_options if we have a non-Auto
            # training type
            if (!is.null(custom_training_options) &&
                  !inherits(custom_training_options,
                            "MLCustomTrainingOptions")) {
              warning(
                paste(
                  "Invalid custom_training_options for training type",
                  training_type, ". Setting to NULL."
                )
              )
              custom_training_options <- NULL
            }
          }

          # Create training options structure for MLModel using
          # MLTrainingOptions class with validation
          training_options <- tryCatch({
            MLTrainingOptions$new(
              test_fraction = cont$TrainingOptions$TestFraction %||% NULL,
              test_latin_hypercube =
                cont$TrainingOptions$TestLatinHypercube %||% FALSE,
              validation_fraction =
                cont$TrainingOptions$ValidationFraction %||% NULL,
              optimization_metric =
                cont$TrainingOptions$OptimizationMetric %||% "RSquared",
              time_to_train = cont$TrainingOptions$TimeToTrain %||% 60,
              num_clusters = cont$TrainingOptions$NumberOfClusters %||% 1,
              num_cv_folds =
                cont$TrainingOptions$NumberOfCrossValidationFolds %||% 1,
              trainers_to_exclude =
                cont$TrainingOptions$TrainersToExclude %||% list(),
              scope = training_scope,
              entity_set = training_entity_set,
              include_incomplete_cases =
                cont$TrainingOptions$IncludeIncompleteCases %||% FALSE,
              custom_training_type = training_type,
              custom_training_options = custom_training_options,
              pre_processors = pre_processors,
              apply_pre_processors_before_training =
                cont$TrainingOptions$ApplyPreProcessorsBeforeTraining %||% TRUE,
              tuner = cont$TrainingOptions$Tuner %||% "EciCostFrugal",
              maximum_models_to_train =
                cont$TrainingOptions$MaximumModelsToTrain %||% 1000,
              survival_data = cont$TrainingOptions$SurvivalData,
              trainer_hyperparameters =
                cont$TrainingOptions$TrainerHyperparameters %||% list()
            )
          }, error = function(e) {
            stop(paste("Failed to create MLTrainingOptions from API data:",
                       e$message))
          })

          # Create MLModel directly with parsed data
          model <- MLModel$new(
            name = cont$Name,
            type = cont$Type %||% "Regression",
            table_formula = cont$TableFormula,
            context_formula = cont$ContextFormula,
            description = cont$Description,
            label_column_name = if (is.null(cont$LabelColumnName) ||
                                      cont$LabelColumnName == "") {
              NULL
            } else {
              cont$LabelColumnName
            },
            survival_data = cont$SurvivalData,
            labels = if (is.null(cont$Labels)) {
              list()
            } else if (is.list(cont$Labels)) {
              cont$Labels
            } else {
              # Convert character vector or single value to list
              if (is.character(cont$Labels)) {
                as.list(cont$Labels)
              } else {
                list(cont$Labels)
              }
            }
          )

          # Set additional fields directly
          model$is_depth_data <- cont$IsDepthData
          model$is_model_per_entity <- cont$IsModelPerEntity %||% FALSE
          model$test_data_mode <- cont$TestDataMode %||% "Union"
          model$validation_data_mode <- cont$ValidationDataMode %||% "Union"
          model$trained_models <- cont$TrainedModels %||% list()

          # Handle outlier filters
          if (is.null(cont$OutlierFilters) ||
                length(cont$OutlierFilters) == 0) {
            model$outlier_filters <- "None"
          } else {
            # If it's a list, extract the first element
            if (is.list(cont$OutlierFilters) &&
                  length(cont$OutlierFilters) > 0) {
              model$outlier_filters <- cont$OutlierFilters[[1]]
            } else {
              model$outlier_filters <- cont$OutlierFilters
            }
          }

          # Set optional fields
          model$is_automatic <- cont$IsAutomatic
          model$trained <- cont$Trained
          model$is_reviewed <- cont$IsReviewed
          model$include_incomplete_cases <- cont$IncludeIncompleteCases
          model$data_provider_config <- cont$DataProviderConfig
          model$validation_scope_formula <- cont$ValidationScopeFormula
          model$validation_entity_set_formula <- cont$ValidationEntitySetFormula
          model$test_scope_formula <- cont$TestScopeFormula
          model$test_entity_set_formula <- cont$TestEntitySetFormula

          # Set the already-properly-parsed training options directly
          model$training_options <- training_options

          return(model)
        },
        PivotTable = return(
          PivotTable$new(
            name = cont$Name,
            add_entity_alias_column = cont$AddEntityAliasColumn,
            entity_parent_columns = cont$AddEntityParentsColumns,
            add_entity_type_column = cont$AddEntityTypeColumn,
            scope_formula = cont$ScopeFormula,
            entity_set_formula = cont$EntitySetFormula,
            table_formula = cont$TableFormula,
            hierarchy_name = cont$HierarchyName,
            tag_entry_date = cont$TagEntryDate,
            saved_date = cont$SavedDate,
            used_tags = cont$UsedTags,
            skip_empty_rows = cont$SkipEmptyDataRows,
            add_is_opportunity_column = cont$AddIsOpportunityColumn,
            append_data = cont$AppendSavedData,
            description = cont$Description,
            labels = cont$Labels
          )
        ),
        Unit = return(
          Unit$new(
            name = cont$Name,
            measurement_name = cont$MeasurementName,
            factor = cont$Factor,
            summand = cont$Summand
          )
        ),
        UnitMeasurement = return(
          UnitMeasurement$new(
            name = cont$Name,
            canonical_unit_name = cont$CanonicalUnitName
          )
        ),
        ReferenceTable = {
          values_list <- list()
          if (length(cont$Values) > 0) {
            for (i in seq_len(nrow(cont$Values))) {
              values_list[[i]] <- ReferenceTableColumn$new(
                name = cont$Values[i, "Name"],
                column_type = cont$Values[i, "ColumnType"],
                unit_name = cont$Values[i, "UnitName"]
              )
            }
          }

          return(
            ReferenceTable$new(
              name = cont$Name,
              description = cont$Description,
              labels = cont$Labels,
              key = ReferenceTableColumn$new(name = cont$Key$Name,
                                             column_type = cont$Key$ColumnType,
                                             unit_name = cont$Key$UnitName),
              values = values_list
            )
          )
        },
        Scenario = {
          cs_list <- list()
          if (length(cont$WorkspaceValues) > 0) {
            for (i in seq_len(nrow(cont$WorkspaceValues))) {
              cs_list[[i]] <- ConfigurationSetting$new(
                name = cont$WorkspaceValues[i, "Name"],
                numeric_value = cont$WorkspaceValues[i, "NumericValue"],
                string_value = cont$WorkspaceValues[i, "StringValue"],
                list_value = cont$WorkspaceValues[i, "ListValue"] %||% list(),
                enumeration_value =
                  cont$WorkspaceValues[i, "EnumerationValue"] %||% list(),
                dictionary_value =
                  cont$WorkspaceValues[i, "DictionaryValue"] %||% list(),
                value_type = cont$WorkspaceValues[i, "ValueType"],
                unit_name = cont$WorkspaceValues[i, "UnitName"],
                possible_values =
                  cont$WorkspaceValues[i, "PossibleValues"] %||% list(),
                is_system = cont$WorkspaceValues[i, "IsSystem"],
                description = cont$WorkspaceValues[i, "Description"],
                labels = cont$WorkspaceValues[i, "Labels"] %||% list()
              )
            }
          }

          return(
            Scenario$new(
              name = cont$Name,
              configuration_settings = cs_list,
              description = cont$Description,
              labels = cont$Labels
            )
          )
        },
        Scope = return(private$get_scope_from_content(cont)),
        Signal = return(
          Signal$new(
            name = cont$Name,
            short_name = cont$ShortName,
            measurement_name = cont$MeasurementName,
            storage_unit_name = cont$StorageUnitName,
            aggregation_type = cont$AggregationType,
            container_aggregation_type = cont$ContainerAggregationType,
            signal_type = cont$SignalType,
            default_color = cont$DefaultColor,
            default_line_type = cont$DefaultLineType,
            setting_name = cont$SettingName,
            labels = cont$Labels,
            description = cont$Description
          )
        ),
        Tag = return(Tag$new(name = cont$Name, tag_group = cont$TagGroup))
      )
    },

    #' @details Add or edit an item.
    #'
    #' @param type The type of the item.
    #' @param item Item to add or edit. Has to be an object of the respective
    #' class.
    save = function(type, item) {
      # Get route
      route <- paste0(private$get_url_type(type), "/", item$name)

      # Add or edit item
      result <- super$put(body = item$toList(),
                          route = route)

      # For hierarchies make sure to save the relationships
      if (type == "Hierarchy") {
        if (item$is_time_dependent) {
          # Update time-dependent relationships
          rel_list <- lapply(
            item$relationship,
            function(x) {
              y <- as.list(x$parent)
              names(y) <- x$child
              return(y)
            }
          )
          super$post(body = rel_list,
                     route = paste0(route, "/Relationships/AddOrEdit"))
        }
      }

      return(result)
    }
  ),
  private = list(
    get_url_type = function(type = c("Chart", "CleansingCalculation",
                                     "CleansingScript", "ConfigurationSetting",
                                     "Context",
                                     "CustomWorkflowActivity", "DataConnection",
                                     "DataIntegrationSet", "DataSource",
                                     "DCAFit", "Entity", "EntitySet",
                                     "EntityType", "EventCalculation", "Filter",
                                     "Hierarchy", "MLModel",
                                     "PivotTable",
                                     "ProcessTemplate", "PSharpScript",
                                     "ReferenceTable",
                                     "RScript", "RWorkflowActivity", "Scenario",
                                     "Scope", "Signal", "TableCalculation",
                                     "Tag", "UnitMeasurement", "Unit",
                                     "Workflow", "WorkflowSchedule")) {
      # check input
      type <- match.arg(type)

      # get type for URL
      switch(type,
        Chart = return("Charts"),
        CleansingCalculation = return("CleansingCalculations"),
        CleansingScript = return("CleansingScripts"),
        ConfigurationSetting = return("ConfigurationSettings"),
        Context = return("Contexts"),
        CustomWorkflowActivity = return("CustomWorkflowActivities"),
        DataConnection = return("DataConnections"),
        DataIntegrationSet = return("DataIntegrationSets"),
        DataSource = return("DataSources"),
        DCAFit = return("DCAFits"),
        Entity = return("Entities"),
        EntitySet = return("EntitySets"),
        EntityType = return("EntityTypes"),
        EventCalculation = return("EventCalculations"),
        Filter = return("Filters"),
        Hierarchy = return("Hierarchies"),
        MLModel = return("MLModels"),
        PivotTable = return("PivotTables"),
        ProcessTemplate = return("ProcessTemplates"),
        PSharpScript = return("PSharpScripts"),
        ReferenceTable = return("RefTables"),
        RScript = return("RScripts"),
        RWorkflowActivity = return("RWorkflowActivities"),
        Scenario = return("Scenarios"),
        Scope = return("Scopes"),
        Signal = return("Signals"),
        TableCalculation = return("TableCalculations", ),
        Tag = return("Tags"),
        UnitMeasurement = return("UnitMeasurements"),
        Unit = return("Units"),
        Workflow = return("Workflows"),
        WorkflowSchedule = return("WorkflowSchedules")
      )
    },

    get_entity_set_from_content = function(content) {
      # Validate content structure
      if (is.null(content) || is.null(content$Name)) {
        return(NULL)
      }

      # create entity list
      entity_list <- list()

      # map entities from content if Entities exists and is valid
      if (!is.null(content$Entities) &&
            is.data.frame(content$Entities) &&
            nrow(content$Entities) > 0) {
        for (i in seq_len(nrow(content$Entities))) {
          entity_list[[i]] <- Entity$new(
            name = content$Entities[i, "Name"],
            alias = content$Entities[i, "Alias"],
            entity_type_name = content$Entities[i, "EntityTypeName"],
            is_opportunity = content$Entities[i, "IsOpportunity"]
          )
        }
      }

      # return new entity set
      return(
        EntitySet$new(
          name = content$Name,
          entities = entity_list,
          formula = content$Formula,
          description = content$Description,
          labels = content$Labels
        )
      )
    },

    get_scope_from_content = function(content) {
      # Validate content structure
      if (is.null(content) || is.null(content$Name)) {
        return(NULL)
      }

      return(
        Scope$new(
          name = content$Name,
          start = content$Start,
          end = content$End,
          time_increment = content$TimeIncrement,
          depth_increment = content$DepthIncrement,
          start_depth = content$StartDepth,
          end_depth = content$EndDepth,
          formula = content$Formula,
          description = content$Description,
          labels = content$Labels
        )
      )
    },

    get_hierarchy_from_content = function(content) {
      # load relationships separately if time dependent
      if (as.logical(content$IsTimeDependent)) {
        rel <- super$get(
          private$url,
          paste0("Hierarchies/", content$Name, "/Relationship/All"),
          private$token_type,
          private$token
        )
        relationship <- lapply(
          rel, function(x) {
            data.frame(
              child = names(x),
              parent = sapply(
                x,
                function(y) if (is.null(y)) NA else as.character(y),
                USE.NAMES = FALSE
              ),
              row.names = NULL
            )
          }
        )
      } else {
        relationship <- data.frame(
          child = names(content$Relationship),
          parent = sapply(
            content$Relationship,
            function(x) if (is.null(x)) NA else as.character(x),
            USE.NAMES = FALSE
          ),
          row.names = NULL
        )
      }

      return(
        Hierarchy$new(
          name = content$Name,
          relationship = relationship,
          is_time_dependent = content$IsTimeDependent,
          time_stamp = content$TimeStamp,
          description = content$Description,
          labels = content$Labels
        )
      )
    }
  )
)
