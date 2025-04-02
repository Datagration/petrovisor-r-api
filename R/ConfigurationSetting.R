library("R6")

#' @title ConfigurationSetting
#'
#' @description Class representing a PetroVisor ConfigurationSetting object.
#'
#' @export ConfigurationSetting
#'
#' @field name The name of the configuration setting.
#' @field numeric_value The numeric value of the configuration setting.
#' @field string_value The string value of the configuration setting.
#' @field list_value The list value of the configuration setting (type list).
#' @field enumeration_value The enumeration value of the configuration setting
#'   (named list).
#' @field dictionary_value The dictionary value of the configuration setting
#'   (named list).
#' @field value_type The value type of the configuration setting.
#'   Must be one of: \code{"Numeric"}, \code{"String"},
#'   \code{"NumericWithUnit"}, \code{"List"}, \code{"Enumeration"},
#'   \code{"Dictionary"}, \code{"StringWithType"}, \code{"ListWithType"},
#'   \code{"StringFromValues"}, \code{"ListFromValues"}
#' @field unit_name The name of the unit associated with the numeric value if
#'   the valueType is \code{"Numeric with unit"}.
#' @field possible_values A list containing the possible values of the
#'   configuration setting.
#' @field is_system This flaf specifies, whether the configuration setting is a
#'   PetroVisor system setting (immutable).
#' @field description The description of the item.
#' @field labels A list of strings holding the labels of the item.
#' @examples
#' \dontrun{
#' ConfigurationSetting$new(name = "MyStringValue",
#'                          value_type = "String",
#'                          string_value = "Hello World!")
#'}
ConfigurationSetting <- R6Class("ConfigurationSetting",
  public = list(
    name = NULL,
    numeric_value = NULL,
    string_value = NULL,
    list_value = NULL,
    enumeration_value = NULL,
    dictionary_value = NULL,
    value_type = NULL,
    unit_name = NULL,
    possible_values = NULL,
    is_system = NULL,
    description = NULL,
    labels = NULL,

    #' @description Create a new ConfigurationSetting instance.
    #'
    #' @param name The name of the configuration setting.
    #' @param numeric_value The numeric value of the configuration setting.
    #' @param string_value The string value of the configuration setting.
    #' @param list_value The list value of the configuration setting.
    #' @param enumeration_value The enumeration value of the configuration
    #'   setting (named list).
    #' @param dictionary_value The dictionary value of the configuration setting
    #'   (named list).
    #' @param value_type The value type of the configuration setting.
    #'   Must be one of: \code{"Numeric"}, \code{"String"},
    #'   \code{"NumericWithUnit"}, \code{"List"}, \code{"Enumeration"},
    #'   \code{"Dictionary"}, \code{"StringWithType"}, \code{"ListWithType"},
    #'   \code{"StringFromValues"}, \code{"ListFromValues"}
    #' @param unit_name The name of the unit associated with the numeric value
    #'   if the valueType is \code{"Numeric with unit"}.
    #' @param possible_values A list containing the possible values of the
    #'   configuration setting.
    #' @param is_system This flaf specifies, whether the configuration setting
    #'   is a PetroVisor system setting (immutable).
    #' @param description The description of the item.
    #' @param labels A list of strings holding the labels of the item.
    initialize = function(name = NULL,
                          numeric_value = NULL,
                          string_value = NULL,
                          list_value = list(),
                          enumeration_value = list(),
                          dictionary_value = list(),
                          value_type = c("Numeric", "String",
                                         "NumericWithUnit", "List",
                                         "Enumeration", "Dictionary",
                                         "StringWithType", "ListWithType",
                                         "StringFromValues", "ListFromValues"),
                          unit_name = NULL,
                          possible_values = list(),
                          is_system = NULL,
                          description = NULL,
                          labels = list()) {
      self$name <- name
      self$numeric_value <- numeric_value
      self$string_value <- string_value
      self$list_value <- list_value
      self$enumeration_value <- enumeration_value
      self$dictionary_value <- dictionary_value
      self$value_type <- match.arg(value_type)
      self$unit_name <- unit_name
      self$possible_values <- possible_values
      self$is_system <- is_system
      self$description <- description
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        NumericValue = if (is.null(self$numeric_value)) {
          ""
        } else {
          self$numeric_value
        },
        StringValue = if (is.null(self$string_value)) "" else self$string_value,
        ListValue = self$list_value,
        ValueType = if (is.null(self$value_type)) "" else self$value_type,
        UnitName = if (is.null(self$unit_name)) "" else self$unit_name,
        PossibleValues = self$possible_values,
        IsSystem = if (is.null(self$is_system)) "" else self$is_system,
        Description = if (is.null(self$description)) "" else self$description,
        Labels = self$labels
      )

      if (length(self$enumeration_value)) {
        dl$EnumerationValue <- self$enumeration_value
      }

      if (length(self$dictionary_value)) {
        dl$DictionaryValue <- self$dictionary_value
      }

      return(dl)
    }
  )
)
