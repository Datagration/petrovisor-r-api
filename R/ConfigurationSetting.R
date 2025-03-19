library("R6")

#' @title ConfigurationSetting
#'
#' @description Class representing a PetroVisor ConfigurationSetting object.
#'
#' @export ConfigurationSetting
#'
#' @field name The name of the configuration setting.
#' @field numericValue The numeric value of the configuration setting.
#' @field stringValue The string value of the configuration setting.
#' @field listValue The list value of the configuration setting (type list).
#' @field enumerationValue The enumeration value of the configuration setting
#'   (named list).
#' @field dictionaryValue The dictionary value of the configuration setting
#'   (named list).
#' @field valueType The value type of the configuration setting. Must be one of:
#'   \code{"Numeric"}, \code{"String"}, \code{"Numeric with unit"},
#'   \code{"List"}, \code{"Enumeration"}, \code{"Dictionary"}
#' @field unitName The name of the unit associated with the numeric value if the
#'   valueType is \code{"Numeric with unit"}.
#' @field isSystem This flaf specifies, whether the configuration setting is a
#'   PetroVisor system setting (immutable).
#' @examples
#' \dontrun{
#' ConfigurationSetting$new(name = "MyStringValue",
#'                          valueType = "String",
#'                          stringValue = "Hello World!")
#'}
ConfigurationSetting <- R6Class("ConfigurationSetting",
  public = list(
    name = NULL,
    numericValue = NULL,
    stringValue = NULL,
    listValue = NULL,
    enumerationValue = NULL,
    dictionaryValue = NULL,
    valueType = NULL,
    unitName = NULL,
    isSystem = NULL,

    #' @description Create a new ConfigurationSetting instance.
    #'
    #' @param name The name of the configuration setting.
    #' @param numericValue The numeric value of the configuration setting.
    #' @param stringValue The string value of the configuration setting.
    #' @param listValue The list value of the configuration setting (type list).
    #' @param enumerationValue The enumeration value of the configuration
    #'   setting (named list).
    #' @param dictionaryValue The dictionary value of the configuration setting
    #'   (named list).
    #' @param valueType The value type of the configuration setting. Must be one
    #'   of: \code{"Numeric"}, \code{"String"}, \code{"Numeric with unit"},
    #'   \code{"List"}, \code{"Enumeration"}, \code{"Dictionary"}
    #' @param unitName The name of the unit associated with the numeric value if
    #'   the valueType is \code{"Numeric with unit"}.
    #' @param isSystem This flaf specifies, whether the configuration setting is
    #'   a PetroVisor system setting (immutable).
    initialize = function(name = NULL,
                          numericValue = NULL,
                          stringValue = NULL,
                          listValue = NULL,
                          enumerationValue = NULL,
                          dictionaryValue = NULL,
                          valueType = c("Numeric", "String",
                                        "Numeric with unit", "List",
                                        "Enumeration", "Dictionary"),
                          unitName = NULL,
                          isSystem = NULL){
      self$name <- name
      self$numericValue <- numericValue
      self$stringValue <- stringValue
      self$listValue <- listValue
      self$enumerationValue <- enumerationValue
      self$dictionaryValue <- dictionaryValue
      self$valueType <- match.arg(valueType)
      self$unitName <- unitName
      self$isSystem <- isSystem
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        NumericValue = if(is.null(self$numericValue)) {
          ""
        } else {
          self$numericValue
        },
        StringValue = if(is.null(self$stringValue)) "" else self$stringValue,
        ListValue = if(is.null(self$listValue)) "" else self$listValue,
        EnumerationValue = if(is.null(self$enumerationValue)) {
          ""
        } else {
          self$enumerationValue
        },
        DictionaryValue = if(is.null(self$dictionaryValue)) {
          ""
        } else {
          self$dictionaryValue
        },
        ValueType = if(is.null(self$valueType)) "" else self$valueType,
        UnitName = if(is.null(self$unitName)) "" else self$unitName,
        IsSystem = if(is.null(self$isSystem)) "" else self$isSystem
      )
      return(dl)
    }
  )
)
