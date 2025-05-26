library("R6")

#' @title ReferenceTable
#'
#' @description Class representing a PetroVisor ReferenceTable object.
#'
#' @export ReferenceTable
#'
#' @field name The name of the reference table.
#' @field description The description of the item.
#' @field labels A list of strings holding the labels of the reference table.
#' @field key The key column definition. Object of type ReferenceTableColumn.
#' @field values List of value column definitions. I.e. a list of objects of
#'   type ReferenceTableColumn.
#'
#' @examples
#' \dontrun{
#' referenceTable <- ReferenceTable$new(
#'     name = "Test R ReferenceTable",
#'     description = "Test Description",
#'     labels = list("A", "B"),
#'     key = ReferenceTableColumn$new(name = "TestKey",
#'                                    column_type = "Numeric",
#'                                    unit_name = "m3"),
#'     values = list(
#'         ReferenceTableColumn$new(name = "Value One",
#'                                  column_type = "Numeric",
#'                                  unit_name = "m3"),
#'         ReferenceTableColumn$new(name = "Value Two",
#'                                  column_type = "Bool",
#'                                  unit_name = " ")
#'     ))
#'}
ReferenceTable <- R6Class("ReferenceTable",
  public = list(
    name = NULL,
    description = NULL,
    labels = NULL,
    key = NULL,
    values = NULL,

    #' @description Create a new ReferenceTable instance.
    #'
    #' @param name The name of the scope.
    #' @param description The description of the item.
    #' @param labels A list of strings holding the labels of the scope.
    #' @param key The key column definition. Object of type
    #'   ReferenceTableColumn.
    #' @param values List of value column definitions. I.e. a list of objects of
    #'   type ReferenceTableColumn.
    initialize = function(name = NULL,
                          description = NULL,
                          labels = list(),
                          key = NULL,
                          values = list()) {
      self$name <- name
      self$description <- description
      self$labels <- labels
      self$key <- key
      self$values <- values
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      value_column_list <- list()
      for (i in seq_along(self$values)){
        value_column_list[[i]] <- self$values[[i]]$toList()
      }

      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        Description = if (is.null(self$description)) "" else self$description,
        Labels = self$labels,
        Key = if (is.null(self$key)) "" else self$key$toList(),
        Values = value_column_list
      )
      return(dl)
    }
  )
)

#' @title ReferenceTableColumn
#'
#' @description Class representing a PetroVisor ReferenceTableColumn object.
#'
#' @export ReferenceTableColumn
#'
#' @field name The name of the column.
#' @field column_type The type of the column.
#' @field unit_name The name of the unit associated with the column.
#'
#' @examples
#' \dontrun{
#' column <- ReferenceTableColumn$new(name = "Column One",
#'                                    column_type = "Numeric",
#'                                    unit_name = "m3")
#'}
ReferenceTableColumn <- R6Class("ReferenceTableColumn",
  public = list(
    name = NULL,
    column_type = NULL,
    unit_name = NULL,

    #' @description Create a new ReferenceTableColumn instance.
    #'
    #' @param name The name of the column.
    #' @param column_type The type of the column.
    #' @param unit_name The name of the unit associated with the column.
    initialize = function(name = NULL,
                          column_type = c("Numeric",
                                          "String",
                                          "DateTime",
                                          "Bool"),
                          unit_name = NULL) {
      self$name <- name
      self$column_type <- column_type
      self$unit_name <- unit_name
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        ColumnType = self$column_type,
        UnitName = if (is.null(self$unit_name)) "" else self$unit_name
      )
      return(dl)
    }
  )
)
