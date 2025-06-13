library("R6")

#' @title PivotTable
#'
#' @description Class representing a PetroVisor pivot table object.
#'
#' @export PivotTable
#'
#' @field name The name of the pivot table.
#' @field add_entity_alias_column This flag specifies whether a column
#'   containing the entity's alias shall be added to the pivot table.
#' @field entity_parent_columns Defines the number of parent columns added to
#'   the pivot table (when a hierarchy is specified).
#' @field add_entity_type_column This flag specifies whether a column containing
#'   the entity's type shall be added to the pivot table.
#' @field scope_formula The scope definition as string (P# syntax).
#' @field entity_set_formula The entity set definition as string (P# syntax).
#' @field table_formula The table definition as string (P# syntax).
#' @field hierarchy_name The name of the hierarchy used to retrieve the parent
#'   name.
#' @field tag_entry_date The date used to evaluate tag entries.
#' @field saved_date The date the pivot table was saved to the database.
#' @field used_tags A list of tag names used in the pivot table.
#' @field skip_empty_rows This flag specifies whether empty rows shall be
#'   skipped during table generation.
#' @field add_is_opportunity_column This flag specifies whether the column
#'   IsOpportunity shall be added to the pivot table.
#' @field append_data This flag specifies whether new data shall be appended to
#'   the saved table.
#' @field description The description of the item.
#' @field labels A list of strings holding the labels of the pivot table.
#' @examples
#' \dontrun{
#' PivotTable$new()
#'}
PivotTable <- R6Class("PivotTable",
  public = list(
    name = NULL,
    add_entity_alias_column = NULL,
    entity_parent_columns = NULL,
    add_entity_type_column = NULL,
    scope_formula = NULL,
    entity_set_formula = NULL,
    table_formula = NULL,
    hierarchy_name = NULL,
    tag_entry_date = NULL,
    saved_date = NULL,
    used_tags = NULL,
    skip_empty_rows = NULL,
    add_is_opportunity_column = NULL,
    append_data = NULL,
    description = NULL,
    labels = NULL,

    #' @description Create a new PivotTable instance.
    #'
    #' @param name The name of the pivot table.
    #' @param add_entity_alias_column This flag specifies whether a column
    #'   containing the entity's alias shall be added to the pivot table.
    #' @param entity_parent_columns Defines the number of parent columns added
    #'   to the pivot table (when a hierarchy is specified).
    #' @param add_entity_type_column This flag specifies whether a column
    #'   containing the entity's type shall be added to the pivot table.
    #' @param scope_formula The scope definition as string (P# syntax).
    #' @param entity_set_formula The entity set definition as string
    #'   (P# syntax).
    #' @param table_formula The table definition as string (P# syntax).
    #' @param hierarchy_name The name of the hierarchy used to retrieve the
    #'   parent name.
    #' @param tag_entry_date The date used to evaluate tag entries.
    #' @param saved_date The date the pivot table was saved to the database.
    #' @param used_tags A list of tag names used in the pivot table.
    #' @param skip_empty_rows This flag specifies whether empty rows shall be
    #'   skipped during table generation.
    #' @param add_is_opportunity_column This flag specifies whether the column
    #'   IsOpportunity shall be added to the pivot table.
    #' @param append_data This flag specifies whether new data shall be appended
    #'   to the saved table.
    #' @param description The description of the item.
    #' @param labels A list of strings holding the labels of the pivot table.
    initialize = function(name = NULL,
                          add_entity_alias_column = NULL,
                          entity_parent_columns = 0,
                          add_entity_type_column = NULL,
                          scope_formula = NULL,
                          entity_set_formula = NULL,
                          table_formula = NULL,
                          hierarchy_name = "",
                          tag_entry_date = NULL,
                          saved_date = NULL,
                          used_tags = list(),
                          skip_empty_rows = NULL,
                          add_is_opportunity_column = NULL,
                          append_data = NULL,
                          description = NULL,
                          labels = list()) {
      self$name <- name
      self$add_entity_alias_column <- add_entity_alias_column
      self$entity_parent_columns <- entity_parent_columns
      self$add_entity_type_column <- add_entity_type_column
      self$scope_formula <- scope_formula
      self$entity_set_formula <- entity_set_formula
      self$table_formula <- table_formula
      self$hierarchy_name <- hierarchy_name
      self$tag_entry_date <- tag_entry_date
      self$saved_date <- saved_date
      self$used_tags <- used_tags
      self$skip_empty_rows <- skip_empty_rows
      self$add_is_opportunity_column <- add_is_opportunity_column
      self$append_data <- append_data
      self$description <- description
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function() {
      dl <- list(
        Name = if (is.null(self$name)) "" else self$name,
        AddEntityAliasColumn = if (is.null(self$add_entity_alias_column)) {
          ""
        } else {
          self$add_entity_alias_column
        },
        AddEntityParentsColumns = if (is.null(self$entity_parent_columns)) {
          ""
        } else {
          self$entity_parent_columns
        },
        AddEntityTypeColumn = if (is.null(self$add_entity_type_column)) {
          ""
        } else {
          self$add_entity_type_column
        },
        ScopeFormula = if (is.null(self$scope_formula)) {
          ""
        } else {
          self$scope_formula
        },
        EntitySetFormula = if (is.null(self$entity_set_formula)) {
          ""
        } else {
          self$entity_set_formula
        },
        TableFormula = if (is.null(self$table_formula)) {
          ""
        } else {
          self$table_formula
        },
        HierarchyName = if (is.null(self$hierarchy_name)) {
          ""
        } else {
          self$hierarchy_name
        },
        TagEntryDate = if (is.null(self$tag_entry_date)) {
          NA
        } else {
          self$tag_entry_date
        },
        SavedDate = if (is.null(self$saved_date)) {
          NA
        } else {
          self$saved_date
        },
        UsedTags = self$used_tags,
        SkipEmptyDataRows = if (is.null(self$skip_empty_rows)) {
          ""
        } else {
          self$skip_empty_rows
        },
        AddIsOpportunityColumn = if (is.null(self$add_is_opportunity_column)) {
          ""
        } else {
          self$add_is_opportunity_column
        },
        AppendSavedData = if (is.null(self$append_data)) {
          ""
        } else {
          self$append_data
        },
        Description = if (is.null(self$description)) "" else self$description,
        Labels = self$labels
      )
      return(dl)
    }
  )
)
