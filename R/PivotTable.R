library("R6")

#' @title PivotTable
#'
#' @description Class representing a PetroVisor pivot table object.
#'
#' @export PivotTable
#'
#' @field name The name of the pivot table.
#' @field addEntityAliasColumn This flag specifies whether a column containing
#'   the entity's alias shall be added to the pivot table.
#' @field addEntityParentColumn This flag specifies whether a column containing
#'   the entity's parent shall be added to the pivot table.
#' @field addEntityTypeColumn This flag specifies whether a column containing
#'   the entity's type shall be added to the pivot table.
#' @field scopeFormula The scope definition as string (P# syntax).
#' @field entitySetFormula The entity set definition as string (P# syntax).
#' @field tableFormula The table definition as string (P# syntax).
#' @field hierarchyName The name of the hierarchy used to retrieve the parent
#'   name.
#' @field useDataPreloading This flag specifies whether data preloading shall be
#'   used when calculating the pivot table.
#' @field allowScriptExecution This flag specifies whether calculations in the
#'   table formula are executed or not.
#' @field tagEntryDate The date used to evaluate tag entries.
#' @field savedDate The date the pivot table was saved to the database.
#' @field usedTags A list of tag names used in the pivot table.
#' @field dateTimeFormat A string specifying the time format in the pivot table.
#' @field numberFormat A string specifying the number format in the pivot table.
#' @field nanValueString A string specifying the NaN value in the pivot table.
#' @field tagActiveString A string specifying the text that is stored in the
#'   table if a tag is active.
#' @field isLocked This flag specifies whether the pivot table is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the pivot table belongs to.
#' @field isFavorite This flag specifies whether the pivot table is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the pivot table.
#' @examples
#' \dontrun{
#' PivotTable$new()
#'}
PivotTable <- R6Class("PivotTable",
  public = list(
    name = NULL,
    addEntityAliasColumn = NULL,
    addEntityParentColumn = NULL,
    addEntityTypeColumn = NULL,
    scopeFormula = NULL,
    entitySetFormula = NULL,
    tableFormula = NULL,
    hierarchyName = NULL,
    useDataPreloading = NULL,
    allowScriptExecution = NULL,
    tagEntryDate = NULL,
    savedDate = NULL,
    usedTags = NULL,
    dateTimeFormat = NULL,
    numberFormat = NULL,
    nanValueString = NULL,
    tagActiveString = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new PivotTable instance.
    #'
    #' @param name The name of the pivot table.
    #' @param addEntityAliasColumn This flag specifies whether a column
    #'   containing the entity's alias shall be added to the pivot table.
    #' @param addEntityParentColumn This flag specifies whether a column
    #'   containing the entity's parent shall be added to the pivot table.
    #' @param addEntityTypeColumn This flag specifies whether a column
    #'   containing the entity's type shall be added to the pivot table.
    #' @param scopeFormula The scope definition as string (P# syntax).
    #' @param entitySetFormula The entity set definition as string (P# syntax).
    #' @param tableFormula The table definition as string (P# syntax).
    #' @param hierarchyName The name of the hierarchy used to retrieve the
    #'   parent name.
    #' @param useDataPreloading This flag specifies whether data preloading
    #'   shall be used when calculating the pivot table.
    #' @param allowScriptExecution This flag specifies whether calculations in
    #'   the table formula are executed or not.
    #' @param tagEntryDate The date used to evaluate tag entries.
    #' @param savedDate The date the pivot table was saved to the database.
    #' @param usedTags A list of tag names used in the pivot table.
    #' @param dateTimeFormat A string specifying the time format in the pivot
    #'   table.
    #' @param numberFormat A string specifying the number format in the pivot
    #'   table.
    #' @param nanValueString A string specifying the NaN value in the pivot
    #'   table.
    #' @param tagActiveString A string specifying the text that is stored in the
    #'   table if a tag is active.
    #' @param isLocked This flag specifies whether the pivot table is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the pivot table belongs to.
    #' @param isFavorite This flag specifies whether the pivot table is marked
    #'   as favorite item, and thus shown in the favorites tab on the home
    #'   module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the pivot table.
    initialize = function(name = NULL,
                          addEntityAliasColumn = NULL,
                          addEntityParentColumn = NULL,
                          addEntityTypeColumn = NULL,
                          scopeFormula = NULL,
                          entitySetFormula = NULL,
                          tableFormula = NULL,
                          hierarchyName = NULL,
                          useDataPreloading = NULL,
                          allowScriptExecution = NULL,
                          tagEntryDate = NULL,
                          savedDate = NULL,
                          usedTags = NULL,
                          dateTimeFormat = NULL,
                          numberFormat = NULL,
                          nanValueString = NULL,
                          tagActiveString = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$addEntityAliasColumn <- addEntityAliasColumn
      self$addEntityParentColumn <- addEntityParentColumn
      self$addEntityTypeColumn <- addEntityTypeColumn
      self$scopeFormula <- scopeFormula
      self$entitySetFormula <- entitySetFormula
      self$tableFormula <- tableFormula
      self$hierarchyName <- hierarchyName
      self$useDataPreloading <- useDataPreloading
      self$allowScriptExecution <- allowScriptExecution
      self$tagEntryDate <- tagEntryDate
      self$savedDate <- savedDate
      self$usedTags <- usedTags
      self$dateTimeFormat <- dateTimeFormat
      self$numberFormat <- numberFormat
      self$nanValueString <- nanValueString
      self$tagActiveString <- tagActiveString
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        AddEntityAliasColumn = if(is.null(self$addEntityAliasColumn)) {
          ""
        } else {
          self$addEntityAliasColumn
        },
        AddEntityParentColumn = if(is.null(self$addEntityParentColumn)) {
          ""
        } else {
          self$addEntityParentColumn
        },
        AddEntityTypeColumn = if(is.null(self$addEntityTypeColumn)) {
          ""
        } else {
          self$addEntityTypeColumn
        },
        ScopeFormula = if(is.null(self$scopeFormula)) "" else self$scopeFormula,
        EntitySetFormula = if(is.null(self$entitySetFormula)) {
          ""
        } else {
          self$entitySetFormula
        },
        TableFormula = if(is.null(self$tableFormula)) "" else self$tableFormula,
        HierarchyName = if(is.null(self$hierarchyName)) {
          ""
        } else {
          self$hierarchyName
        },
        UseDataPreloading = if(is.null(self$useDataPreloading)) {
          ""
        } else {
          self$useDataPreloading
        },
        AllowScriptExecution = if(is.null(self$allowScriptExecution)) {
          ""
        } else {
          self$allowScriptExecution
        },
        TagEntryDate = if(is.null(self$tagEntryDate)) {
          ""
        } else {
          self$tagEntryDate
        },
        SavedDate = if(is.null(self$savedDate)) "" else self$savedDate,
        UsedTags = if(is.null(self$usedTags)) "" else self$usedTags,
        DateTimeFormat = if(is.null(self$dateTimeFormat)) {
          ""
        } else {
          self$dateTimeFormat
        },
        NumberFormat = if(is.null(self$numberFormat)) "" else self$numberFormat,
        NaNValueString = if(is.null(self$nanValueString)) {
          ""
        } else {
          self$nanValueString
        },
        TagActiveString = if(is.null(self$tagActiveString)) {
          ""
        } else {
          self$tagActiveString
        },
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)
