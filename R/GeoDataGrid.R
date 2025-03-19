library("R6")

#' @title GeoDataGrid
#'
#' @description Class representing a PetroVisor GeoDataGrid object.
#'
#' @export GeoDataGrid
#'
#' @field name The name of the geo data grid.
#' @field gridData A list of values giving the data in the grid.
#' @field rows The number of rows in the grid.
#' @field columns The number of columns in the grid.
#' @field minX The x-index of the grid's min-value.
#' @field minY The y-index of the grid's min-value.
#' @field maxX The x-index of the grid's max-value.
#' @field maxY The y-index of the grid's max-value.
#' @field description A description of the geo data grid.
#' @field isLocked This flag specifies whether the geo data grid is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the geo data grid belongs to.
#' @field isFavorite This flag specifies whether the geo data grid is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the geo data grid.
#' @examples
#' \dontrun{
#' GeoDataGrid$new( )
#'}
GeoDataGrid <- R6Class("GeoDataGrid",
  public = list(
    name = NULL,
    gridData = NULL,
    rows = NULL,
    columns = NULL,
    minX = NULL,
    minY = NULL,
    maxX = NULL,
    maxY = NULL,
    description = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new GeoDataGrid instance.
    #'
    #' @param name The name of the geo data grid.
    #' @param gridData A list of values giving the data in the grid.
    #' @param rows The number of rows in the grid.
    #' @param columns The number of columns in the grid.
    #' @param minX The x-index of the grid's min-value.
    #' @param minY The y-index of the grid's min-value.
    #' @param maxX The x-index of the grid's max-value.
    #' @param maxY The y-index of the grid's max-value.
    #' @param description A description of the geo data grid.
    #' @param isLocked This flag specifies whether the geo data grid is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the geo data grid belongs to.
    #' @param isFavorite This flag specifies whether the geo data grid is marked
    #'   as favorite item, and thus shown in the favorites tab on the home
    #'   module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the geo data grid.
    initialize = function(name = NULL,
                          gridData = NULL,
                          rows = NULL,
                          columns = NULL,
                          minX = NULL,
                          minY = NULL,
                          maxX = NULL,
                          maxY = NULL,
                          description = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$gridData <- gridData
      self$rows <- rows
      self$columns <- columns
      self$minX <- minX
      self$minY <- minY
      self$maxX <- maxX
      self$maxY <- maxY
      self$description <- description
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
        GridData = if(is.null(self$gridData)) "" else self$gridData,
        Rows = if(is.null(self$rows)) "" else self$rows,
        Columns = if(is.null(self$columns)) "" else self$columns,
        Min = list(
          X = if(is.null(self$minX)) {
            ""
          } else {
            self$minX
          },
          Y = if(is.null(self$minY)) {
            ""
          } else {
            self$minY
          }),
        Max = list(
          X = if(is.null(self$maxX)) {
            ""
          } else {
            self$maxX
          },
          Y = if(is.null(self$maxY)) {
            ""
          } else {
            self$maxY
          }),
        Description = if(is.null(self$description)) "" else self$description,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)
