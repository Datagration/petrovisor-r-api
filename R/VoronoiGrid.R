library("R6")

#' @title VoronoiGrid
#'
#' @description Class representing a PetroVisor Voronoi grid object.
#'
#' @export VoronoiGrid
#'
#' @field name The name of the Voronoi grid.
#' @field centralPoints A list of named points (instances of class NamedPoint)
#'   defining the central points of the Voronoi grid.
#' @field boundaries A list of named points (instances of class NamedPoint)
#'   defining the boundaries of the Voronoi grid.
#' @field borders A list of points (instances of class Point) defining the
#'   Voronoi grid's borders.
#' @field cells A list of cells (instances of class VoronoiCell) giving the
#'   cells of the Voronoi grid.
#' @field areaUnitName The name of the unit the cells' area is computed in.
#' @field isLocked This flag specifies whether the Voronoi grid is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the Voronoi grid belongs to.
#' @field isFavorite This flag specifies whether the Voronoi grid is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the Voronoi grid.
#' @examples
#' \dontrun{
#' VoronoiGrid$new()
#'}
VoronoiGrid <- R6Class("VoronoiGrid",
  public = list(
    name = NULL,
    centralPoints = NULL,
    boundaries = NULL,
    borders = NULL,
    cells = NULL,
    areaUnitName = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new VoronoiGrid instance.
    #'
    #' @param name The name of the Voronoi grid.
    #' @param centralPoints A list of named points (instances of class
    #'   NamedPoint) defining the central points of the Voronoi grid.
    #' @param boundaries A list of named points (instances of class NamedPoint)
    #'   defining the boundaries of the Voronoi grid.
    #' @param borders A list of points (instances of class Point) defining the
    #'   Voronoi grid's borders.
    #' @param cells A list of cells (instances of class VoronoiCell) giving the
    #'   cells of the Voronoi grid.
    #' @param areaUnitName The name of the unit the cells' area is computed in.
    #' @param isLocked This flag specifies whether the Voronoi grid is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the Voronoi grid belongs to.
    #' @param isFavorite This flag specifies whether the Voronoi grid is marked
    #'   as favorite item, and thus shown in the favorites tab on the home
    #'   module in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the Voronoi grid.
    initialize = function(name = NULL,
                          centralPoints = NULL,
                          boundaries = NULL,
                          borders = NULL,
                          cells = NULL,
                          areaUnitName = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$centralPoints <- centralPoints
      self$boundaries <- boundaries
      self$borders <- borders
      self$cells <- cells
      self$areaUnitName <- areaUnitName
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of central points
      centralPointsList <- list()
      if(!is.null(self$centralPoints)){
        for (i in 1:length(self$centralPoints)){
          centralPointsList[[i]] <- self$centralPoints[[i]]$toList()
        }
      } else {
        centralPointsList[[1]] <- ""
      }

      # create list from list of boundaries
      boundariesList <- list()
      if(!is.null(self$boundaries)){
        for (i in 1:length(self$boundaries)){
          boundariesList[[i]] <- self$boundaries[[i]]$toList()
        }
      } else {
        boundariesList[[1]] <- ""
      }

      # create list from list of borders
      bordersList <- list()
      if(!is.null(self$borders)){
        for (i in 1:length(self$borders)){
          bordersList[[i]] <- self$borders[[i]]$toList()
        }
      } else {
        bordersList[[1]] <- ""
      }

      # create list from list of cells
      cellsList <- list()
      if(!is.null(self$cells)){
        for (i in 1:length(self$cells)){
          cellsList[[i]] <- self$cells[[i]]$toList()
        }
      } else {
        cellsList[[1]] <- ""
      }

      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        CentralPoints = if(is.null(self$centralPoints)) {
          ""
        } else {
          centralPointsList
        },
        Boundaries = if(is.null(self$boundaries)) {
          ""
        } else {
          boundariesList
        },
        Borders = if(is.null(self$borders)) {
          ""
        } else {
          bordersList
        },
        Cells = if(is.null(self$cells)) {
          ""
        } else {
          cellsList
        },
        AreaUnitName = if(is.null(self$areaUnitName)) {
          ""
        } else {
          self$areaUnitName
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

#' @title VoronoiCell
#'
#' @description Class representing a PetroVisor VoronoiCell object.
#'
#' @export VoronoiCell
#'
#' @field name The name of the Voronoi cell.
#' @field center The center of the Voronoi Cell (instances of class Point).
#' @field points A list of points of the Voronoi cell (instances of class
#'   Point).
#' @field area The area of the vornoi cell.
#'
#' @examples
#' \dontrun{
#' VoronoiCell$new()
#'}
VoronoiCell <- R6Class("VoronoiCell",
  public = list(
    name = NULL,
    center = NULL,
    points = NULL,
    area = NULL,

    #' @description Create a new VoronoiCell instance.
    #'
    #' @param name The name of the Voronoi cell.
    #' @param center The center of the Voronoi Cell (instances of class Point).
    #' @param points A list of points of the Voronoi cell (instances of class
    #'   Point).
    #' @param area The area of the vornoi cell.
    initialize = function(name = NULL,
                          center = NULL,
                          points = NULL,
                          area = NULL){
      self$name <- name
      self$center <- center
      self$points <- points
      self$area <- area
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of points
      pointsList <- list()
      if(!is.null(self$points)){
        for (i in 1:length(self$points)){
          pointsList[[i]] <- self$points[[i]]$toList()
        }
      } else {
        pointsList[[1]] <- ""
      }

      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Center = if(is.null(self$center)) "" else self$center$toList(),
        Points = if(is.null(self$points)) "" else pointsList,
        Area = if(is.null(self$area)) "" else self$area
      )
      return(dl)
    }
  )
)
