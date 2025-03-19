library("R6")

#' @title Polygon
#'
#' @description Class representing a PetroVisor polygon object.
#'
#' @export Polygon
#'
#' @field name The name of the polygon.
#' @field points A list of points (objects of class Point) that define the
#'   polygon.
#' @field linkedEntityName The name of the entity linked to the polygon.
#' @field isLocked This flag specifies whether the polygon is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the polygon belongs to.
#' @field isFavorite This flag specifies whether the polygon is marked as
#'   favorite item, and thus shown in the favorites tab on the home module in
#'   PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the polygon.
#' @examples
#' \dontrun{
#' Polygon$new(name = "MyPolygon",
#'           points = list(Point$new(x = 10, y = 20),
#'                         Point$new(x = 20, y = 10),
#'                         Point$new(x = 30, y = 40)))
#'}
Polygon <- R6Class("Polygon",
  public = list(
    name = NULL,
    points = NULL,
    linkedEntityName = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new Polygon instance.
    #'
    #' @param name The name of the polygon.
    #' @param points A list of points (objects of class Point) that define the
    #'   polygon.
    #' @param linkedEntityName The name of the entity linked to the polygon.
    #' @param isLocked This flag specifies whether the polygon is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the polygon belongs to.
    #' @param isFavorite This flag specifies whether the polygon is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the polygon.
    initialize = function(name = NULL,
                          points = NULL,
                          linkedEntityName = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$points <- points
      self$linkedEntityName <- linkedEntityName
      self$isLocked <- isLocked
      self$user <- user
      self$isFavorite <- isFavorite
      self$labels <- labels
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      # create list from list of points
      pointList <- list()
      if(!is.null(self$points)){
        for (i in 1:length(self$points)){
          pointList[[i]] <- self$points[[i]]$toList()
        }
      } else {
        pointList[[1]] <- ""
      }


      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Points = if(is.null(self$points)) "" else pointList,
        LinkedEntityName = if(is.null(self$linkedEntityName)) {
          ""
        } else {
          self$linkedEntityName
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
