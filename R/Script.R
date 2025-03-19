library("R6")

#' @title CleansingScript
#'
#' @description Class representing a PetroVisor cleansing script object.
#'
#' @export CleansingScript
#'
#' @field name The name of the cleansing script.
#' @field content The script's content as string (P# syntax).
#' @field isLocked This flag specifies whether the script is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the script belongs to.
#' @field isFavorite This flag specifies whether the script is marked as
#'   favorite item, and thus shown in the favorites tab on the home module
#'   in PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the script.
#' @examples
#' \dontrun{
#' CleansingScript$new(name = "Myscript",
#'           content = "content dummy",
#'           isLocked = FALSE,
#'           isFavorite = FALSE)
#'}
CleansingScript <- R6Class("CleansingScript",
  public = list(
    name = NULL,
    content = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new cleansing script instance.
    #'
    #' @param name The name of the cleansing script.
    #' @param content The script's content as string (P# syntax).
    #' @param isLocked This flag specifies whether the script is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the script belongs to.
    #' @param isFavorite This flag specifies whether the script is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the script.
    initialize = function(name = NULL,
                          content = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$content <- content
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
        Content = if(is.null(self$content)) "" else self$content,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)

#' @title PSharpScript
#'
#' @description Class representing a PetroVisor P# script object.
#'
#' @export PSharpScript
#'
#' @field name The name of the script.
#' @field content The script's content as string (P# syntax).
#' @field isLocked This flag specifies whether the script is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the script belongs to.
#' @field isFavorite This flag specifies whether the script is marked as
#'   favorite item, and thus shown in the favorites tab on the home module
#'   in PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the script.
#' @examples
#' \dontrun{
#' PSharpScript$new(name = "Myscript",
#'           content = "content dummy",
#'           isLocked = FALSE,
#'           isFavorite = FALSE)
#'}
PSharpScript <- R6Class("PSharpScript",
  public = list(
    name = NULL,
    content = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new P# script instance.
    #'
    #' @param name The name of the P# script.
    #' @param content The script's content as string (P# syntax).
    #' @param isLocked This flag specifies whether the script is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the script belongs to.
    #' @param isFavorite This flag specifies whether the script is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the script.
    initialize = function(name = NULL,
                          content = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$content <- content
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
        Content = if(is.null(self$content)) "" else self$content,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)

#' @title RScript
#'
#' @description Class representing a PetroVisor R script object.
#'
#' @export RScript
#'
#' @field name The name of the script.
#' @field content The script's content as string.
#' @field isLocked This flag specifies whether the script is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the script belongs to.
#' @field isFavorite This flag specifies whether the script is marked as
#'   favorite item, and thus shown in the favorites tab on the home module
#'   in PetroVisor. Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the script.
#' @examples
#' \dontrun{
#' RScript$new(name = "Myscript",
#'           content = "content dummy",
#'           isLocked = FALSE,
#'           isFavorite = FALSE)
#'}
RScript <- R6Class("RScript",
  public = list(
    name = NULL,
    content = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new R script instance.
    #'
    #' @param name The name of the script.
    #' @param content The script's content as string.
    #' @param isLocked This flag specifies whether the script is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the script belongs to.
    #' @param isFavorite This flag specifies whether the script is marked as
    #'   favorite item, and thus shown in the favorites tab on the home module
    #'   in PetroVisor. Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the script.
    initialize = function(name = NULL,
                          content = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$content <- content
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
        Content = if(is.null(self$content)) "" else self$content,
        IsLocked = if(is.null(self$isLocked)) "" else self$isLocked,
        User = if(is.null(self$user)) "" else self$user,
        IsFavorite = if(is.null(self$isFavorite)) "" else self$isFavorite,
        Labels = if(is.null(self$labels)) "" else self$labels
      )
      return(dl)
    }
  )
)
