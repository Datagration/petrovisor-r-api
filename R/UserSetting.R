library("R6")

#' @title UserSetting
#'
#' @description Class representing a PetroVisor UserSetting object.
#'
#' @export UserSetting
#'
#' @field name The name of the user setting.
#' @field value The value of the user setting.
#'
#' @examples
#' \dontrun{
#' UserSetting$new(name = "MySetting", value = "MySettingValue")
#'}
UserSetting <- R6Class("UserSetting",
  public = list(
    name = NULL,
    value = NULL,

    #' @description Create a new UserSetting instance.
    #'
    #' @param name The name of the user setting.
    #' @param value The value of the user setting.
    initialize = function(name = NULL, value = NULL){
      self$name <- name
      self$value <- value
    },

    #' @details Convert the object to a list. This function is mainly used
    #' by the RepositoryService to convert the objects to lists and then
    #' call the web API.
    toList = function(){
      dl <- list(
        Name = if(is.null(self$name)) "" else self$name,
        Value = if(is.null(self$value)) "" else self$value)
      return(dl)
    }
  )
)
