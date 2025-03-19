library("R6")

#' @title MLModel
#'
#' @description Class representing a PetroVisor ml model object.
#'
#' @export MLModel
#'
#' @field name The name of the ml model.
#' @field modelType The model's type.
#' @field trainedModel This field holds the string representation of the
#'   trained ML model.
#' @field tableFormula The model's table formula as string.
#' @field contextFormula The model's context formula as string.
#' @field labelColumnName The model's label column name.
#' @field isLocked This flag specifies whether the model is locked.
#'   Defaults to \code{FALSE}.
#' @field user The user the model belongs to.
#' @field isFavorite This flag specifies whether the model is marked as favorite
#'   item, and thus shown in the favorites tab on the home module in PetroVisor.
#'   Defaults to \code{FALSE}.
#' @field labels A list of strings holding the labels of the model.
#' @examples
#' \dontrun{
#' MLModel$new()
#'}
MLModel <- R6Class("MLModel",
  public = list(
    name = NULL,
    modelType = NULL,
    trainedModel = NULL,
    tableFormula = NULL,
    contextFormula = NULL,
    labelColumnName = NULL,
    isLocked = NULL,
    user = NULL,
    isFavorite = NULL,
    labels = NULL,

    #' @description Create a new ML model instance.
    #'
    #' @param name The name of the ml model.
    #' @param modelType The model's type.
    #' @param trainedModel This field holds the string representation of the
    #'   trained ML model.
    #' @param tableFormula The model's table formula as string.
    #' @param contextFormula The model's context formula as string.
    #' @param labelColumnName The model's label column name.
    #' @param isLocked This flag specifies whether the model is locked.
    #'   Defaults to \code{FALSE}.
    #' @param user The user the model belongs to.
    #' @param isFavorite This flag specifies whether the model is marked as favorite
    #'   item, and thus shown in the favorites tab on the home module in PetroVisor.
    #'   Defaults to \code{FALSE}.
    #' @param labels A list of strings holding the labels of the model.
    initialize = function(name = NULL,
                          modelType = NULL,
                          trainedModel = NULL,
                          tableFormula = NULL,
                          contextFormula = NULL,
                          labelColumnName = NULL,
                          isLocked = FALSE,
                          user = NULL,
                          isFavorite = FALSE,
                          labels = NULL){
      self$name <- name
      self$modelType <- modelType
      self$trainedModel <- trainedModel
      self$tableFormula <- tableFormula
      self$contextFormula <- contextFormula
      self$labelColumnName <- labelColumnName
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
        ModelType = if(is.null(self$modelType)) "" else self$modelType,
        TrainedModel = if(is.null(self$trainedModel)) "" else self$trainedModel,
        TableFormula = if(is.null(self$tableFormula)) "" else self$tableFormula,
        ContextFormula = if(is.null(self$contextFormula)) {
          ""
        } else {
          self$contextFormula
        },
        LabelColumnName = if(is.null(self$labelColumnName)) {
          ""
        } else {
          self$labelColumnName
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
