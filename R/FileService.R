library("R6")

#' @title FileService
#'
#' @description Provides access to file related functionality provided through
#'  the web API.
#'
#' @details A new instance of this class will be created by the ServiceProvider
#'  automatically.
#'
#' @export FileService
#'
#' @examples \dontrun{
#' # create a new instance of the service provider
#' sp <- ServiceProvider$new(
#'   url = discovery_url,
#'   workspace = workspace,
#'   user = user,
#'   password = password
#' )
#'
#' # load file
#' file <- sp$files$load("Test_File.csv")
#'
#' # save file
#' result <- sp$files$save("Test_File.csv")
#' }
FileService <- R6Class(
  "FileService",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(

    #' @description Create a new FileService instance. This is done by the
    #'  ServiceProvider automatically.
    #'
    #' @param url the URL for the API calls.
    #' @param token_type the type of the issued token.
    #' @param token the issued token.
    initialize = function(url, token_type, token) {
      private$url <- url
      private$token_type <- token_type
      private$token <- token
    },

    #' @description Retrieve the names of the files in the workspace's blob
    #'  storage.
    #'
    #' @param prefix If specified, only file names with the given prefix are
    #'  returned.
    #'
    #' @returns A character vector containing the names of the file in the
    #'  workspace's blob storage.
    load_names = function(prefix = NULL) {
      # Build query string
      query <- if (!is.null(prefix)) list(Prefix = prefix) else NULL

      file_names <- private$get(private$url,
                                "Files",
                                private$token_type,
                                private$token,
                                query)
      return(file_names)
    },

    #' @description Download the file with the specified name from the
    #'  workspace's blob storage.
    #'
    #' @param name The name of the file to download.
    #' @param target_path The path to download the file to.
    #'
    #' @returns The content of the file as string.
    load = function(name, target_path = "") {
      response <- private$download_file(name,
                                    private$url,
                          "Files/",
                          private$token_type,
                          private$token)

      # If a path is given, detect the operating system and convert path as
      # needed. Also make sure that the path ends with "/" or "\\".
      if (target_path != "") {
        os <- private$get_os()

        if (os == "windows"){
          target_path <- gsub("/", "\\\\", target_path)
          if (!endsWith("\\", target_path)) {
            target_path <- paste0(target_path, "\\")
          }
        } else {
          target_path <- gsub("\\\\", "/", target_path)
          if (!endsWith("/", target_path)) {
            target_path <- paste0(target_path, "/")
          }
        }
      }

      # Save response content (binary data) to specified path
      con <- file(paste0(target_path, name), "wb")
      writeBin(response$content, con)
      close(con)

      # Return full path to the file
      return(paste0(target_path, name))
    },

    #' @description Upload a file to the workspace's blob storage.
    #'
    #' @param file The file to upload (path incl. file name).
    save = function(file) {
      return(private$upload_file(file,
                                 private$url,
                                 "Files/Upload",
                                 private$token_type,
                                 private$token))
    },

    #' @description Delete the file with the specified name from the
    #'  workspace's blob storage.
    #'
    #' @param name The name of the file to delete.
    delete = function(name) {
      return(private$delete(name,
                            private$url,
                            "Files/",
                            private$token_type,
                            private$token))
    }
  ),
  private = list(
    url = NULL,
    token_type = NULL,
    token = NULL,

    get_os = function(){
      sysinf <- Sys.info()
      if (!is.null(sysinf)){
        os <- sysinf['sysname']
        if (os == 'Darwin')
          os <- "osx"
      } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
          os <- "osx"
        if (grepl("linux-gnu", R.version$os))
          os <- "linux"
      }
      tolower(os)
    }
  )
)
