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
FileService <- R6Class( # nolint: object_name_linter
  "FileService",
  inherit = ApiRequests, # inherit methods from ApiRequests class
  public = list(

    #' @description Create a new FileService instance. This is done by the
    #'  ServiceProvider automatically.
    initialize = function() {},

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

      file_names <- super$get(route = "Files",
                              query = query)
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
      response <- super$download_file(name = name,
                                      route = "Files/")

      # If a path is given, detect the operating system and convert path as
      # needed. Also make sure that the path ends with "/" or "\\".
      if (target_path != "") {
        os <- private$get_os()

        if (os == "windows") {
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
      return(super$upload_file(file = file,
                               route = "Files/Upload"))
    },

    #' @description Delete the file with the specified name from the
    #'  workspace's blob storage.
    #'
    #' @param name The name of the file to delete.
    delete = function(name) {
      return(super$delete(name = name,
                          route = "Files/"))
    }
  ),
  private = list(
    get_os = function() {
      sysinf <- Sys.info()
      if (!is.null(sysinf)) {
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
