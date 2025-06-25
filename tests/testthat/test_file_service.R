context("FileService Tests")

test_that("Files can be handled", {
  withr::with_tempfile(
    "path",
    {
      # Create test data
      test_data <- data.frame(entity = c("One", "Two", "Three"),
                              x = c(1, 2, 3),
                              name = c("This", "is", "something"))

      # Save data to csv file
      write.csv(test_data, path, row.names = FALSE)

      # File can be uploaded
      result <- sp$files$save(path)
      expect_equal(result$status_code, 200)

      # File is available on server
      file_names <- sp$files$load_names()
      expect_true(basename(path) %in% file_names)

      # When using the file name as prefix, only one file should be returned
      file_names2 <- sp$files$load_names(prefix = basename(path))
      expect_equal(length(file_names2), 1)

      # File can be downloaded
      withr::with_tempfile(
        "path2",
        {
          file <- sp$files$load(basename(path), dirname(path2))
          # Convert content to data.frame
          data_retrieved <- read.csv(file)
          expect_equal(data_retrieved, test_data)
        },
        fileext = ".csv"
      )

      # File can be deleted from server
      result <- sp$files$delete(basename(path))
      expect_equal(result$status_code, 200)
    },
    fileext = ".csv"
  )
})
