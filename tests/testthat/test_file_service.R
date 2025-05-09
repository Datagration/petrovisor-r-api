context("FileService Tests")

# Create test data
test_data <- data.frame(entity = c("One", "Two", "Three"),
                        x = c(1, 2, 3),
                        name = c("This", "is", "something"))

# Save data to csv file
write.csv(test_data, "test_file.csv", row.names = FALSE)

# Perform tests
test_that("File can be uploaded", {
  result <- sp$files$save("test_file.csv")

  expect_equal(result$status_code, 200)
})

test_that("File is available on the server", {
  file_names <- sp$files$load_names()

  expect_true("test_file.csv" %in% file_names)
})

test_that("File can be downloaded", {
  file <- sp$files$load("test_file.csv")

  # Convert content to data.frame
  data_retrieved <- read.csv(file)

  expect_equal(data_retrieved, test_data)
})

test_that("File can be deleted", {
  result <- sp$files$delete("test_file.csv")

  expect_equal(result$status_code, 200)
})
