##### POLYGON #####
context("Polygon instantiation and conversion to list")

test_that("Polygon instantiation and conversion works",{
  point1 <- Point$new(x = 5,
                      y = 5)

  point2 <- Point$new(x = 10,
                      y = 10)

  point3 <- Point$new(x = 7,
                      y = 7)

  polygon <- Polygon$new(name = "MyPolygon",
                         points = list(point1, point2, point3),
                         linkedEntityName = "LinkedEntityName",
                         isLocked = TRUE,
                         user = "MyUser",
                         isFavorite = TRUE,
                         labels = list("label1", "label2"))

  listed <- polygon$toList()

  expect_equal(listed,
               list(Name = "MyPolygon",
                    Points = list(point1$toList(),
                                  point2$toList(),
                                  point3$toList()),
                    LinkedEntityName = "LinkedEntityName",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Entity set instantiation and conversion works (empty constructor)",{
  polygon <- Polygon$new()

  listed <- polygon$toList()

  expect_equal(listed,
               list(Name = "",
                    Points = "",
                    LinkedEntityName = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
