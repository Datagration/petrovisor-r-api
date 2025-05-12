##### VORONOI GRID #####
context("Voronoi grid instantiation and conversion to list")

test_that("Voronoi grid instantiation and conversion works",{
  np1 <- NamedPoint$new(name = "MyPoint1", tagName = "MyTag1", x = 10, y = 20)
  np2 <- NamedPoint$new(name = "MyPoint2", tagName = "MyTag2", x = 1, y = 2)

  p1 <- Point$new(x = 10, y = 20)
  p2 <- Point$new(x = 1, y = 2)

  c1 <- VoronoiCell$new(name = "MyCell1",
                        center = p1,
                        points = list(p1, p2),
                        area = 25.98)
  c2 <- VoronoiCell$new(name = "MyCell2",
                        center = p2,
                        points = list(p1, p2),
                        area = 45.785)

  vg <- VoronoiGrid$new(name = "MyGrid",
                        centralPoints = list(np1, np2),
                        boundaries = list(np1, np2),
                        borders = list(p1, p2),
                        cells = list(c1, c2),
                        areaUnitName = "m2",
                        isLocked = TRUE,
                        user = "MyUser",
                        isFavorite = TRUE,
                        labels = list("label1", "label2"))

  listed <- vg$toList()

  expect_equal(listed,
               list(Name = "MyGrid",
                    CentralPoints = list(np1$toList(), np2$toList()),
                    Boundaries = list(np1$toList(), np2$toList()),
                    Borders = list(p1$toList(), p2$toList()),
                    Cells = list(c1$toList(), c2$toList()),
                    AreaUnitName = "m2",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("Voronoi grid instantiation and conversion works
          (empty constructor)",{
  vg <- VoronoiGrid$new()

  listed <- vg$toList()

  expect_equal(listed,
               list(Name = "",
                    CentralPoints = "",
                    Boundaries = "",
                    Borders = "",
                    Cells = "",
                    AreaUnitName = "",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})

##### VORONOI CELL #####
context("Voronoi cell instantiation and conversion to list")

test_that("Voronoi cell instantiation and conversion works",{
  c <- Point$new(x = 20,
                 y = 10)

  p1 <- Point$new(x = 1,
                  y = 1)
  p2 <- Point$new(x = 4,
                  y = 7)
  p3 <- Point$new(x = 25,
                  y = 30)

  vc <- VoronoiCell$new(name = "MyCell",
                        center = c,
                        points = list(p1, p2, p3),
                        area = 25.98)

  listed <- vc$toList()

  expect_equal(listed,
               list(Name = "MyCell",
                    Center = c$toList(),
                    Points = list(p1$toList(), p2$toList(), p3$toList()),
                    Area = 25.98))
})

test_that("Voronoi cell instantiation and conversion works
          (empty constructor)",{
  vc <- VoronoiCell$new()

  listed <- vc$toList()

  expect_equal(listed,
               list(Name = "",
                    Center = "",
                    Points = "",
                    Area = ""))
})
