##### FILTER #####
context("Filter instanciation and conversion to list")

test_that("Filter instanciation and conversion works",{
  filter <- Filter$new(name = "MyFilter",
                       entityNamePattern = "ENamePattern",
                       signalNamePattern = "SNamePattern",
                       entitySetName = "MyEntitySet",
                       checkedEntityNames = list("Well1", "Well2"),
                       checkedSignalNames = list("oil rate", "gas rate"),
                       checkedUnitNames = list("m3/d", "m3/d"),
                       start = "2020-01-01T00:00:00.000Z",
                       end = "2020-03-01T00:00:00.000Z",
                       step = "Monthly",
                       isLocked = TRUE,
                       user = "MyUser",
                       isFavorite = TRUE,
                       labels = list("label1", "label2"))

  listed <- filter$toList()

  expect_equal(listed,
               list(Name = "MyFilter",
                    EntityNamePattern = "ENamePattern",
                    SignalNamePattern = "SNamePattern",
                    EntitySetName = "MyEntitySet",
                    CheckedEntities = list("Well1", "Well2"),
                    CheckedSignals = list("oil rate", "gas rate"),
                    CheckedUnits = list("m3/d", "m3/d"),
                    Start = "2020-01-01T00:00:00.000Z",
                    End = "2020-03-01T00:00:00.000Z",
                    Step = "Monthly",
                    IsLocked = TRUE,
                    User = "MyUser",
                    IsFavorite = TRUE,
                    Labels = list("label1", "label2")))
})

test_that("filter instanciation and conversion works (empty constructor)",{
  filter <- Filter$new()

  listed <- filter$toList()

  expect_equal(listed,
               list(Name = "",
                    EntityNamePattern = "",
                    SignalNamePattern = "",
                    EntitySetName = "",
                    CheckedEntities = "",
                    CheckedSignals = "",
                    CheckedUnits = "",
                    Start = "",
                    End = "",
                    Step = "EveryMinute",
                    IsLocked = FALSE,
                    User = "",
                    IsFavorite = FALSE,
                    Labels = ""))
})
