context("appenders")


test_that("dummy Appender works as expected", {
  app <- Appender$new()
  x <- as.environment(list(
    level = 2,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))
  expect_match(app$append(x), "environment")
})




test_that("dummy AppenderFormat works as expected", {
  app <- AppenderFormat$new()
  x <- as.environment(list(
    level = 2,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))
  expect_equal(app$append(x), "ERROR [2018-11-02 17:19:33] foo bar")
})



test_that("AppenderFile works as expected", {
  tf <- tempfile()

  app <- AppenderFile$new(file = tf)
  x <- as.environment(list(
    level = 2,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  ))
  app$append(x)
  app$append(x)
  res <- readLines(tf)

  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))
})




test_that("AppenderConsole works as expected", {
  app <- AppenderConsole$new()

  x <- list(
    level = 2,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  )

  expect_identical(
    capture.output(app$append(x)),
    "ERROR [2018-11-02 17:19:33] foo bar"
  )
})



# AppenderMemory ----------------------------------------------------------

test_that("memory cycling works", {
  expect_true(
    data.table::is.data.table(ml$showdt())
  )

  expect_true(nrow(ml$showdt()) == 2L)

  fatalfun <- function() ml$fatal("test ftl")
  expect_output(fatalfun(), "FATAL")
  expect_true(grepl("fatalfun", ml$showdt()[3]$caller))
})


test_that("memory cycling works", {
  ml <- Memlog$new(collector = CollectorDefault$new(
    level = NA_integer_,
    msg = NA_character_,
    .cache_size = 10)
  )

  expect_output(replicate(12, ml$info("blubb info")))
  expect_equal(ml$showdt()$id, 3:12)
})

