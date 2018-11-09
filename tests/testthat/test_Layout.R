context("layouts")


test_that("layouts works as expected", {

  lo <- Layout$new()
  x <- list(
    level = 200,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  )
  expect_match(lo$format_event(x), "19:33 CET")
  expect_true(is_scalar_character(lo$format_event(x)))
})



test_that("LayoutFormat works as expected", {
  lo <- LayoutFormat$new()
  x <- list(
    level = 200,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  )
  expect_match(lo$format_event(x), "ERROR \\[2018-11-02 17:19:33.*\\] foo bar")

  lo$timestamp_fmt <- "%Y-%m-%d"
  lo$fmt <- "[%t]"
  expect_identical(lo$format_event(x), "[2018-11-02]")
})




test_that("LayoutGlue works as expected", {
  lo <- LayoutGlue$new()
  x <- list(
    level = 200,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    msg = "foo bar"
  )
  expect_equal(lo$format_event(x), "ERROR [2018-11-02 17:19:33] foo bar")
})




test_that("LayoutJson works as expected", {
  lo <- LayoutJson$new(logger_vals = "user")
  x <- LogEvent$new(logger = Logger$new("dummy"))
  x[["level"]] <- 200L
  x[["timestamp"]] <- structure(1541175573.9308, class = c("POSIXct", "POSIXt"))
  x[["caller"]] <- NA_character_
  x[["msg"]] <- "foo bar"

  eres <- c(x$values, user = x$logger$user)
  json <- lo$format_event(x)
  tres <- jsonlite::fromJSON(json)

  expect_setequal(names(eres), names(tres))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["user"]], eres[["user"]])
  expect_identical(tres[["caller"]], eres[["caller"]])
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)

})
