# LayoutJson --------------------------------------------------------------

test_that("LayoutJson works as expected", {
  lo <- LayoutJson$new()

  event <- LogEvent$new(
    logger = Logger$new("dummy"),
    level = 200L,
    timestamp = as.POSIXct(1541175573.9308, origin = "1970-01-01", tz = "UTC"),
    caller = NA_character_,
    msg = "foo bar",
    rawMsg = "foo raw",
    customField = "hashbaz",
    customField2 = "barfoo",
    foo = "bar"
  )

  eres <- event$values
  json <- lo$format_event(event)
  tres <- jsonlite::fromJSON(json)

  expected_values <- c("level", "timestamp", "logger", "caller", "msg", "foo", "customField", "customField2")  # rawMsg is excluded by default

  tres[sapply(tres, is.null)] <- NA_character_
  expect_setequal(expected_values, names(tres))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["caller"]], eres[["caller"]])
  expect_identical(tres[["customField"]], eres[["customField"]])
  expect_identical(tres[["customField"]], eres[["customField"]])
  expect_equal(as.POSIXct(tres[["timestamp"]], tz = "UTC"), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")
})


test_that("LayoutJson `excluded_fields` works as expected", {
  lo <- LayoutJson$new(excluded_fields = c("customField", "customField2"))

  event <- LogEvent$new(
    logger = Logger$new("dummy"),
    level = 200L,
    timestamp = as.POSIXct(1541175573.9308, origin = "1970-01-01", tz = "UTC"),
    caller = NA_character_,
    msg = "foo bar",
    rawMsg = "foo raw",
    customField = "hashbaz",
    customField2 = "barfoo",
    foo = "bar"
  )

  eres <- event$values
  json <- lo$format_event(event)
  tres <- jsonlite::fromJSON(json)

  expected_values <- c("level", "timestamp", "logger", "caller", "msg", "foo", "rawMsg")

  tres[sapply(tres, is.null)] <- NA_character_
  expect_setequal(expected_values, names(tres))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["caller"]], eres[["caller"]])
  expect_equal(as.POSIXct(tres[["timestamp"]], tz = "UTC"), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")
  expect_equal(tres[["rawMsg"]], "foo raw")
})


test_that("formatting timestamps with LayoutJson works", {
  lo <- LayoutJson$new()

  event <- LogEvent$new(
    logger = Logger$new("dummy"),
    level = 200L,
    timestamp = as.POSIXct(1541175573.9308, origin = "1970-01-01", tz = "UTC"),
    caller = NA_character_,
    msg = "foo bar",
    rawMsg = "foo raw",
    customField = "hashbaz",
    customField2 = "barfoo"
  )

  eres <- event$values
  json <- lo$format_event(event)

  lo$set_timestamp_fmt(function(event) "fmt timestamp with function")
  expect_match(lo$format_event(event), "fmt timestamp with function")

  lo$set_timestamp_fmt("%H:%M:%S..%OS")
  expect_match(lo$format_event(event), format(event$timestamp, "%H:%M:%S..%OS"))
})


test_that("LayoutJson.format_event() - with field rename function - renames fields correctly", {

  # Arrange
  event <- LogEvent$new(
    logger = Logger$new("dum/my"),
    level = 200L,
    timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
    caller = NA_character_,
    msg = "foo bar",
    rawMsg = "foobar-raw"
  )

  lo <- LayoutJson$new(
    transform_event_names = toupper,
    excluded_fields = c("RAWMSG", "CALLER")
  )

  # Act
  res <- jsonlite::fromJSON(lo$format_event(event))

  # Assert
  expect_setequal(
    names(res),
    c("LOGGER", "LEVEL", "TIMESTAMP", "MSG"))

  expect_identical(res$MSG, "foo bar")
  expect_identical(res$LEVEL, 200L)
  expect_identical(res$LOGGER, "dum/my")
})
