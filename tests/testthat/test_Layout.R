context("Layout")


tevent <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = as.POSIXct(1541175573.9308, origin = "1970-01-01", tz = "UTC"),
  caller = NA_character_,
  msg = "foo bar"
)




test_that("Layouts works as expected", {
  lo <- Layout$new()
  expect_match(lo$format_event(tevent), "19:33")
  expect_true(is_scalar_character(lo$format_event(tevent)))
})



test_that("LayoutFormat works as expected", {
  lo <- LayoutFormat$new()
  expect_match(lo$format_event(tevent), "ERROR .*2018-11-02 .*:19:33.*\\.* foo bar")

  lo$set_timestamp_fmt("%Y-%m-%d")
  lo$set_fmt("[%t]")
  expect_identical(lo$format_event(tevent), "[2018-11-02]")
})



test_that("LayoutGlue works as expected", {
  # basic formatting works
  lo <- LayoutGlue$new(fmt = "{level} [{timestamp}] msg")
  expect_match(lo$format_event(tevent), "^200.*")

  # active bindings work
  lo <- LayoutGlue$new(fmt = "{level_name} [{timestamp}] msg")
  expect_match(lo$format_event(tevent), "^error.*")

  # functions work
  lo <- LayoutGlue$new(fmt = "{toupper(level_name)} [{timestamp}] msg")
  expect_match(lo$format_event(tevent), "^ERROR.*")

  # default format works
  lo <- LayoutGlue$new(fmt = "{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] msg")
  expect_match(lo$format_event(tevent), "ERROR.*msg$")

  if (crayon::has_color()){
    expect_true(crayon::has_style(lo$format_event(tevent)))
  }
})



# LayoutJson --------------------------------------------------------------

test_that("LayoutJson works as expected", {
  lo <- LayoutJson$new()

  x <- tevent$clone()
  x$foo <- "bar"

  eres <- x$values
  json <- lo$format_event(x)
  tres <- jsonlite::fromJSON(json)

  tres[sapply(tres, is.null)] <- NA_character_
  expect_setequal(c(names(eres), "foo"), names(tres))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["caller"]], eres[["caller"]])
  expect_equal(as.POSIXct(tres[["timestamp"]], tz = "UTC"), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")
})




test_that("formatting timestamps with LayoutJson works", {
  lo <- LayoutJson$new()
  x <- tevent$clone()

  eres <- x$values
  json <- lo$format_event(x)

  lo$set_timestamp_fmt(function(x) "fmt timestamp with function")
  expect_match(lo$format_event(x), "fmt timestamp with function")

  lo$set_timestamp_fmt("%H:%M:%S..%OS")
  expect_match(lo$format_event(x), format(x$timestamp, "%H:%M:%S..%OS"))
})

