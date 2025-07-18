context("Layout")


tevent <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = as.POSIXct(1541175573.9308, origin = "1970-01-01", tz = "UTC"),
  caller = NA_character_,
  msg = "foo bar",
  rawMsg = "foo raw",
  customField = "hashbaz",
  customField2 = "barfoo"
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


test_that("LayoutFormat works as expected with custom fields", {
  lo <- LayoutFormat$new("%r %j")
  expect_identical(lo$format_event(tevent), "foo raw {\"customField\":\"hashbaz\",\"customField2\":\"barfoo\"}")

  lo$set_timestamp_fmt("%Y-%m-%d")
  lo$set_fmt("[%t]")
  expect_identical(lo$format_event(tevent), "[2018-11-02]")
})


test_that("LayoutFormat works as expected with custom fields", {
  lo <- LayoutFormat$new("%r %j", excluded_fields = "customField2")
  expect_identical(lo$format_event(tevent), "foo raw {\"customField\":\"hashbaz\"}")

  lo$set_timestamp_fmt("%Y-%m-%d")
  lo$set_fmt("[%t]")
  expect_identical(lo$format_event(tevent), "[2018-11-02]")
})



test_that("LayoutGlue works as expected", {
  # basic formatting works
  lo <- LayoutGlue$new(fmt = "{level} [{timestamp}] {msg}")
  expect_match(lo$format_event(tevent), "^200.*foo bar$")

  # active bindings work
  lo <- LayoutGlue$new(fmt = "{level_name} [{timestamp}] {msg}")
  expect_match(lo$format_event(tevent), "^error.*foo bar$")

  # functions work
  lo <- LayoutGlue$new(fmt = "{toupper(level_name)} [{timestamp}] {msg}")
  expect_match(lo$format_event(tevent), "^ERROR.*foo bar$")

  # default format works
  lo <- LayoutGlue$new(fmt = "{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] {msg}")
  expect_match(lo$format_event(tevent), "ERROR.*foo bar$")

  if (crayon::has_color()){
    expect_true(crayon::has_style(lo$format_event(tevent)))
  }
})
