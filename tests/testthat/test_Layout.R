context("Layout")


tevent <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
  caller = NA_character_,
  msg = "foo bar"
)




test_that("layouts works as expected", {
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



test_that("LayoutDbi works as expected", {
  col_types <-  c(
    timestamp = "timestamp",
    level = "smallint",
    logger = "varchar(512)",
    msg = "varchar(1024)",
    foo = "varchar(255)"
  )

  expect_error(
    lo <- LayoutDbi$new(
      event_values = c("level", "timestamp", "msg"),
      col_types = col_types
    ), "either"
  )


  lo <- LayoutDbi$new(
    col_types = col_types
  )

  x <- tevent$clone()
  x$foo <- "bar"

  eres <- x$values
  tres <- lo$format_event(x)

  tres[sapply(tres, is.null)] <- NA_character_
  expect_setequal(names(eres), c(names(tres), "caller"))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_true(is.null(tres[["caller"]]))
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")
})




test_that("LayoutDbi works with custom fields", {
  lo <- LayoutDbi$new(
    col_types =  c(
      timestamp = "timestamp",
      logger = "varchar(256)",
      level = "smallint",
      msg = "varchar(2048)",
      custom_field = "integer"
    )
  )

  tres1 <- lo$format_event(tevent)
  expect_identical(names(tres1), names(lo$col_types))
  expect_setequal(names(tres1), lo$event_values)
  expect_true(is.na(tres1$custom_field))


  # custom fields
    te3 <- tevent$clone()
    te3$foo <- "blah"
    lo <- LayoutDbi$new(event_values  = "foo")
    expect_identical(
      lo$format_event(te3),
      data.frame(foo = "blah", stringsAsFactors = FALSE)
    )

  # no fields
    lo <- LayoutDbi$new(event_values  = character())
    expect_identical(lo$format_event(x), data.frame())
})




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
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")


  # named event vals
  lo <- LayoutJson$new(event_values  = "foo")
  expect_match(lo$format_event(x), "foo")


  # no vals
  lo <- LayoutJson$new(
    event_values  = character()
  )
  expect_identical(as.character(lo$format_event(x)), "{}")
})
