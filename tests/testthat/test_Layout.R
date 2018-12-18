context("Layout")


tevent <- LogEvent$new(
  logger = Logger$new("dummy", user = "testuser"),
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



test_that("LayoutDbi works as expected", {
  col_types <-  c(
    timestamp = "timestamp",
    level = "smallint",
    msg = "varchar(1024)",
    user = "varchar(256)",
    foo = "varchar(255)"
  )

  expect_error(
    lo <- LayoutDbi$new(
      event_vals = c("level", "timestamp", "msg"),
      logger_vals = "user",
      col_types = col_types
    ), "foo"
  )

  expect_error(
    lo <- LayoutDbi$new(
      event_vals = c("level", "timestamp", "msg", "foo", "caller"),
      logger_vals = "user",
      col_types = col_types
    ), "caller"
  )

  lo <- LayoutDbi$new(
    event_vals = c("level", "timestamp", "msg", "foo"),
    logger_vals = "user",
    col_types = col_types
  )

  x <- tevent$clone()
  x$foo <- "bar"

  eres <- c(x$values, user = x$logger$user)
  tres <- lo$format_event(x)

  tres[sapply(tres, is.null)] <- NA_character_
  expect_setequal(names(eres), c(names(tres), "caller"))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["user"]], eres[["user"]])
  expect_true(is.null(tres[["caller"]]))
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")
})




test_that("LayoutDbi works with custom fields", {
  lo <- LayoutDbi$new(
    event_vals = c("level", "timestamp", "msg", "custom_field"),
    logger_vals = c(logger_name = "name", "user"),
    col_types =  c(
      timestamp = "timestamp",
      user = "varchar(256)",
      logger_name = "varchar(256)",
      level = "smallint",
      msg = "varchar(2048)",
      custom_field = "integer"
    )
  )

  tres1 <- lo$format_event(tevent)
  expect_identical(names(tres1), names(lo$col_types))
  expect_setequal(names(tres1), c(names(lo$event_vals), names(lo$logger_vals)))
  expect_true(is.na(tres1$custom_field))


  # custom fields
    te3 <- tevent$clone()
    te3$foo <- "blah"
    lo <- LayoutDbi$new(
      event_vals  = c(event_foo = "foo"),
      logger_vals = c(logger_user = "user")
    )
    expect_identical(
      lo$format_event(te3),
      data.frame(event_foo = "blah", logger_user = "testuser", stringsAsFactors = FALSE)
    )

  # no fields
    lo <- LayoutDbi$new(
      event_vals  = character(),
      logger_vals = character()
    )
    expect_identical(lo$format_event(x), data.frame())
})




test_that("LayoutJson works as expected", {
  lo <- LayoutJson$new(
    logger_vals = c(logger_user = "user")
  )

  x <- tevent$clone()
  x$foo <- "bar"

  eres <- c(x$values, logger_user = x$logger$user, foo = "bar")
  json <- lo$format_event(x)
  tres <- jsonlite::fromJSON(json)


  tres[sapply(tres, is.null)] <- NA_character_
  expect_setequal(c(names(eres), "foo"), names(tres))
  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_identical(tres[["user"]], eres[["user"]])
  expect_identical(tres[["caller"]], eres[["caller"]])
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
  expect_equal(tres[["foo"]], "bar")


  # named event vals
  lo <- LayoutJson$new(
    event_vals  = c(event_foo = "foo"),
    logger_vals = c(logger_user = "user", "ancestry")
  )

  expect_match(lo$format_event(x), "event_foo")
  expect_match(lo$format_event(x), "logger_user")
  expect_match(lo$format_event(x), "ancestry")


  # no vals
  lo <- LayoutJson$new(
    event_vals  = character(),
    logger_vals = character()
  )
  expect_identical(as.character(lo$format_event(x)), "{}")
})
