context("Appender")




x <- LogEvent$new(
  logger = Logger$new("dummy"),
  level = 200L,
  timestamp = structure(1541175573.9308, class = c("POSIXct", "POSIXt")),
  caller = NA_character_,
  msg = "foo bar"
)




# Appender --------------------------------------------------------------------
test_that("dummy Appender works as expected", {
  app <- Appender$new()
  expect_match(app$append(x), "foo bar")
})




test_that("setting appender threshold works", {
  app <- Appender$new()

  app$set_threshold(200)
  expect_identical(app$threshold, 200L)
  app$set_threshold("info")
  expect_identical(app$threshold, 400L)
  app$set_threshold(NA)
  expect_identical(app$threshold, NA_integer_)
  expect_error(app$set_threshold("blubb"), "log levels")
})




# AppenderFile ---------------------------------------------------------
test_that("AppenderFile works as expected", {
  tf <- tempfile()

  # with default format
  app <- AppenderFile$new(file = tf)
  app$append(x)
  app$append(x)
  res <- readLines(tf)
  expect_true(grepl("foo", res[[1]]))
  expect_true(grepl("bar", res[[2]]))

  tf <- tempfile()
  # with Json
  app <- AppenderFile$new(file = tf, layout = LayoutJson$new())
  app$append(x)
  app$append(x)
  tres <- read_json_lines(tf)
  eres <- data.table::rbindlist(list(x$values, x$values))

  expect_identical(tres[["level"]], eres[["level"]])
  expect_identical(tres[["msg"]], eres[["msg"]])
  expect_true(all(is.na(tres[["caller"]])) && all(is.na(eres[["caller"]])))
  expect_equal(as.POSIXct(tres[["timestamp"]]), eres[["timestamp"]], tolerance = 1)
})




# AppenderConsole ---------------------------------------------------------

test_that("AppenderConsole works as expected", {
  app <- AppenderConsole$new()
  expect_match(
    capture.output(app$append(x)),
    "ERROR .*:19:33.* foo bar"
  )
})




# AppenderMemoryDt ----------------------------------------------------------

test_that("AppenderMemoryDt: appending multiple rows works", {
  app <- AppenderMemoryDt$new()
  y <- x$clone()
  y$level <- seq(100L, 300L, 100L)

  expect_silent(app$append(y))


  expect_true(data.table::is.data.table(app$data))
  expect_identical(app$data$level[1:3], y$level)
  expect_identical(app$data$timestamp[1:3], rep(y$timestamp, 3))
  expect_identical(app$data$msg[1:3], rep(y$msg, 3))
  expect_identical(app$data$caller[1:3], rep(NA_character_, 3))

  y <- x$clone()
  y$level <- 300
  app$append(y)
  expect_identical(app$.__enclos_env__$private$.data$.id[1:4], 1:4)

  expect_match(paste(capture.output(app$show()), collapse = ""), "ERROR.*WARN")
})




# AppenderBuffer ----------------------------------------------------

test_that("AppenderBuffer behaves as expected", {
  tf <- tempfile()

  # Sub sub appenders must have a reference to the original logger
  l <- Logger$new(
    "dummy",
    appenders = list(
      buffer = AppenderBuffer$new(
        appenders = list(file = AppenderFile$new(file = tf)),
        buffer_size = 10
      )
    ),
    parent = NULL
  )
  l$info(LETTERS[1:3])
  expect_identical(length(l$appenders$buffer$buffered_events), 1L)
  l$info(LETTERS[4:7])
  expect_identical(length(l$appenders$buffer$buffered_events), 2L)


  # FATAL log level triggers flush
  l$fatal(letters[1:3])
  expect_identical(l$appenders$buffer$buffered_events, list())
  expect_match(
    paste(readLines(tf), collapse = "#"),
    "INFO.*A#INFO.*B#INFO.*C#INFO.*D#INFO.*E#INFO.*F#INFO.*G#FATAL.*a#FATAL.*b#FATAL.*c"
  )

  # Does the next flush flush the correct event?
  l$fatal("x")
  expect_identical(length(readLines(tf)), 11L)
  expect_identical(l$appenders$buffer$buffered_events, list())
  expect_match(paste(readLines(tf), collapse = "#"), ".*A#.*B#.*C#.*a#.*b#.*c#.*x")

  # does flushing on memory cycling work?
  replicate(10, l$info("z"))
  expect_identical(length(l$appenders$buffer$buffered_events), 10L)
  l$info(c("y", "y", "y"))
  expect_identical(length(readLines(tf)), 24L)
  expect_identical(l$appenders$buffer$buffered_events, list())
  expect_match(
    paste(readLines(tf), collapse = "#"),
    ".*A#.*B#.*C#.*a#.*b#.*c#.*x(.*z.*){10}(.*y.*){3}"
  )

  # manual flushing works
  l$info(c("y", "y", "y"))
  l$appenders$buffer$flush()
  expect_identical(length(readLines(tf)), 27L)
  eres <- readLines(tf)
  expect_match(paste(eres, collapse = "#"), "(.*z.*){10}(.*y.*){6}")


  # memory cycling without flushing also works
  l$appenders$buffer$set_flush_on_rotate(FALSE)
  for (i in 1:15) l$info(i)
  expect_identical(length(l$appenders$buffer$buffered_events), 10L)
  msgs <- sapply(l$appenders$buffer$buffered_events, `[[`, "msg")
  expect_identical(msgs, as.character(6:15))
  # Nothing should have been flushed to the log file
  expect_identical(readLines(tf), eres)


  # does flushing on object destruction work?
  l$appenders$buffer$flush()  # ensure empty appender
  l$info(c("destruction", "destruction"))
  expect_identical(length(l$appenders$buffer$buffered_events), 1L)
  rm(l)
  gc()
  expect_match(
    paste(readLines(tf), collapse = "#"),
    "(.*destruction){2}"
  )



  # does flushing honor log levels and filters??
  try(file.remove(tf), silent = TRUE)
})




test_that("AppenderBuffer: dont flush on object destruction if switched of", {
  tf <- tempfile()

  # Sub sub appenders must have a reference to the original logger
  l <- Logger$new(
    "dummy",
    appenders = list(
      buffer = AppenderBuffer$new(
        appenders = list(file = AppenderFile$new(file = tf)),
        buffer_size = 10
      )
    ),
    parent = NULL
  )
  l$info(LETTERS[1:3])
  l$appenders$buffer$set_flush_on_exit(FALSE)
  rm(l)
  gc()
  expect_true(!file.exists(tf))
  suppressWarnings(file.remove(tf))
})




test_that("AppenderMemory: memory cycling works", {
  app1 <- AppenderMemoryDt$new(buffer_size = 10)
  replicate(12, app1$append(x))
  expect_equal(app1$data$.id, 3:12)
  r1 <- app1$data

  # bulk insert behaves like sepparate inserts
  app2 <- AppenderMemoryDt$new(buffer_size = 10)
  y <- x$clone()
  y$msg <- rep(y$msg, 12)

  app2$append(y)
  expect_equal(app2$data$.id,  3:12)
  expect_equal(app2$data, r1)
})




test_that("Appender: filters work", {
  app1 <- AppenderConsole$new()
  expect_true(app1$filter(x))
  app1$set_threshold(100)
  expect_false(app1$filter(x))
})





# AppenderDbi + AppenderRjdbc ---------------------------------------------

# +- Multiple RDMS ----------------------------------------------------------

dt_sp <- options("datatable.showProgress")
options("datatable.showProgress" = FALSE)

dbs <- list(
  "MySQL via RMySQL" = list(
    conn = try(silent = TRUE, DBI::dbConnect(
      RMySQL::MySQL(),
      username = "travis",
      dbname = "travis_ci_test",
      host = "localhost"
    )),
    ctor = AppenderDbi
  ),

  "MySQL via RMariaDB" = list(
    conn = try(silent = TRUE, DBI::dbConnect(
      RMariaDB::MariaDB(),
      username = "travis",
      dbname = "travis_ci_test",
      host = "localhost"
    )),
    ctor = AppenderDbi
  ),

  "PostgreSQL" = list(
    conn = try(silent = TRUE, DBI::dbConnect(
      RPostgreSQL::PostgreSQL(),
      host = "localhost",
      dbname = "travis_ci_test"
    )),
    ctor = AppenderDbi
  ),

  "DB2 via RJDBC" = list(
    conn = try(silent = TRUE, dataSTAT::dbConnectDB2("RTEST", "rtest", "rtest")),
    ctor = AppenderRjdbc
  ),

  # keep SQLite last so that the testthat::context calls stay correct
  "SQLite via RSQLite" = list(
    conn = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    ctor = AppenderDbi
  )
)

options("datatable.showProgress" = dt_sp)


for (nm in names(dbs)){
  conn <- dbs[[nm]]$conn
  ctor <- dbs[[nm]]$ctor

  context(paste("AppenderDbi /", nm))

  test_that(paste("AppenderDbi /", nm), {
    if (inherits(conn, "try-error")){
      skip(trimws(paste("Cannot connect to", nm, "database: ", conn)))
    }

    # setup test environment
      tname <- "logging_test"
      expect_message(
        app <- ctor$new(
          conn = conn,
          table = tname,
          close_on_exit = FALSE  # we are closing manually and dont want warnings
        ),
        "Creating"
      )
      e <- LogEvent$new(
        yog, level = 600L, msg = "ohno", caller = "nope()", timestamp = Sys.time()
      )

    # round trip event inserts
      expect_silent(app$append(e))
      expect_silent(app$append(e))
      tres <- app$data
      eres <- rbind(
        as.data.frame(e, stringsAsFactors = FALSE),
        as.data.frame(e, stringsAsFactors = FALSE)
      )
      expect_equal(tres[, -2], eres[, -2])
      # small tolerance is allowed for timestamps
      tdiff <- as.numeric(tres[, 2]) - as.numeric(eres[, 2])
      expect_true(all(tdiff < 1), info = tdiff)
      expect_true(all(tres$timestamp == format(e$timestamp)))

    # col order does not impact inserts
      for (i in 1:20){
        app$layout$set_col_types(sample(app$layout$col_types))
        expect_silent(app$append(e))
      }
      expect_true(all(vapply(app$data$timestamp, all_are_identical, logical(1))))
      expect_true(all(format(app$data$timestamp) == format(e$timestamp)))


    # log display
      expect_output(app$show(5), paste(rep("TRACE.*", 5), collapse = "") )
      expect_output(expect_identical(nrow(app$show(1)), 1L), "TRACE")
      expect_output(expect_identical(show_log(target = app), app$show()))
      expect_identical(
        capture.output(show_log(target = app)),
        capture.output(app$show())
      )


    # cleanup
    expect_true(
      DBI::dbExistsTable(conn, tname) ||
      DBI::dbExistsTable(conn, toupper(tname))
    )
    expect_silent({
      DBI::dbRemoveTable(conn, tname)
      expect_false(DBI::dbExistsTable(conn, tname))
      DBI::dbDisconnect(conn)
    })
  })
}




# +- RSQLite extra tests --------------------------------------------------

test_that("AppenderDbi / RSQLite: manual field types work", {
  if (!requireNamespace("RSQLite", quietly = TRUE))
    skip("Test requires RSQLite")

  # setup test environment
  tdb <- tempfile()
  tname <- "LOGGING_TEST"
  expect_message(
    app <- AppenderDbi$new(
      conn = DBI::dbConnect(RSQLite::SQLite(), tdb),
      layout = LayoutSqlite$new(col_types = c(
        level = "INTEGER",
        timestamp = "TEXT",
        caller = "TEXT",
        msg = "TEXT"
      )),
    table = tname
    ),
  "column types"
  )
  e <- LogEvent$new(yog, level = 600, msg = "ohno", caller = "nope()", timestamp = Sys.time())

  # do a few inserts
  for (i in 1:10){
    app$layout$set_col_types(sample(app$layout$col_types))
    expect_silent(app$append(e))
  }

  # verify correct data types (sqlite doesnt have that many)
  t <- DBI::dbGetQuery(app$conn, sprintf("PRAGMA table_info(%s)", tname))
  expect_true(t[t$name == "level", ]$type == "INTEGER")
  expect_true(all(vapply(app$data$timestamp, all_are_identical, logical(1))))
  expect_true(all(format(app$data$timestamp) == format(e$timestamp)))

  # cleanup
  rm(app)
  gc()
  unlink(tdb)
})




test_that("AppenderDbi / RSQLite: displaying logs works for Loggers", {
  if (!requireNamespace("RSQLite", quietly = TRUE))
    skip("Test requires RSQLite")

  # Setup test environment
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  tname <- "LOGGING_TEST"
  expect_message(
    lg <- Logger$new(
      "test_dbi",
      threshold = "trace",
      appenders = list(db = AppenderDbi$new(conn = conn, table = tname, close_on_exit = FALSE)),
      propagate = FALSE
    ),
    "Creating.*on first log"
  )

  lg$fatal("blubb")
  lg$trace("blah")

  expect_output(lg$appenders$db$show(), "FATAL.*TRACE")
  expect_output(
    expect_identical(nrow(lg$appenders$db$show(1)), 1L),
    "TRACE"
  )

  expect_identical(nrow(lg$appenders$db$data), 2L)

  expect_output(
    expect_identical(
      show_log(target = lg),
      lg$appenders$db$show()
    )
  )

  expect_silent(DBI::dbDisconnect(conn))
})




test_that("AppenderDbi / RSQlite: Automatic closing of connections works", {
  if (!requireNamespace("RSQLite", quietly = TRUE))
    skip("Test requires RSQLite")

  # setup test environment
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  tname <- "LOGGING_TEST"

  # With close_on_exit
  lg <- Logger$new(
    "test_dbi",
    threshold = "trace",
    appenders = list(db = AppenderDbi$new(conn = conn, table = tname, close_on_exit = TRUE))
  )
  rm(lg)
  gc()
  expect_warning(DBI::dbDisconnect(conn), "Already disconnected")


  # Without close_on_exit
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  tname <- "LOGGING_TEST"
  lg <- Logger$new(
    "test_dbi",
    threshold = "trace",
    appenders = list(db = AppenderDbi$new(conn = conn, table = tname, close_on_exit = FALSE))
  )
  rm(lg)
  gc()
  expect_silent(DBI::dbDisconnect(conn))
})
