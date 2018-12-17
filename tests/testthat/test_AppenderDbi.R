# Test multiple RDBMS

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

  "SQLite via RSQLite" = list(
    conn = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    ctor = AppenderDbi
  )
)

options("datatable.showProgress" = dt_sp)



for (nm in names(dbs)){
  conn <- dbs[[nm]]$conn
  ctor <- dbs[[nm]]$ctor
  title <- paste(ctor$classname, "/", nm)

  context(title)
  if (inherits(conn, "try-error")) {
    test_that(title, {trimws(strwrap(skip(conn)))})
    next
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


  test_that("round trip event inserts", {
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
  })


  test_that("col order does not impact inserts", {
    for (i in 1:20){
      app$layout$set_col_types(sample(app$layout$col_types))
      expect_silent(app$append(e))
    }
    expect_true(all(vapply(app$data$timestamp, all_are_identical, logical(1))))
    expect_true(all(format(app$data$timestamp) == format(e$timestamp)))
  })


  test_that("querying / displaying logs works", {
    expect_output(app$show(5), paste(rep("TRACE.*", 5), collapse = "") )
    expect_output(expect_identical(nrow(app$show(1)), 1L), "TRACE")
    expect_output(expect_identical(show_log(target = app), app$show()))
    expect_identical(
      capture.output(show_log(target = app)),
      capture.output(app$show())
    )
  })


  test_that("cleanup behaves as expected", {
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



context("AppenderDbi / SQLite: Extra Tests")


test_that("displaying logs works for Loggers", {
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




test_that("Automatic closing of connections works", {
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
