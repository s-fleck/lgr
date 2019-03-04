# Test multiple RDBMS

dt_sp <- options("datatable.showProgress")
options("datatable.showProgress" = FALSE)




# RDBMS batch tests -------------------------------------------------------


# +- setup ----------------------------------------------------------------



tsqlite <- tempfile()

dbs <- list(
  "MySQL via RMariaDB" = list(
    conn = try(silent = TRUE, DBI::dbConnect(
      RMariaDB::MariaDB(),
      username = "travis",
      dbname = "travis_ci_test",
      host = "localhost"
    )),
    ctor = AppenderDbi
  ),

  "MySQL via RMySQL" = list(
    conn = try(silent = TRUE, DBI::dbConnect(
      RMySQL::MySQL(),
      username = "travis",
      dbname = "travis_ci_test",
      host = "localhost"
    )),
    ctor = AppenderDbi
  ),

  "PostgreSQL via RPostgreSQL" = list(
    conn = try(silent = TRUE, DBI::dbConnect(
      RPostgreSQL::PostgreSQL(),
      user = "postgres",
      host = "localhost",
      dbname = "travis_ci_test"
    )),
    ctor = AppenderDbi
  ),

  "PostgreSQL via RPostgres" = list(
    conn = try(silent = TRUE, DBI::dbConnect(
      RPostgres::Postgres(),
      user = "postgres",
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
    conn = DBI::dbConnect(RSQLite::SQLite(), database = tsqlite),
    ctor = AppenderDbi
  )
)

options("datatable.showProgress" = dt_sp)

nm <- "SQLite via RSQLite"  # for manual testing, can be deleted
nm <- "DB2 via RJDBC" # for manual testing, can be deleted



# +- tests -------------------------------------------------------------------

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

  suppressMessages(
    app <- ctor$new(
      conn = conn,
      table = tname,
      close_on_exit = FALSE,  # we are closing manually and dont want warnings
      buffer_size = 0L
    )
  )

  e <- LogEvent$new(
    lgr, level = 600L, msg = "ohno", caller = "nope()", timestamp = Sys.time()
  )


  test_that(paste0(nm, ": round trip event inserts"), {
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
    expect_true(all(format(tres$timestamp) == format(e$timestamp, usetz = FALSE)))
  })


  test_that(paste0(nm, ": col order does not impact inserts"), {
    for (i in 1:20){
      app$layout$set_col_types(sample(app$layout$col_types))
      expect_silent(app$append(e))
    }
    expect_true(all(vapply(app$data$timestamp, all_are_identical, logical(1))))
    expect_true(all(format(app$data$timestamp) == format(e$timestamp)))
  })


  test_that(paste0(nm, ": querying / displaying logs works"), {
    expect_output(app$show(n = 5), paste(rep("TRACE.*", 5), collapse = "") )
    expect_output(expect_identical(nrow(app$show(n = 1)), 1L), "TRACE")
    expect_output(expect_identical(show_log(target = app), app$show()))
    expect_identical(
      capture.output(show_log(target = app)),
      capture.output(app$show())
    )
  })


  # custom fields
  test_that(paste0(nm, ": Creating tables with custom fields works"), {
    try(DBI::dbRemoveTable(conn, "logging_test_create"), silent = TRUE)

    lg <- Logger$new(
      "test_dbi",
      threshold = "trace",
      propagate = FALSE,
      exception_handler = function (...) stop(...)
    )


    if (ctor$classname == "AppenderRjdbc"){
      lo <- LayoutRjdbc$new(
        col_types = c(
          level = "smallint",
          timestamp = "timestamp",
          logger= "varchar(512)",
          msg = "varchar(1024)",
          caller = "varchar(1024)",
          foo = "varchar(256)"
        )
      )
    } else {
      lo <- LayoutSqlite$new(
        col_types = c(
          level = "INTEGER",
          timestamp = "TEXT",
          logger= "TEXT",
          msg = "TEXT",
          caller = "TEXT",
          foo = "TEXT"
        )
      )
    }

    expect_message(
      lg$add_appender(
        ctor$new(
          conn = conn,
          table = "logging_test_create",
          layout = lo,
          close_on_exit = FALSE
        ), "db"
      ),
    "Creating"
    )

    lg$fatal("test", foo = "bar")
    expect_false(is.na(lg$appenders$db$data$foo[[1]]))
    lg$fatal("test")
    expect_true(is.na(lg$appenders$db$data$foo[[2]]))
    lg$remove_appender("db")
  })


  test_that(paste0(nm, ": Log to all fields that are already present in table by default"), {
    lg <- Logger$new(
      "test_dbi",
      threshold = "trace",
      propagate = FALSE,
      exception_handler = function (...) stop(...)
    )

    lg$set_appenders(list(db =
      ctor$new(
        conn = conn,
        table = "logging_test_create",
        close_on_exit = FALSE
      ))
    )

    lg$fatal("test2", foo = "baz", blubb = "blah")
    expect_identical(tail(lg$appenders$db$data, 1)$foo, "baz")

    try(DBI::dbRemoveTable(conn, "logging_test_create"), silent = TRUE)
  })


  test_that(paste0(nm, ": Buffered inserts work"), {
    lg <- Logger$new(
      "test_dbi",
      threshold = "trace",
      propagate = FALSE,
      exception_handler = function (...) stop(...)
    )

    lg$set_appenders(list(db =
      ctor$new(
        conn = conn,
        table = "logging_test_buffer",
        close_on_exit = FALSE,
        buffer_size = 10
      ))
    )

    replicate(10, lg$info("buffered_insert", foo = "baz", blubb = "blah"))

    expect_length(lg$appenders$db$buffer_events, 10)
    expect_true(
      is.null(lg$appenders$db$data) ||
      identical(nrow(lg$appenders$db$data), 0L)
    )

    lg$info("test")
    expect_length(lg$appenders$db$buffer_events, 0)
    expect_identical(nrow(lg$appenders$db$data), 11L)

    # cleanup
    expect_true(
      x <- tryCatch({
        r <- DBI::dbRemoveTable(conn, lg$appenders$db$layout$format_table_name("LOGGING_TEST_BUFFER"))
        if (!length(r)) TRUE else r # for RJDBC
      },
        error = function(e) FALSE # for RJDBC
      )
    )
  })


  test_that(paste0(nm, ": SQL is sanitzed"), {
    msg <- ";*/;   \"' /* blubb;"
    e <- LogEvent$new(
      lgr, level = 600L, msg = msg, caller = "nope()", timestamp = Sys.time()
    )
    app$append(e)
    res <- app$data$msg
    expect_identical(res[length(res)], msg)
  })


  test_that(paste0(nm, ": cleanup behaves as expected"), {
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



# +- teardown looped tests ---------------------------------------------------
unlink(tsqlite)



# SQLite extra tests ------------------------------------------------------


context("AppenderDbi / SQLite: Extra Tests")

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
  e <- LogEvent$new(lgr, level = 600, msg = "ohno", caller = "nope()", timestamp = Sys.time())

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
      appenders = list(db = AppenderDbi$new(
        conn = conn,
        table = tname,
        close_on_exit = FALSE,
        buffer_size = 0
      )),
      propagate = FALSE
    ),
    "manual"
  )

  lg$fatal("blubb")
  lg$trace("blah")

  expect_output(lg$appenders$db$show(), "FATAL.*TRACE")
  expect_output(
    expect_identical(nrow(lg$appenders$db$show(n = 1)), 1L),
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
