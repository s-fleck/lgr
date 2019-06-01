# Test multiple RDBMS

dt_sp <- options("datatable.showProgress")

setup(options("datatable.showProgress" = FALSE))
teardown({
  options("datatable.showProgress" = dt_sp)
})




# RDBMS batch tests -------------------------------------------------------


# +- setup connections -----------------------------------------------------

tsqlite <- tempfile()
teardown(unlink(tsqlite))

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

  "PostgreSQL via RPostgres" = list(
    conn = try(
      silent = TRUE, {
        assert(packageVersion("RPostgres") > "1.1.1",
          "RPostgres <= 1.1.1 works but tests are disabled due to this bug:",
          "https://github.com/r-dbi/RMariaDB/issues/119"
        )
        DBI::dbConnect(
          RPostgres::Postgres(),
          user = "postgres",
          host = "localhost",
          dbname = "travis_ci_test"
        )
      }
    ),
    ctor = AppenderDbi
  ),

  "DB2 via RJDBC" = list(
    conn = try(silent = TRUE, dataSTAT::dbConnectDB2("RTEST", "rtest", "rtest")),
    ctor = AppenderRjdbc
  ),

  "DB2 via odbc" = list(
    conn = try(silent = TRUE, dataSTAT::dbConnectDB2("RTEST", "rtest", "rtest", type = "odbc")),
    ctor = AppenderDbi,
    layout = LayoutDb2$new(
      col_types = c(
        level = "smallint",
        timestamp = "timestamp",
        logger= "varchar(512)",
        msg = "varchar(1024)",
        caller = "varchar(1024)",
        foo = "varchar(256)"
      )
    )
  ),

  "SQLite via RSQLite" = list(
    conn = DBI::dbConnect(RSQLite::SQLite(), database = tsqlite),
    ctor = AppenderDbi
  )
)

teardown({
  for (db in dbs){
    try(DBI::dbDisconnect(db$conn), silent = TRUE)
  }
})




init_test_appender = function(
  ctor,
  conn,
  table = "logging_test",
  layout = select_dbi_layout(conn, table)$set_col_types(c(
    level = "smallint",
    timestamp = "timestamp",
    logger= "varchar(512)",
    msg = "varchar(1024)",
    caller = "varchar(1024)",
    foo = "varchar(256)"
  ))){
  ap <- ctor$new(
    conn = conn,
    table = table,
    layout = layout,
    close_on_exit = FALSE
  )
}



# for manual testing
nm <- "MySQL via RMySQL"
nm <- "DB2 via RJDBC"
nm <- "DB2 via odbc"
nm <- "MySQL via RMariaDB"
nm <- "SQLite via RSQLite"
nm <-  "PostgreSQL via RPostgres"


for (nm in names(dbs)){

# +- setup test appender --------------------------------------------------
  conn <- dbs[[nm]]$conn
  ctor <- dbs[[nm]]$ctor
  title <- paste(ctor$classname, "/", nm)
  context(title)
  lg <- get_logger("db_test")$set_propagate(FALSE)

  if (is_try_error(conn)){
    test_that(title, trimws(strwrap(skip("Cannot establish connection"))))
    next
  }




  # +- tests -------------------------------------------------------------------
  test_that(paste0(nm, ": create schema.table at initalization via DBI::Id"), {
    if (nm == "SQLite via RSQLite"){
      skip("SQLite doesn't support schemas")
    }

    tab <-  DBI::Id(schema = "TMP", table = "TEST")

    if (inherits(conn, "PqConnection")){
      try(DBI::dbExecute(conn, 'create schema "TMP"'), silent = TRUE)
      on.exit(DBI::dbExecute(conn, 'drop schema "TMP" cascade'))
    }

    ap <- init_test_appender(ctor, conn, tab)
    on.exit(dbRemoveTableCaseInsensitive(conn, tab), add = TRUE)
    lg$set_appenders(list(db = ap))

    expect_identical(nrow(ap$data), 0L)
    lg$fatal("test")
    expect_identical(nrow(ap$data),  1L)
  })




  test_that(paste0(nm, ": create schema.table at initalization via qualified table name"), {
    if (nm == "SQLite via RSQLite"){
      skip("SQLite doesn't support schemas")
    }

    tab <-  "TmP.TeST"

    ap <- init_test_appender(ctor, conn, tab)
    on.exit(dbRemoveTableCaseInsensitive(conn, ap$table))

    if (inherits(conn, "JDBCConnection")){
      expect_identical(
        nrow(DBI::dbGetQuery(conn, paste("select * from", ap$table_name))),
        0L
      )
    } else {
      expect_identical(nrow(DBI::dbReadTable(conn, ap$table_name)),  0L)
    }

    lg$set_appenders(list(db = ap))
    lg$fatal("test")
    expect_identical(nrow(ap$data),  1L)
  })




  test_that(paste0(nm, ": round trip event inserts"), {
    app <- init_test_appender(ctor, conn)
    on.exit(dbRemoveTableCaseInsensitive(conn, app$table))
    lg$set_appenders(list(db = app))

    lg$log(
      200L,
      "test",
      timestamp = as.POSIXct("2019-12-31"),
      caller = "foo()",
      foo = "bar"
    )

    tres <- app$data

    eres <- data.frame(
      level = 200L,
      timestamp = as.POSIXct("2019-12-31"),
      logger = "db_test",
      msg = "test",
      caller = "foo()",
      foo = "bar",
      stringsAsFactors = FALSE
    )

    expect_equal(tres, eres)
  })




  test_that(paste0(nm, ": col order does not impact inserts"), {
    app <- init_test_appender(ctor, conn)
    on.exit(dbRemoveTableCaseInsensitive(conn, app$table))
    lg$set_appenders(list(db = app))

    for (i in 1:20){
      app$layout$set_col_types(sample(app$layout$col_types))
      lg$log(
        200L,
        "test",
        timestamp = as.POSIXct("2019-12-31"),
        caller = "foo()",
        foo = "bar"
      )
    }

    expect_true(all(vapply(app$data$timestamp, all_are_identical, logical(1))))
    expect_setequal_timestamp(app$data$timestamp, as.POSIXct("2019-12-31"))
  })



  # custom fields
  test_that(paste0(nm, ": Creating tables with custom fields works"), {
    if ("layout" %in% names(dbs[[nm]])){
      lo <- dbs[[nm]]$layout

    } else if (ctor$classname == "AppenderRjdbc"){
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

    ap <- init_test_appender(ctor, conn, "LOGGING_TEST_CREATE", layout = lo)
    on.exit(dbRemoveTableCaseInsensitive(conn, app$table))
    lg$set_appenders(list(db = ap))
    on.exit(dbRemoveTableCaseInsensitive(conn, ap$table))

    lg$fatal("test", foo = "bar")
    expect_identical(nrow(ap$data), 1L)
    expect_false(is.na(lg$appenders$db$data$foo[[1]]))
    lg$fatal("test")
    expect_identical(nrow(ap$data), 2L)
    expect_true(is.na(lg$appenders$db$data$foo[[2]]))

    # Log to all fields that are already present in table by default
    lg$fatal("test2", foo = "baz", blubb = "blah")
    expect_identical(tail(ap$data, 1)$foo, "baz")
    expect_false("blubb" %in% names(lg$appenders$db$data))

    lg$remove_appender("db")
  })




  test_that(paste0(nm, ": Buffered inserts work"), {
    ap <- init_test_appender(ctor, conn, "LOGGING_TEST_BUFFER")
    ap$set_buffer_size(10L)
    on.exit(dbRemoveTableCaseInsensitive(conn, ap$table))
    lg$set_appenders(list(db = ap))

    replicate(10, lg$info("buffered_insert", foo = "baz", blubb = "blah"))

    expect_length(lg$appenders$db$buffer_events, 10)
    expect_identical(nrow(ap$data), 0L)

    lg$info("test")
    expect_length(lg$appenders$db$buffer_events, 0)
    expect_identical(nrow(lg$appenders$db$data), 11L)
  })




  test_that(paste0(nm, ": SQL is sanitzed"), {
    app <- init_test_appender(ctor, conn)
    on.exit(dbRemoveTableCaseInsensitive(conn, app$table))

    msg <- ";*/;   \"' /* blubb;"
    e <- LogEvent$new(
      lgr, level = 600L, msg = msg, caller = "nope()", timestamp = Sys.time()
    )
    app$append(e)
    app$flush()
    expect_identical(app$data$msg, msg)
  })
}




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




test_that("AppenderDBI / RSQLite: $show()", {
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




test_that("AppenderDbi / RSQLite: automatic closing of connections works", {
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
