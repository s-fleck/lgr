context("AppenderFileRotating")

td <- file.path(tempdir(), "lgr")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
})



# AppenderFileRotating -----------------------------------------------------

test_that("AppenderFileRotating works as expected", {
  tf <- file.path(td, "test.log")
  app <- AppenderFileRotating$new(file = tf, size = "1tb")

  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(app)

  on.exit({
    lg$config(NULL)
    file.remove(tf)
  })

  expect_identical(app, lg$appenders[[1]])
  lg$fatal("test")
  expect_true(file.exists(lg$appenders[[1]]$file))

  # first rotate roates a file with content
  app$rotate(force = TRUE)
  expect_gt(lg$appenders[[1]]$backups[1, ]$size, 0L)
  expect_identical(nrow(lg$appenders[[1]]$backups), 1L)

  # second rotate only has a file of size 0 to rotate
  lg$appenders[[1]]$rotate(force = TRUE)
  expect_equal(lg$appenders[[1]]$backups[1, ]$size, 0)
  expect_identical(nrow(lg$appenders[[1]]$backups), 2L)

  # compression is possible
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate(force = TRUE)
  expect_identical(lg$appenders[[1]]$backups$ext, c("log.zip", "log", "log"))
  expect_identical(lg$appenders[[1]]$backups$sfx, as.character(1:3))

  # cleanup
  lg$appenders[[1]]$prune(0)
  expect_identical(nrow(lg$appenders[[1]]$backups), 0L)
  lg$config(NULL)
})



test_that("AppenderFileRotating works with different backup_dir", {
  tf     <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")

  # backup_dir does not exist
  expect_error(
    app <- AppenderFileRotating$new(file = tf, backup_dir = bu_dir)
  )

  # rotating to different dir works
  dir.create(bu_dir)
  app <- AppenderFileRotating$new(
    file = tf,
    backup_dir = bu_dir,
    size = 100
  )
  lg <- get_logger("test")$
    set_propagate(FALSE)$
    add_appender(app)

  on.exit({
    app$prune(0)
    lg$config(NULL)
    unlink(c(tf, bu_dir), recursive = TRUE)
  })

  lg$info(paste(LETTERS))
  app$set_compression(TRUE)
  lg$info(paste(LETTERS))

  expect_equal(file.size(tf), 0)

  expect_equal(list.files(bu_dir), basename(app$backups$path))
  expect_setequal(app$backups$ext, c("log.zip", "log"))
  file.remove(app$backups$path)
})




test_that("AppenderFileRotating works as expected", {
  #setup
    tf <- file.path(td, "test.log")
    app <- AppenderFileRotating$new(file = tf)$set_size(-1)
    saveRDS(iris, app$file)
    on.exit(file.remove(tf))

  # logic
    app$set_size("3 KiB")
    app$rotate()
    expect_identical(nrow(app$backups), 0L)

    app$set_size("0.5 KiB")
    app$rotate()
    expect_identical(nrow(app$backups), 1L)
    expect_gt(app$backups$size[[1]], 10)
    expect_equal(file.size(app$file), 0)

    app$prune(0)
    app$set_size(0)

  # cleanup
    app$prune(0)
})



# AppenderFileRotatingDate ----------------------------------------------------

test_that("AppenderFileRotatingDate works as expected", {
  tf <- file.path(td, "test.log")

  app <- AppenderFileRotatingDate$new(file = tf)
  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(app)

  on.exit({
    app$prune(0)
    lg$config(NULL)
    unlink(tf)
  })

  lg$fatal("test")

  # first rotate roates a file with content
  app$set_size(-1)
  app$rotate(now = as.Date("2019-01-01"))
  app$.__enclos_env__$private$bq$backups


  expect_gt(app$backups[1, ]$size, 0)

  # second rotate only has a file of size 0 to rotate
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-02"))
  expect_equal(lg$appenders[[1]]$backups[1, ]$size, 0)

  # compression is possible
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-03"))
  expect_identical(lg$appenders[[1]]$backups$ext, c("log.zip", "log", "log"))
})




test_that("AppenderFileRotatingDate works with different backup_dir", {
  tf     <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")

  # backup_dir does not exist
  expect_error(
    app <- AppenderFileRotatingDate$new(file = tf, backup_dir = bu_dir)
  )

  # setup
  dir.create(bu_dir)
  app <- AppenderFileRotatingDate$new(
    file = tf,
    backup_dir = bu_dir,
    size = 100
  )
  lg <- get_logger("test")$set_propagate(FALSE)
  lg$add_appender(app)

  on.exit({
    app$prune(0)
    unlink(c(tf, bu_dir), recursive = TRUE)
    lg$config(NULL)
  })


  # rotating to different dir works
  lg$info(paste(LETTERS))
  app$set_compression(TRUE)
  lg$info(paste(LETTERS))

  expect_equal(file.size(tf), 0)

  expect_equal(list.files(bu_dir), basename(app$backups$path))
  expect_setequal(app$backups$ext, c("log.zip", "log"))
  file.remove(app$backups$path)
})



# AppenderFileRotatingTime ----------------------------------------------------

test_that("AppenderFileRotatingTime works as expected", {
  tf <- file.path(td, "test.log")
  app <- AppenderFileRotatingTime$new(file = tf)
  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(app)

  on.exit({
    app$prune(0)
    lg$config(NULL)
    unlink(tf)
  })

  lg$fatal("test")

  # first rotate roates a file with content
  app$set_size(-1)
  lg$appenders[[1]]$rotate(now = "2019-01-03--12-01")
  expect_gt(lg$appenders[[1]]$backups[1, ]$size, 0)
  expect_match(app$backups[1, ]$path, "2019-01-03--12-01-00")

  # second rotate only has a file of size 0 to rotate
  lg$appenders[[1]]$rotate(now = "2019-01-03--12-02")
  expect_equal(lg$appenders[[1]]$backups[1, ]$size, 0)
  expect_match(app$backups[1, ]$path, "2019-01-03--12-02-00")

  # compression is possible
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate(now = "2019-01-03--12-03")
  expect_identical(lg$appenders[[1]]$backups$ext, c("log.zip", "log", "log"))
  expect_match(app$backups[1, ]$path, "2019-01-03--12-03-00.log.zip")

  # cleanup
  lg$appenders[[1]]$prune(0)
  expect_identical(nrow(lg$appenders[[1]]$backups), 0L)
  lg$config(NULL)
})



test_that("AppenderFileRotatingTime works with different backup_dir", {
  tf     <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")

  # backup_dir does not exist
  expect_error(
    app <- AppenderFileRotatingTime$new(file = tf, backup_dir = bu_dir)
  )

  # setup
  dir.create(bu_dir)
  app <- AppenderFileRotatingTime$new(
    file = tf,
    backup_dir = bu_dir,
    size = 100
  )
  lg <- get_logger("test")$
    set_propagate(FALSE)$
    add_appender(app)

  on.exit({
    app$prune(0)
    unlink(c(tf, bu_dir), recursive = TRUE)
    lg$config(NULL)
  })


  # rotating to different dir works
  lg$info(paste(LETTERS))
  app$set_compression(TRUE)
  lg$info(paste(LETTERS))

  expect_equal(file.size(tf), 0)

  expect_equal(list.files(bu_dir), basename(app$backups$path))
  expect_setequal(app$backups$ext, c("log.zip", "log"))
  file.remove(app$backups$path)
})
