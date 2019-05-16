context("AppenderFileRotating")

dr <- tempdir()
td <- file.path(dr, "lgr")
dir.create(td, recursive = TRUE)

teardown({
  unlink(td, recursive = TRUE)
  if (!length(list.files(dr))) unlink(dr, recursive = TRUE)
})



# AppenderFileRotating -----------------------------------------------------

test_that("AppenderFileRotating works as expected", {
  tf <- file.path(td, "test.log")
  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(AppenderFileRotating$new(file = tf)$set_size(-1))

  lg$fatal("test")

  # first rotate roates a file with content
  lg$appenders[[1]]$rotate()
  expect_gt(lg$appenders[[1]]$backups[1, ]$size, 0L)
  expect_identical(nrow(lg$appenders[[1]]$backups), 1L)

  # second rotate only has a file of size 0 to rotate
  lg$appenders[[1]]$rotate()
  expect_equal(lg$appenders[[1]]$backups[1, ]$size, 0)
  expect_identical(nrow(lg$appenders[[1]]$backups), 2L)

  # compression is possible
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate()
  expect_identical(lg$appenders[[1]]$backups$ext, c("log.zip", "log", "log"))
  expect_identical(lg$appenders[[1]]$backups$sfx, as.character(1:3))

  # cleanup
  lg$appenders[[1]]$prune(0)
  expect_identical(nrow(lg$appenders[[1]]$backups), 0L)
  lg$config(NULL)
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
  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(AppenderFileRotatingDate$new(file = tf, size = -1))

  lg$fatal("test")

  # first rotate roates a file with content
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-01"))
  expect_gt(lg$appenders[[1]]$backups[1, ]$size, 0)

  # second rotate only has a file of size 0 to rotate
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-02"))
  expect_equal(lg$appenders[[1]]$backups[1, ]$size, 0)

  # compression is possible
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-03"))
  expect_identical(lg$appenders[[1]]$backups$ext, c("log.zip", "log", "log"))

  # cleanup
  lg$appenders[[1]]$prune(0)
  expect_identical(nrow(lg$appenders[[1]]$backups), 0L)
  lg$config(NULL)
})




# AppenderFileRotatingTime ----------------------------------------------------

test_that("AppenderFileRotatingTime works as expected", {
  tf <- file.path(td, "test.log")
  app <- AppenderFileRotatingTime$new(file = tf, size = -1)
  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(app)

  lg$fatal("test")

  # first rotate roates a file with content
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
