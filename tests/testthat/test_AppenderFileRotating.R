context("AppenderFileRotating")

setup({
  td <- file.path(tempdir(), "lgr")
  assign("td", td, parent.env(environment()))
  dir.create(td, recursive = TRUE)
})


teardown({
  unlink(td, recursive = TRUE)
})

# AppenderFileRotating -----------------------------------------------------

test_that("AppenderFileRotating: works as expected", {
  skip_if_not_installed("rotor", "0.3.0")

  if (!is_zipcmd_available()){
    skip("Test requires a workings system zip command")
  }

  tf <- file.path(td, "test.log")
  app <- AppenderFileRotating$new(file = tf, size = "1tb")

  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(app)

  on.exit({
    lg$config(NULL)
    file.remove(tf)
    app$prune(0)
  })

  # appender logs to file
  expect_identical(app, lg$appenders[[1]])
  lg$fatal("test973")
  expect_true(file.exists(app$file))
  expect_length(readLines(app$file), 1)
  expect_match(readLines(app$file), "test973")

  # first rotate rotates a file with content
  app$rotate(force = TRUE)
  expect_gt(lg$appenders[[1]]$backups[1, ]$size, 0L)
  expect_identical(nrow(lg$appenders[[1]]$backups), 1L)
  expect_length(readLines(app$file), 0)
  expect_match(readLines(app$backups$path[[1]]), "test973")

  # second rotate only has a file of size 0 to rotate
  app$rotate(force = TRUE)
  expect_equal(app$backups[1, ]$size, 0)
  expect_identical(nrow(lg$appenders[[1]]$backups), 2L)

  # compression works
  lg$fatal("test987")
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate(force = TRUE)
  expect_identical(lg$appenders[[1]]$backups$ext, c("log.zip", "log", "log"))
  expect_identical(lg$appenders[[1]]$backups$sfx, as.character(1:3))
  con <- unz(app$backups$path[[1]], filename = "test.log")
  on.exit(close(con), add = TRUE)
  expect_match(readLines(con), "test987")

  # pruning works
  expect_identical(nrow(app$prune(0)$backups), 0L)
})



test_that("AppenderFileRotating: works with different backup_dir", {
  if (!is_zipcmd_available()) {
    skip("Test requires a workings system zip command")
  }

  skip_if_not_installed("rotor", "0.3.0")

  tf     <- file.path(td, "test.log")
  bu_dir <- file.path(td, "backups")

  # backup_dir does not exist
  expect_error(
    app <- AppenderFileRotating$new(file = tf, backup_dir = bu_dir),
    class = "DirDoesNotExistError"
  )

  # rotating to different dir works
  dir.create(bu_dir)
  app <- AppenderFileRotating$new(
    file = tf,
    backup_dir = bu_dir,
    size = 10
  )
  lg <- get_logger("test")$
    set_propagate(FALSE)$
    add_appender(app)

  on.exit({
    app$prune(0)
    lg$config(NULL)
    unlink(c(tf, bu_dir), recursive = TRUE)
  })


  # triggers rotation because resulting file will is bigger than 100 byte
  lg$info(paste(LETTERS, collapse = "-"))
  app$set_compression(TRUE)
  lg$info(paste(letters, collapse = "-"))

  # paths are as expected
  expect_equal(file.size(tf), 0)
  expect_equal(list.files(bu_dir), basename(app$backups$path))
  expect_setequal(app$backups$ext, c("log.zip", "log"))
  expect_match(app$backups$path[[1]], "lgr/backups")
  expect_match(app$backups$path[[2]], "lgr/backups")

  # file contents are as expected
  con <- unz(app$backups$path[[1]], filename = "test.log")
  on.exit(close(con), add = TRUE)
  expect_match(readLines(con), "a-b-.*-y-z")
  expect_match(readLines(app$backups$path[[2]]), "A-B-.*-Y-Z")


  file.remove(app$backups$path)
})




test_that("AppenderFileRotating: `size` argument works as expected", {

  skip_if_not_installed("rotor", "0.3.0")

  #setup
    tf <- file.path(td, "test.log")
    app <- AppenderFileRotating$new(file = tf)$set_size(-1)
    saveRDS(iris, app$file)
    on.exit({
      unlink(tf)
      app$prune(0)
    })

  # logic
    app$set_size("3 KiB")
    app$rotate()
    expect_identical(nrow(app$backups), 0L)

    app$set_size("0.5 KiB")
    app$rotate()
    expect_identical(nrow(app$backups), 1L)
    expect_gt(app$backups$size[[1]], 10)
    expect_equal(file.size(app$file), 0)
})



# AppenderFileRotatingDate ----------------------------------------------------

test_that("AppenderFileRotatingDate: works as expected", {
  if (!is_zipcmd_available())
    skip("Test requires a workings system zip command")

  skip_if_not_installed("rotor", "0.3.0")

  tf <- file.path(td, "test.log")
  app <- AppenderFileRotatingDate$new(file = tf, size = "1tb")
  lg <-
    lgr::get_logger("test")$
    set_propagate(FALSE)$
    set_appenders(app)

  on.exit({
    lg$config(NULL)
    file.remove(tf)
    app$prune(0)
  })

  # appender logs to file
  expect_identical(app, lg$appenders[[1]])
  lg$fatal("test341")
  expect_true(file.exists(app$file))
  expect_length(readLines(app$file), 1)
  expect_match(readLines(app$file), "test341")
  expect_identical(nrow(app$backups), 0L)

  # first rotate rotates a file with content
  app$set_size(-1)$set_age("1 day")
  app$rotate(now = as.Date("2019-01-01"), force = TRUE)
  expect_identical(nrow(app$backups), 1L)
  expect_gt(lg$appenders[[1]]$backups[1, ]$size, 0L)
  expect_length(readLines(app$file), 0)
  expect_match(app$backups$path[[1]], "2019-01-01")
  expect_match(readLines(app$backups$path[[1]]), "test341")

  # second rotate only has a file of size 0 to rotate
  app$rotate(now = "2019-01-02")
  expect_identical(nrow(app$backups), 2L)
  expect_equal(app$backups[1, ]$size, 0)
  expect_match(app$backups$path[[1]], "2019-01-02")

  # compression works
  app$set_age("10000 years")  # prevent rotation on log
  lg$fatal("test938")
  app$set_age("1 day")
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate(now = as.Date("2019-01-03"))
  expect_identical(nrow(app$backups), 3L)
  expect_identical(app$backups$ext, c("log.zip", "log", "log"))
  expect_identical(lg$appenders[[1]]$backups$sfx, c("2019-01-03", "2019-01-02", "2019-01-01"))
  con <- unz(app$backups$path[[1]], filename = "test.log")
  on.exit(close(con), add = TRUE)
  expect_match(readLines(con), "test938")

  # pruning works
  expect_identical(nrow(app$prune(0)$backups), 0L)
})




test_that("AppenderFileRotatingDate: works with different backup_dir", {
  skip_if_not(is_zipcmd_available(), "Test requires a workings system zip command")
  skip_if_not_installed("rotor", "0.3.0")

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
  app$set_age(-1)$set_size(-1)
  lg$info(paste(LETTERS))
  app$set_compression(TRUE)
  lg$info(paste(LETTERS))

  expect_equal(file.size(tf), 0)

  expect_equal(list.files(bu_dir), basename(app$backups$path))
  expect_setequal(app$backups$ext, c("log.zip", "log"))
  file.remove(app$backups$path)
})




test_that("AppenderFileRotatingDate: `size` and `age` arguments work as expected", {
  skip_if_not_installed("rotor", "0.3.0")

  #setup
  tf <- file.path(td, "test.log")
  app <- AppenderFileRotatingDate$new(file = tf)$set_age(-1)
  saveRDS(iris, app$file)
  on.exit({
    unlink(tf)
    app$prune(0)
  })

  # file size to small
  app$set_size("3 KiB")
  app$rotate()
  expect_identical(nrow(app$backups), 0L)

  app$set_size("0.5 KiB")
  app$rotate(now = "2999-01-01")
  expect_identical(nrow(app$backups), 1L)

  # age to small
  app$set_size(-1)
  app$set_age("1 day")
  app$rotate(now = "2999-01-01")
  expect_identical(nrow(app$backups), 1L)

  app$rotate(now = "2999-01-02")
  expect_identical(nrow(app$backups), 2L)
})




# AppenderFileRotatingTime ----------------------------------------------------

test_that("AppenderFileRotatingTime: works as expected", {
  skip_if_not(is_zipcmd_available(), "Test requires a workings system zip command")
  skip_if_not_installed("rotor", "0.3.0")

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
  app$set_age(-1)
  app$rotate(now = "2999-01-03--12-01")
  expect_gt(app$backups[1, ]$size, 0)
  expect_match(app$backups[1, ]$path, "2999-01-03--12-01-00")

  # second rotate only has a file of size 0 to rotate
  lg$appenders[[1]]$rotate(now = "2999-01-03--12-02")
  expect_equal(lg$appenders[[1]]$backups[1, ]$size, 0)
  expect_match(app$backups[1, ]$path, "2999-01-03--12-02-00")

  # compression is possible
  lg$appenders[[1]]$set_compression(TRUE)
  lg$appenders[[1]]$rotate(now = "2999-01-03--12-03")
  expect_identical(lg$appenders[[1]]$backups$ext, c("log.zip", "log", "log"))
  expect_match(app$backups[1, ]$path, "2999-01-03--12-03-00.log.zip")

  # cleanup
  app$prune(0)
  expect_identical(nrow(app$backups), 0L)
  lg$config(NULL)
})




test_that("AppenderFileRotatingTime: works with different backup_dir", {
  skip_if_not(is_zipcmd_available(), "Test requires a workings system zip command")
  skip_if_not_installed("rotor", "0.3.0")

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
    size = 100,
    age = -1
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
  Sys.sleep(1)  # for sort order of backups
  lg$info(paste(LETTERS))

  expect_equal(file.size(tf), 0)

  expect_equal(rev(list.files(bu_dir)), basename(app$backups$path))
  expect_equal(app$backups$ext, c("log.zip", "log"))
  file.remove(app$backups$path)
})




test_that("AppenderFileRotatingTime: `size` and `age` arguments work as expected", {

  skip_if_not_installed("rotor", "0.3.0")

  #setup
  tf <- file.path(td, "test.log")
  app <- AppenderFileRotatingTime$new(file = tf)$set_age(-1)
  saveRDS(iris, app$file)
  on.exit({
    unlink(tf)
    app$prune(0)
  })

  # file size to small
  app$set_size(file.size(tf) + 2)
  app$rotate()
  expect_identical(nrow(app$backups), 0L)

  app$set_size(floor(file.size(tf) / 2))
  app$rotate(now = "2999-01-01")
  expect_identical(nrow(app$backups), 1L)

  # age to small
  app$set_size(-1)
  app$set_age("1 day")
  app$rotate(now = "2999-01-01")
  expect_identical(nrow(app$backups), 1L)

  app$rotate(now = "2999-01-02")
  expect_identical(nrow(app$backups), 2L)
})


# Issues ------------------------------------------------------------------

test_that("AppenderFileRotatingTime: `size` and `age` arguments work as expected #39", {
  skip_if_not_installed("rotor", "0.3.0")

  #setup
  tf <- file.path(td, "test.log")
  log_dir <- file.path(td, "backups")
  dir.create(log_dir)
  app <- AppenderFileRotatingTime$new(
    file = tf,
    layout = LayoutJson$new(),
    age = -1,
    size = "0.5 kb",
    max_backups = 5,
    backup_dir = log_dir,
    overwrite = FALSE,
    compression = FALSE,
    threshold = "info"
  )

  on.exit({
    unlink(tf)
    unlink(log_dir, recursive = TRUE)
    app$prune(0)
  })

  lg <- get_logger("test_issue_39")$
    set_propagate(FALSE)$
    set_appenders(list(rotating = app))

  # push log messages until rotation is triggered, should only take a few iterations
  for (i in 1:100){
    lg$info("test")
    if (nrow(lg$appenders$rotating$backups) >= 1)
      break
  }

  expect_identical(nrow(lg$appenders$rotating$backups), 1L)
  expect_length(list.files(log_dir), 1)
})

