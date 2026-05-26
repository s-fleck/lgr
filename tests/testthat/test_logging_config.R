test_that("root logger can apply multi-logger configs", {
  old <- get_logger("old/logger")
  old$set_threshold("fatal")

  cfg <- list(
    version = 1,
    disable_existing_loggers = TRUE,
    layouts = list(
      plain = list(class = "LayoutFormat", fmt = "%L %m")
    ),
    appenders = list(
      console_info = list(
        class = "AppenderConsole",
        threshold = "info",
        layout = "plain"
      )
    ),
    filters = list(
      force_warn = list(class = "FilterForceLevel", level = "warn")
    ),
    loggers = list(
      root = list(threshold = "info", appenders = "console_info"),
      "pkg/sub" = list(propagate = FALSE, filters = "force_warn")
    )
  )

  lgr$config(cfg)

  expect_identical(lgr$threshold, 400L)
  expect_length(lgr$appenders, 1)
  expect_true(inherits(lgr$appenders[[1]], "AppenderConsole"))

  sub <- get_logger("pkg/sub")
  expect_false(sub$propagate)
  expect_length(sub$filters, 1)
  expect_true(inherits(sub$filters[[1]], "FilterForceLevel"))

  expect_true(is_virgin_Logger("old/logger"))

  lgr$config(NULL)
  get_logger("pkg/sub")$config(NULL)
  old$config(NULL)
})


test_that("multi-logger YAML config text works", {
  skip_if_not_installed("yaml")

  cfg <- "
version: 1
appenders:
  console:
    class: AppenderConsole
    threshold: info
loggers:
  root:
    threshold: info
    appenders: [console]
  app/component:
    propagate: false
"

  lgr$config(cfg)

  expect_identical(lgr$threshold, 400L)
  expect_length(lgr$appenders, 1)
  expect_false(get_logger("app/component")$propagate)

  lgr$config(NULL)
  get_logger("app/component")$config(NULL)
})


test_that("legacy logger config behavior remains unchanged", {
  cfg <- list(
    threshold = "error",
    appenders = list(
      AppenderConsole = list(threshold = "info")
    ),
    propagate = FALSE
  )

  lg <- get_logger("legacy")
  lg$config(cfg)

  expect_identical(lg$threshold, 200L)
  expect_identical(lg$appenders[[1]]$threshold, 400L)
  expect_false(lg$propagate)

  lg$config(NULL)
})


test_that("resolve_r6_ctors supports namespaced constructors", {
  skip_if_not_installed("lgrExtra")

  ns <- asNamespace("lgrExtra")
  objs <- mget(
    ls(ns, all.names = TRUE),
    envir = ns,
    ifnotfound = list(NULL),
    inherits = FALSE
  )
  r6_names <- names(objs)[vapply(objs, R6::is.R6Class, logical(1))]
  skip_if(length(r6_names) == 0L)

  cls <- get0_R6Class(paste0("lgrExtra::", r6_names[[1]]))
  expect_true(R6::is.R6Class(cls))
})
