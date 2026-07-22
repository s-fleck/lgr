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


test_that("re-applying a declarative config resets fields omitted from a mentioned logger", {
  sub <- get_logger("pkg/resettable")

  lgr$config(
    list(
      version = 1,
      loggers = list(
        root = list(threshold = "warn"),
        "pkg/resettable" = list(
          threshold = "debug",
          appenders = list(class = "AppenderConsole"),
          propagate = FALSE,
          exception_handler = list(code = "function(e) NULL")
        )
      )
    ),
    allow_code = TRUE
  )

  expect_identical(lgr$threshold, 300L)
  expect_identical(sub$threshold, 500L)
  expect_length(sub$appenders, 1)
  expect_false(sub$propagate)
  expect_false(identical(sub$exception_handler, default_exception_handler))

  # a second config that mentions both loggers but omits every other field
  # must reset them to their defaults, not preserve the previous state
  lgr$config(list(
    version = 1,
    loggers = list(
      root = list(),
      "pkg/resettable" = list()
    )
  ))

  expect_identical(lgr$threshold, 400L)
  expect_true(is_threshold_inherited(sub))
  expect_length(sub$appenders, 0)
  expect_true(sub$propagate)
  expect_identical(sub$exception_handler, default_exception_handler)

  lgr$config(NULL)
  sub$config(NULL)
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


test_that("programmatic single-logger list config accepts constructed objects", {
  cfg <- list(
    threshold = "error",
    appenders = list(AppenderConsole$new(threshold = "info")),
    propagate = FALSE
  )

  lg <- get_logger("legacy")
  lg$config(cfg)

  expect_identical(lg$threshold, 200L)
  expect_identical(lg$appenders[[1]]$threshold, 400L)
  expect_false(lg$propagate)

  lg$config(NULL)
})


test_that("programmatic single-logger list config rejects unknown keys", {
  lg <- get_logger("legacy")
  expect_error(
    lg$config(list(thresholdd = "error")),
    "Unknown element"
  )
  lg$config(NULL)
})


test_that("get0_R6Class supports namespaced constructors", {
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
