library(yog)


  ml_col <- Logger$new(
    appenders = c(
      AppenderFile$new(file = tempfile()),
      AppenderConsole$new(),
      AppenderFile$new(threshold = 100, file = paste0(tempfile(), tempfile(), tempfile())),
      AppenderMemoryDt$new()
    )
  )

  ml_col$info("blub")
  ml_col$info("wub")
  ml_col$info("wub")


  ml_col$appenders[[1]]
  ml_col$appenders[[2]]
  ml_col$appenders[[3]]
  ml_col$appenders[[4]]

  ml_col


  list(yog, ml_col)

  ml_col$appenders
