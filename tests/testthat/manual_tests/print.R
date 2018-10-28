library(memlog)


  ml_col <- Memlog$new(
    appenders = c(
      appender_console_color,
      AppenderConsoleGlue$new(threshold = 3),
      AppenderConsoleMinimal$new(),
      AppenderFile$new(file = tempfile()),
      AppenderFile$new(threshold = 1, file = paste0(tempfile(), tempfile(), tempfile()))
    )
  )

  ml_col$info("blub")
  ml_col$info("wub")
  ml_col$info("wub")

  print(ml_col)

  app <- AppenderConsoleGlue$new()
  print(app, single_line_summary = TRUE)


  collector_default
