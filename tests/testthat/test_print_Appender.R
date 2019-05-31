context("print_Appender")




test_that("print_Appender works as expected", {
  tf <- tempfile()
  on.exit(unlink(tf))

  Appender$new()
  AppenderConsole$new()
  AppenderDbi$new(RSQLite::dbConnect(RSQLite::SQLite()), "blubb")
  AppenderMemory$new()
  AppenderMail$new()
  AppenderSendmail$new("test@blah.com", control = list())
  AppenderTable$new()
  AppenderBuffer$new(appenders = list(
    AppenderConsole$new(),
    blah = AppenderBuffer$new()
  ))
  AppenderPushbullet$new(recipients = "blubb")
  AppenderFileRotating$new(tf)
  AppenderFileRotating$new(tf, size = "1kb")
  AppenderFileRotatingDate$new(tf, layout = LayoutJson$new())
  AppenderFileRotatingTime$new(tf, layout = LayoutJson$new(), age = "2 years", size = 9000)
  AppenderFile$new(tf)

})
