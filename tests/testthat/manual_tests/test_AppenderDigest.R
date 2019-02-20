# AppenderPushbullet ----------------------------------------------------

email <- whoami::email_address()

test_that("AppenderPushbullet: ERROR log level triggers push", {
  skip_if_not_installed("RPushbullet")

  tryCatch(
    RPushbullet::pbGetUser(),
    error = function(e) skip(as.character(e))
  )

  l <- Logger$new(
    "dummy",
    threshold = NA_integer_,
    appenders = list(
      pb = AppenderPushbullet$new(
        buffer_size = 3
      )
    ),
    propagate = FALSE
  )

  l$info("1")
  l$info("2")
  l$trace("3")
  l$debug("4")
  l$debug("something something 5ish has occurred")

  l$fatal("pushbullet wörks ÄÖÜßÄ", foo = "bar")
})



# AppenderSendmail --------------------------------------------------------

test_that("AppenderSendmail: ERROR log level triggers push", {

  skip_if_not_installed("sendmailR")
  smtp  <- getOption("stat.smtp_server")


  if (is.null(smtp))  skip("Please set the 'stat.smtp_server' option")
  if (is.null(email)) skip("Cannot determine email addresse")

  l <- Logger$new(
    "dummy",
    threshold = NA_integer_,
    appenders = AppenderSendmail$new(
      to = email,
      control = list(smtpServer = smtp, verboseShow = TRUE),
      buffer_size = 3,
      html = FALSE),
    propagate = FALSE
  )

  l$info("1")
  l$info("2")
  l$trace("3")
  l$debug("4")
  l$debug("something something 5ish has occurred ÄÖÜ")

  l$fatal("sendmailR wörks", foo = "bar")
})



# AppenderGmail --------------------------------------------------------

test_that("AppenderGmail: ERROR log level triggers push", {
  skip_if_not_installed("gmailr")

  l <- Logger$new(
    "dummy",
    threshold = NA_integer_,
    appenders = AppenderGmail$new(
      to = email,
      html = TRUE,
      buffer_size = 3),
    propagate = FALSE
  )

  l$info("1")
  l$info("2")
  l$trace("3")
  l$debug("4")
  l$debug("ÄÖÜßÄ")

  l$fatal("gmailr wörks ÄÖÜßÄ", foo = "bar")
})
