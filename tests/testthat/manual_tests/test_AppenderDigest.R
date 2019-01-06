# AppenderPushbullet ----------------------------------------------------

email <- ""

test_that("AppenderPushbullet: ERROR log level triggers push", {
  if (requireNamespace("RPushbullet")){
    tryCatch(
      RPushbullet::pbGetUser(),
      error = function(e) skip(e)
    )
  } else {
    skip("Package RPushbullet not available")
  }

  l <- Logger$new(
    "dummy",
    threshold = NA_integer_,
    appenders = list(
      pb = AppenderPushbullet$new(
        buffer_size = 3
      )
    ),
    parent = NULL
  )

  l$info("1")
  l$info("2")
  l$trace("3")
  l$debug("4")
  l$debug("something something 5ish has occured")

  l$fatal("Oh well, here we go", foo = "bar")
})



# AppenderSendmail --------------------------------------------------------

test_that("AppenderSendmail: ERROR log level triggers push", {

  if (requireNamespace("sendmailR"))

    l <- Logger$new(
      "dummy",
      threshold = NA_integer_,
      appenders = AppenderSendmail$new(
        to = email,
        control = list(smtpServer = "localhost", verboseShow = TRUE),
        buffer_size = 3),
      parent = NULL
    )

  l$info("1")
  l$info("2")
  l$trace("3")
  l$debug("4")
  l$debug("something something 5ish has occured")

  l$fatal("sendmailR works", foo = "bar")
})



# AppenderGmail --------------------------------------------------------

test_that("AppenderGmail: ERROR log level triggers push", {
  if (requireNamespace("gmailr"))

    l <- Logger$new(
      "dummy",
      threshold = NA_integer_,
      appenders = AppenderGmail$new(
        to = email,
        html = TRUE,
        buffer_size = 3),
      parent = NULL
    )

  l$info("1")
  l$info("2")
  l$trace("3")
  l$debug("4")
  l$debug("something something 5ish has occured")

  l$fatal("gmailr works", foo = "bar")
})
