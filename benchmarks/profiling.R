library(memlog)
library(futile.logger)

# Benchmarking different appenders ----------------------------------------

times <- list()

ml_min <- memlog$new(appenders = console_appender_minimal)
ml_std <- memlog$new(appenders = appender_console$new())
ml_col <- memlog$new(appenders = console_appender_color)

ml_min$fatal("blubb")
ml_std$fatal("blubb")
ml_col$fatal("blubb")


sink("/dev/null")

times[[1]] <- bench::system_time({for (i in 1:1e4) flog.info("blubb")})         # 15.20 s
times[[2]] <- bench::system_time({for (i in 1:1e4) ml_min$fatal("blubb")})      #  4.17 s
times[[3]] <- bench::system_time({for (i in 1:1e4) ml_std$fatal("blubb")})      #  7.28 s
times[[4]] <- bench::system_time({for (i in 1:1e4) ml_col$fatal("blubb")})      #  12.9 s


sink()


times





formatting
ml$showdt()
ml$show()




ml$fatal("blubb")
ml$error("blubb")
ml$warn("blubb")
ml$info("blubb")
ml$debug("blubb")
ml$trace("blubb")


ml <- memlog$new()
ml$set_threshold("blubb")


ml$.__enclos_env__$private$appenders[[1]]$threshold <- "blubb"

ml$debug("blubb")

browser()

cat(format.memlog_data(
  x,
  ml = ml,
  colors = list(
    "fatal" = function(x) colt::clt_error(colt::clt_emph2(x)),
    "error" = colt::clt_error,
    "warn" = colt::clt_warning,
    "debug" = colt::clt_chr,
    "trace" = colt::clt_chr_subtle
  )
))
