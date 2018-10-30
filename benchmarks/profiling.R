library(memlog)
library(futile.logger)

# Benchmarking different appenders ----------------------------------------

times <- list()


ml_nul <- Memlog$new(appenders = NULL)
ml_min <- Memlog$new(appenders = AppenderConsoleMinimal$new())
ml_std <- Memlog$new(appenders = AppenderConsole$new())
ml_col <- Memlog$new(appenders = AppenderConsole$new(
  colors = list(
    "fatal" = function(x) colt::clt_error(colt::clt_emph2(x)),
    "error" = colt::clt_error,
    "warn"  = colt::clt_warning,
    "debug" = colt::clt_chr,
    "trace" = colt::clt_chr_subtle
  )
))

ml_nul$fatal("blubb")
ml_min$fatal("blubb")
ml_std$fatal("blubb")
ml_col$fatal("blubb")


ml_min <- Memlog$new(appenders = AppenderConsoleMinimalColor$new())
ml_min
ml_min$fatal("blubb")
ml_min$info("blubb")

for (i in 1:100){
  l <- sample(c("fatal", "error", "warn", "info", "debug", "trace"), 1, prob = 1:6)
  msg <- paste(i, sample(month.name, 5), sample(colors(), 5), collapse = " ", sep = " ")
  ml_col[[l]](msg)
}

ml_col$show(100)
ml_col$showdt()


stop()

sink("/dev/null")

# Compare different appenders against futile.logger

#times[["flog"]] <- bench::system_time({for (i in 1:1e4) flog.info("blubb")})        # 15.20 s
times[["nul"]] <- bench::system_time({for (i in 1:1e4) ml_nul$fatal("blubb")})       #  2.84 s
times[["min"]] <- bench::system_time({for (i in 1:1e4) ml_min$fatal("blubb")})       #  4.04 s
times[["std"]] <- bench::system_time({for (i in 1:1e4) ml_std$fatal("blubb")})       #  6.29 s
times[["col"]] <- bench::system_time({for (i in 1:1e4) ml_col$fatal("blubb")})       #  11.8 s

ml_col$suspend()
times[["suspended"]] <- bench::system_time({for (i in 1:1e4) ml_col$fatal("blubb")})       #  26 ms
ml_col$unsuspend()

times[["forloop"]] <- bench::system_time({for (i in 1:1e4) NULL})       #  1.5 ms

sink()




ml$fatal("blubb")
ml$error("blubb")
ml$warn("blubb")
ml$info("blubb")
ml$debug("blubb")
ml$trace("blubb")


ml <- Memlog$new()
ml$set_threshold("blubb")


ml$.__enclos_env__$private$appenders[[1]]$threshold <- "blubb"

ml$debug("blubb")


