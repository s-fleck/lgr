library(memlog)


ml <- memlog$new()
sink("/dev/null")
for (i in 1:1e5) ml$fatal("blubb")
sink()

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
