ml <- memlog$new()


sink("/dev/null")

for (i in 1:1e5) ml$fatal("blubb")
