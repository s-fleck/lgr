logger_tree <- function(
  x
){
  names <- ls(envir = loggers)

  nodes <- lapply(names, function(.x) unlist(strsplit(.x, "/")))
  max_depth <- max(seq_len(vapply(nodes, length, integer(1))))

  res <- list()

  for (node in nodes){
    for (i in seq_along(node)){
      ancestry <- node[1:i]
      nm <- node[[i]]
      res[[nm]] <- list()
    }

  }

  res
}
