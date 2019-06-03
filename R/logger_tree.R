logger_tree <- function(
  x
){
  names <- ls(envir = loggers)
  nodes <- lapply(names, function(.x) unlist(strsplit(.x, "/")))

  second_tier <- vapply(nodes, `[[`, FUN.VALUE = character(1), 1)

  res <- data.frame(
    parents = "root",
    children = I(list(unique(second_tier))),
    stringsAsFactors = FALSE
  )


  for (i in seq_along(nodes)){
    for (j in seq_along(nodes[[i]])){
      parent <- nodes[[i]][[j]]

      if (j < length(nodes[[i]])){
        child  <- nodes[[i]][[j + 1]]
      } else {
        child <- NULL
      }

      if (parent %in% res$parent){
        sel <- res$parent == parent
        res$children[sel] <- I(list(unlist(unique(compact(c(res$children[sel], child))))))

      } else {
        res <- rbind(
          res,
          data.frame(parents = parent, children = I(list(child)))
        )
      }
    }
  }

  res
}
