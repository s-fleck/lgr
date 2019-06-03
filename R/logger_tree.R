logger_tree <- function(
  x
){
  names <- ls(envir = loggers)
  nodes <- lapply(names, function(.x) unlist(strsplit(.x, "/")))
  max_depth <- seq_len(max(vapply(nodes, length, integer(1))))
  second_tier <- setdiff(vapply(nodes, `[[`, character(1), 1L), "root")

  res <- data.frame(
    parent = "root",
    children = I(list(unique(second_tier))),
    stringsAsFactors = FALSE
  )


  for (i in seq_along(nodes)){
    for (j in seq_along(nodes[[i]])){
      parent_cur <- nodes[[i]][[j]]

      if (j < length(nodes[[i]])){
        child_cur  <- nodes[[i]][[j + 1]]
      } else {
        child_cur <- NULL
      }

      if (parent_cur %in% res$parent){
        sel <- res$parent == parent_cur
        res$children[sel] <- I(list(unlist(unique(compact(c(res$children[sel], child_cur))))))

      } else {
        res <- rbind(
          res,
          data.frame(parent = parent_cur, children = I(list(child_cur)))
        )
      }
    }
  }


  assert(all_are_distinct(res$parent))

  structure(
    res,
    class = union("logger_tree", class(res))
  )
}




print.logger_tree <- function(x, ...){
  assert_namespace("cli")
  print(cli::tree(x))
  x
}
