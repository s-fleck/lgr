# Return a summary string of the items of a list or environment
# x must be a list or environment
object_summaries <- function(x, exclude = NULL) {
  if (length(x) == 0)
    return(NULL)

  if (is.list(x))
    obj_names <- names(x)
  else if (is.environment(x))
    obj_names <- ls(x, all.names = TRUE)

  obj_names <- setdiff(obj_names, exclude)

  values <- vapply(obj_names, function(name) {
    if (is.environment(x) && bindingIsActive(name, x)) {
      "active binding"
    } else {
      obj <- .subset2(x, name)
      if (is.function(obj)) deparse(args(obj))[[1L]]
      # Plain environments (not envs with classes, like R6 or RefClass objects)
      else if (is.environment(obj) && identical(class(obj), "environment")) "environment"
      else if (is.null(obj)) "NULL"
      else if (is.atomic(obj)) {
        # If obj has many elements, paste() can be very slow, so we'll just
        # use just a subset of it. https://github.com/r-lib/R6/issues/159
        txt <- as.character(utils::head(obj, 60))
        txt <- paste(txt, collapse = " ")
        trim(txt)
      }
      else paste(class(obj), collapse = ", ")
    }
  }, FUN.VALUE = character(1))
}








style_accent <- colt::clt_chr_accent
style_subtle <- colt::clt_chr_subtle

