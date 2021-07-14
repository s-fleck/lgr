r6_usage <- function(
  x,
  name = "x",
  ignore = NULL,
  header = "",
  show_methods = TRUE
){
  if (is.list(x)){
    classname <- deparse(substitute(x))
    classname <- gsub("(list\\()|\\)$", "", classname)
    classname <- unlist(strsplit(classname, ", ", fixed = TRUE))

    res <- lapply(
      seq_along(x),
      function(i){
        collect_usage.R6(
          x = x[[i]],
          classname = classname[[i]],
          ignore = ignore
        )
      }
    )

    res <- list(
      ctor    = unlist(lapply(res, `[[`, "ctor")),
      fields  = unique(unlist(lapply(res, `[[`, "fields"))),
      methods = unique(unlist(lapply(res, `[[`, "methods")))
    )

  } else if (R6::is.R6Class(x)){
    res <- collect_usage.R6(
      x,
      classname = deparse(substitute(x)),
      ignore = ignore
    )
  } else {
    stop("Object ", string_repr(x), "not supported")
  }


  fmt_r6_usage(
    res,
    name = name,
    header = header,
    show_methods = show_methods
  )
}




#' Format R6 usage
#'
#' @param x an `R6ClassGenerator`
#' @param classname `character` scalar. The name of the R6 class
#' @param ignore `character` vector. methods/fields to ignore when generating
#'   usage
#'
#' @return a `list` with the components `ctor`, `fields` and `methods`
#' @noRd
collect_usage.R6 <- function(
  x,
  classname = deparse(substitute(x)),
  ignore = TRUE
){
  public_methods <- vapply(
    setdiff(names(x$public_methods), ignore),
    function(nm) make_function_usage(nm, formals(x$public_methods[[nm]])),
    character(1)
  )


  ctor <- get_public_method_recursively(x, "initialize")
  if (!is.null(ctor)){
    ctor <- make_function_usage(paste0(classname, "$new"), formals(ctor))
  }

  fields <- c(names(x$public_fields), names(x$active))


  if (!is.null(fields)) fields <- sort(fields)
  fields <- setdiff(fields, ignore)

  els <- list(
    ctor = ctor,
    methods =
      public_methods[!names(public_methods) %in% c("initialize", "finalize")],
    fields = fields
  )

  els <- els[!vapply(els, is_empty, FALSE)]

  if ("get_inherit" %in% names(x)){
    els <- c(els, collect_usage.R6(x$get_inherit(), ignore = ignore))
    list(
      ctor    = els$ctor,
      fields  = unique(unlist(els[names(els) == "fields"])),
      methods = unique(unlist(els[names(els) == "methods"]))
    )
  } else {
    els
  }
}




#' Format R6 usage
#'
#' @param x output of collect_usage.R6
#' @param header an optional `character` vector for a heading
#' @param show_methods `logical` scalar: Show methods
#'
#' @return a `character` vector
#' @noRd
fmt_r6_usage <- function(
  x,
  name = x,
  header = "",
  show_methods = TRUE
){
  assert(is_scalar_bool(show_methods))

  res <- c()
  res <- c("@section Usage:", "")


  ctors <- unlist(lapply(
    x$ctor,
    function(.x) c(strwrap(paste0(name, " <- ", .x), width = 80, exdent = 2), "")
  ))

  res <- c(
    res,
    "```",
    header,
    ctors
  )

  if (show_methods){
    res <- c(
      res,
      paste0(name, "$", sort(x$methods)), "",
      paste0(name, "$", sort(x$fields)), "",
      "```"
    )
  }

  res
}




get_public_method_recursively = function(ctor, method){
  if (is.function(ctor))
    return(ctor)
  else if (is.null(ctor))
    return(NULL)

  if (method %in% names(ctor$public_methods)){
    return(ctor$public_methods[[method]])

  } else {
    get_public_method_recursively(ctor$get_inherit(), method)
  }
}




make_function_usage <- function(name, arglist){
  paste0(name, "(", fmt_formals(arglist), ")")
}




fmt_formals <- function(fmls){

  arg_to_text <- function(.x) {
    if (is.symbol(.x) && deparse(.x) == "")
      return("")

    text <- enc2utf8(deparse(.x, backtick = TRUE, width.cutoff = 500L))
    text <- paste0(text, collapse = "\n")
    Encoding(text) <- "UTF-8"
    text
  }

  res <- vapply(fmls, arg_to_text, character(1))
  sep <- ifelse(res == "", "", " = ")
  paste0(names(res), sep, res, collapse = ", ")
}
