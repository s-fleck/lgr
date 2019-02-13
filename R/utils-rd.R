r6_usage <- function(
  x,
  name = "x",
  ignore = NULL
){
  els <- collect_usage(x, ignore = ignore)

  c(
    "@section Usage:",
    "```", "",
    strwrap(paste0(name, " <- ", els$ctor), width = 80, exdent = 2), "",
    paste0(name, "$",  els$methods), "",
    paste0(name, "$", els$fields), "",
    "```"
  )
}



collect_usage <- function(
  x,
  name = "x",
  ignore = NULL
){
  public_methods <- vapply(
    setdiff(names(x$public_methods), ignore),
    function(nm) make_function_usage(nm, formals(x$public_methods[[nm]])),
    character(1)
  )


  if ("initialize" %in% names(public_methods)){
    ctor <- public_methods[["initialize"]]
    ctor <- gsub("^initialize", paste0(deparse(substitute(x)), "$new"), ctor)
  } else {
    ctor <- NULL
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
    els <- c(els, collect_usage(x$get_inherit(), ignore = ignore))
    list(
      ctors   = els$ctor,  # the first one
      fields  = unique(unlist(els[names(els) == "fields"])),
      methods = unique(unlist(els[names(els) == "methods"]))
    )
  } else {
    els
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
