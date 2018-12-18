r6_usage <- function(
  x,
  name = "x"
){
  public_methods <- vapply(
    names(x$public_methods),
    function(nm) make_function_usage(nm, formals(x$public_methods[[nm]])),
    character(1)
  )
  ctor <- public_methods[["initialize"]]
  ctor <- gsub("^initialize", paste0(deparse(substitute(x)), "$new"), ctor)

  fields <- c(names(x$public_fields), names(x$active))
  if (!is.null(fields)) fields <- sort(fields)

  els <- list(
    ctor = ctor,
    methods =
      public_methods[!names(public_methods) %in% c("initialize", "finalize")],
    fields = fields
  )


  c(
    "@section Usage:",
    "```",
    strwrap(paste(name, "<-", ctor), width = 80, exdent = 2), "",
    paste0(name, "$",  els$methods), "",
    paste0(name, "$", els$fields),
    "```"
  )
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
  sep <- ifelse(res == "", "", "\u{A0}=\u{A0}")
  paste0(names(res), sep, res, collapse = ", ")
}

