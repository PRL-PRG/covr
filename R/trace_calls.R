#' trace each call with a srcref attribute
#'
#' This function calls itself recursively so it can properly traverse the AST.
#' @param x the call
#' @param parent_functions the functions which this call is a child of.
#' @param parent_ref argument used to set the srcref of the current call during
#'   the recursion.
#' @seealso <http://adv-r.had.co.nz/Expressions.html>
#' @return a modified expression with count calls inserted before each previous
#' call.
#' @keywords internal
trace_calls <- function (x, parent_functions = NULL, parent_ref = NULL) {

  # Construct the calls by hand to avoid a NOTE from R CMD check
  count <- function(key, val) {
    call("if", TRUE,
      call("{",
        as.call(list(call(":::", as.symbol("covr"), as.symbol("count")), key)),
        val
      )
    )
  }

  if (is.null(parent_functions)) {
    parent_functions <- deparse(substitute(x))
  }
  recurse <- function(y) {
    lapply(y, trace_calls, parent_functions = parent_functions)
  }

  if (is.atomic(x) || is.name(x)) {
    if (is.null(parent_ref)) {
      x
    }
    else {
      if (is_na(x) || is_brace(x)) {
        x
      } else {
        key <- new_counter(parent_ref, parent_functions) # nolint
        count(key, x)
      }
    }
  }
  else if (is.call(x) && identical(x[[1]], as.name("function"))) {
    fun_call <- trace_function_call(x, parent_functions, parent_ref)
    if (!is.null(parent_ref)) {
      src_ref <- impute_srcref(x, parent_ref)
      key <- new_counter(parent_ref, parent_functions)
      count(key, fun_call)
    } else {
      fun_call
    }
  }
  else if (is.call(x)) {
    src_ref <- attr(x, "srcref") %||% impute_srcref(x, parent_ref)

    if ((identical(x[[1]], as.name("<-")) || identical(x[[1]], as.name("="))) && # nolint
      (is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function")))) {
      parent_functions <- c(parent_functions, as.character(x[[2]]))
    }

    # do not try to trace curly curly
    if (identical(x[[1]], as.name("{")) && length(x) == 2 && is.call(x[[2]]) && identical(x[[2]][[1]], as.name("{"))) {
      as.call(x)
    } else if (!is.null(src_ref)) {
      stopifnot(length(x) == length(src_ref))
      as.call(Map(trace_calls, x, src_ref, MoreArgs = list(parent_functions = parent_functions)))
    } else if (!is.null(parent_ref)) {
      key <- new_counter(parent_ref, parent_functions)
      count(key, as.call(recurse(x)))
    } else {
      as.call(recurse(x))
    }
  }
  else if (is.function(x)) {
    # We cannot trace primitive functions
    if (is.primitive(x)) {
      return(x)
    }

    srcref <- attr(x, "srcref")
    fun <- trace_function_call(call("function", formals(x), body(x)), parent_functions, srcref)

    if (!is.null(fun[[2]])) {
      formals(x) <- fun[[2]]
    }
    body(x) <- fun[[3]]
    x
  }
  else if (is.pairlist(x)) {
    as.pairlist(recurse(x))
  }
  else if (is.expression(x)) {
    as.expression(recurse(x))
  }
  else if (is.list(x)) {
    recurse(x)
  }
  else {
    message("Unknown language class: ", paste(class(x), collapse = "/"),
      call. = FALSE)
    x
  }
}

trace_function_call <- function (x, parent_functions = NULL, parent_ref = NULL) {
  fun <- as.list(x)
  fun_formals <- x[[2]]
  fun_body <- x[[3]]

  if (is.null(parent_ref) && length(x) == 4 && inherits(fun[[4]], "srcref")) {
    parent_ref <- x[[4]]
  }

  fun_srcref <- impute_srcref(x, parent_ref)
  fun_body_srcref <- attr(fun_body, "srcref") %||% fun_srcref[[3]]

  if (is.list(fun_body_srcref)) {
    parent_ref <- fun_body_srcref
  } else {
    # The body does not contain `{` neither it does contain any
    # control structure. In such case we use the srcref as a parent
    # so trace_calls generates a counter
    attr(fun_body, "srcref") <- NULL
    parent_ref <- fun_body_srcref %||% parent_ref
  }

  new_body <- trace_calls(fun_body, parent_functions, parent_ref=parent_ref)
  if (!is.null(new_body)) {
    x[[3]] <- new_body
  }

  fun_formals_srcref <- fun_srcref[[2]]
  if (!is.null(fun_formals) && !is.null(fun_formals_srcref)) {

    new_formals <- as.pairlist(
      Map(trace_calls, fun_formals, fun_formals_srcref, MoreArgs = list(parent_functions = parent_functions))
    )
    if (!is.null(new_formals)) {
      x[[2]] <- new_formals
    }
  }

  x
}

.counters <- new.env(parent = emptyenv())

#' initialize a new counter
#'
#' @param src_ref a [base::srcref()]
#' @param parent_functions the functions that this srcref is contained in.
#' @keywords internal
new_counter <- function(src_ref, parent_functions) {
  key <- key(src_ref)
  .counters[[key]]$value <- 0
  .counters[[key]]$srcref <- src_ref
  .counters[[key]]$functions <- parent_functions
  key
}

#' increment a given counter
#'
#' @param key generated with [key()]
#' @keywords internal
count <- function(key) {
  .counters[[key]]$value <- .counters[[key]]$value + 1
}

#' clear all previous counters
#'
#' @keywords internal
clear_counters <- function() {
  rm(envir = .counters, list = ls(envir = .counters))
}

#' Generate a key for a  call
#'
#' @param x the srcref of the call to create a key for
#' @keywords internal
key <- function(x) {
  paste(collapse = ":", c(get_source_filename(x), x))
}

f1 <- function() {
  f2 <- function() {
    2
  }
  f2()
}
