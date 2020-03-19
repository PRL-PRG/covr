#' @importFrom utils getParseData getSrcref tail
impute_srcref_ast <- function(x, parent_ref, strip_function_def=FALSE) {
  if (is.null(parent_ref)) return(x)
  if (!is.call(x)) return(x)

  pd <- tryCatch(get_tokens(parent_ref), error=function(e) return(NULL))

  if (is.null(pd)) return(x)

  pd_expr <-
    (
      (pd$line1 == parent_ref[[1L]] & pd$line2 == parent_ref[[3L]]) |
      (pd$line1 == parent_ref[[7L]] & pd$line2 == parent_ref[[8L]])
    ) &
    pd$col1 == parent_ref[[2L]] &
    pd$col2 == parent_ref[[4L]] &
    pd$token == "expr"

  pd_expr_idx <- which(pd_expr)

  if (length(pd_expr_idx) == 0L) return(x) # srcref not found in parse data

  if (length(pd_expr_idx) > 1) pd_expr_idx <- pd_expr_idx[[1]]

  expr_id <- pd$id[pd_expr_idx]

  pd_child <- pd[pd$parent == expr_id, ]
  pd_child <- pd_child[order(pd_child$line1, pd_child$col1), ]

  # exclude comments
  pd_child <- pd_child[pd_child$token != "COMMENT", ]

  if (pd$line1[pd_expr_idx] == parent_ref[[7L]] & pd$line2[pd_expr_idx] == parent_ref[[8L]]) {
    line_offset <- parent_ref[[7L]] - parent_ref[[1L]]
  } else {
    line_offset <- 0
  }

  make_srcref <- function(from, to = from, pd=pd_child) {
    if (length(from) == 0) {
      return(NULL)
    }

    srcref(
      attr(parent_ref, "srcfile"),
      c(pd$line1[from] - line_offset,
        pd$col1[from],
        pd$line2[to] - line_offset,
        pd$col2[to],
        pd$col1[from],
        pd$col2[to],
        pd$line1[from],
        pd$line2[to]
      )
    )
  }

  make_branch_srcref <- function(from) {
    ref <- make_srcref(from)
    attr(ref, "branch") <- TRUE
    ref
  }

  make_default_branch_srcref <- function() {
    ref <- make_srcref(nrow(pd_child))
    attr(ref, "default_branch") <- TRUE
    ref[1:8] <- c(
      ref[3L],
      ref[6L] + 1,
      ref[3L],
      ref[6L],
      ref[6L] + 1,
      ref[6L],
      ref[3L],
      ref[3L]
    )
    ref
  }

  find_first_counter_srcref <- function(r) {
    if (is.list(r)) {
      non_empty <- Filter(function(x) !is.null(x), r)
      find_first_counter_srcref(non_empty[[1]])
    } else {
      r
    }
  }

  fun <- as.character(x[[1]])

  if ((fun %in% c("!", "~", "~", "+", "-", "*", "/", "^", "<", ">", "<=",
                 ">=", "==", "&", "&&", "|", "||", "$", "[", "[[")) ||
        (startsWith(fun, "%") && endsWith(fun, "%"))) {
    if (length(x) == 3) {
      x[[2]] <- impute_srcref_ast(x[[2]], make_srcref(1))
      x[[3]] <- impute_srcref_ast(x[[3]], make_srcref(3))
    } else {
      x[[1]] <- impute_srcref_ast(x[[1]], make_srcref(2))
    }
  } else if (fun == "<-" || fun == "=") {
    x[[3]] <- impute_srcref_ast(x[[3]], make_srcref(3))
  } else if (fun == "->") {
    x[[2]] <- impute_srcref_ast(x[[2]], make_srcref(1))
  } else if (fun == "if") {
    cond_srcref <- make_srcref(3)
    x[[2]] <- impute_srcref_ast(x[[2]], cond_srcref)

    then_srcref <- make_branch_srcref(5)
    x[[3]] <- impute_srcref_ast(x[[3]], then_srcref)

    else_srcref <- if (length(x) == 4) {
      ref <- make_branch_srcref(7)
      x[[4]] <- impute_srcref_ast(x[[4]], ref)
      ref
    } else {
      tmp <- as.call(c(as.list(x), list(NULL)))
      attributes(tmp) <- attributes(x)
      x <- tmp
      make_default_branch_srcref()
    }

    refs <- list(NULL, cond_srcref, then_srcref, else_srcref)
    attr(x, "srcref") <- refs
  } else if (fun == "for") {
    cond_srcref <- {
      forcond_id <- pd_child$id[2]
      pd_expr <- pd[pd$parent==forcond_id & pd$token=="expr", ]
      stopifnot(nrow(pd_expr) == 1)
      make_srcref(1, pd=pd_expr)
    }
    body_srcref <- make_branch_srcref(3)

    x[[3]] <- impute_srcref_ast(x[[3]], cond_srcref)
    x[[4]] <- impute_srcref_ast(x[[4]], body_srcref)
    attr(x, "srcref") <- list(NULL, NULL, cond_srcref, body_srcref)

    tmp <- attr(x[[4]], "srcref")
    if (!is.null(tmp)) {
      body_srcref <- tmp
    }

    # for this to work, the essential is to find the first and the last
    # expression the first one is used to find out if the loop body was executed
    # or not, and the last one to find out what should be the source reference
    # of the implicit other branch.

    first_body_srcref <- find_first_counter_srcref(body_srcref)

    check_default_branch <- substitute(
      if (!covr::hit(REF)) NULL,
      list(REF=key(first_body_srcref))
    )

    attr(check_default_branch, "srcref") <- list(NULL, NULL, make_default_branch_srcref())

    x <- call("{", x, check_default_branch)
  } else if (fun == "while") {
    stopifnot(FALSE)
  } else if (fun == "switch") {
    stopifnot(FALSE)
  } else if (fun == "function") {
    # first update formals
    args <- if (!is.null(x[[2]])) {
      which(sapply(x[[2]], function(y) !is.symbol(y) || as.character(y) != ""))
    } else {
      integer(0)
    }
    srcrefs <- which(pd_child$token == "expr")

    stopifnot(length(args) == length(srcrefs) - 1)

    for (i in seq_along(args)) {
      arg <- args[i]
      x[[2]][[arg]] <- impute_srcref_ast(x[[2]][[arg]], make_srcref(srcrefs[i]))
    }

    # then update body
    x[[3]] <- impute_srcref_ast(x[[3]], make_srcref(nrow(pd_child)))
  } else if (fun == "{") {
    # TODO: add NULL for srcref of { itself
    refs <- attr(x, "srcref")
    stopifnot(length(x) == length(refs))
    for (i in seq_along(x)[-1]) {
      x[[i]] <- impute_srcref_ast(x[[i]], refs[[i]])
    }
  } else {
    refs <- which(pd_child$token == "expr")
    stopifnot(length(x) == length(refs))

    for (i in seq_along(x)) {
      x[[i]] <- impute_srcref_ast(x[[i]], make_srcref(refs[i]))
    }
  }

  x
}

is_conditional_or_loop <- function(x) is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("if", "for", "else", "switch", "while")
## #' @importFrom utils getParseData getSrcref tail
## impute_srcref <- function(x, parent_ref) {
##   if (!is_conditional_or_loop(x)) return(NULL)
##   if (is.null(parent_ref)) return(NULL)

##   pd <- get_tokens(parent_ref)
##   pd_expr <-
##     (
##       (pd$line1 == parent_ref[[1L]] & pd$line2 == parent_ref[[3L]]) |
##       (pd$line1 == parent_ref[[7L]] & pd$line2 == parent_ref[[8L]])
##     ) &
##     pd$col1 == parent_ref[[2L]] &
##     pd$col2 == parent_ref[[4L]] &
##     pd$token == "expr"
##   pd_expr_idx <- which(pd_expr)
##   if (length(pd_expr_idx) == 0L) return(NULL) # srcref not found in parse data

##   if (length(pd_expr_idx) > 1) pd_expr_idx <- pd_expr_idx[[1]]

##   expr_id <- pd$id[pd_expr_idx]
##   pd_child <- pd[pd$parent == expr_id, ]
##   pd_child <- pd_child[order(pd_child$line1, pd_child$col1), ]

##   # exclude comments
##   pd_child <- pd_child[pd_child$token != "COMMENT", ]

##   if (pd$line1[pd_expr_idx] == parent_ref[[7L]] & pd$line2[pd_expr_idx] == parent_ref[[8L]]) {
##     line_offset <- parent_ref[[7L]] - parent_ref[[1L]]
##   } else {
##     line_offset <- 0
##   }

##   make_srcref <- function(from, to = from) {
##     if (length(from) == 0) {
##       return(NULL)
##     }

##     srcref(
##       attr(parent_ref, "srcfile"),
##       c(pd_child$line1[from] - line_offset,
##         pd_child$col1[from],
##         pd_child$line2[to] - line_offset,
##         pd_child$col2[to],
##         pd_child$col1[from],
##         pd_child$col2[to],
##         pd_child$line1[from],
##         pd_child$line2[to]
##       )
##     )
##   }

##   make_branch_srcref <- function(from) {
##     ref <- make_srcref(from)
##     attr(ref, "branch") <- 1
##     ref
##   }

##   make_default_branch_srcref <- function() {
##     last_expr <- pd_child[nrow(pd_child), ]

##     outer_scope_id <- pd$parent[pd_expr_idx]
##     outer_scope <- pd[pd$id == outer_scope_id, ]

##     ref <- srcref(
##       attr(parent_ref, "srcfile"),
##       c(last_expr$line2,
##         last_expr$col2 + 1,
##         outer_scope$line2,
##         outer_scope$col2 - 1,
##         last_expr$col2 + 1,
##         outer_scope$col2 - 1,
##         last_expr$line2,
##         outer_scope$line2
##         )
##     )

##     attr(ref, "branch") <- 2
##     ref
##   }

##   #browser()

##   switch(
##     as.character(x[[1L]]),
##     "if" = {
##       ## browser()
##       list(
##         NULL,
##         make_srcref(3),
##         make_branch_srcref(5),
##         if (length(x) == 4) make_branch_srcref(7) else make_default_branch_srcref()
##       )
##     },

##     "for" = {
##       list(
##         NULL,
##         NULL,
##         make_srcref(2),
##         make_branch_srcref(3),
##         make_default_branch_srcref()
##       )
##     },

##     "while" = {
##       list(
##         NULL,
##         make_srcref(3),
##         make_branch_srcref(5),
##         make_default_branch_srcref()
##       )
##     },

##     "switch" = {
##       exprs <- tail(which(pd_child$token == "expr"), n = -1)

##       # Add NULLs for drop through conditions
##       token <- pd_child$token
##       next_token <- c(tail(token, n = -1), NA_character_)
##       drops <- which(token == "EQ_SUB" & next_token != "expr")

##       exprs <- sort(c(exprs, drops))

##       ignore_drop_through <- function(x) {
##         if (x %in% drops) {
##           return(NULL)
##         }
##         x
##       }

##       exprs <- lapply(exprs, ignore_drop_through)

##       # Don't create srcrefs for ... conditions
##       ignore_dots <- function(x) {
##         if (identical("...", pd$text[pd$parent == pd_child$id[x]])) {
##           return(NULL)
##         }
##         x
##       }

##       exprs <- lapply(exprs, ignore_dots)

##       browser()
##       # TODO: add default branch is there is no default case
##       c(list(NULL),
##         list(make_srcref(3)),
##         lapply(exprs[-1], make_branch_srcref))
##     },

##     NULL
##   )
## }

## is_conditional_or_loop <- function(x) is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("if", "for", "else", "switch", "while")

package_parse_data <- new.env()

get_parse_data <- function(srcfile) {
  if (length(package_parse_data) == 0) {
    lines <- getSrcLines(srcfile, 1L, Inf)
    res <- lapply(split_on_line_directives(lines),
      function(x) getParseData(parse(text = x, keep.source = TRUE), includeText = TRUE))
    for (i in seq_along(res)) {
      package_parse_data[[names(res)[[i]]]] <- res[[i]]
    }
  }
  package_parse_data[[srcfile[["filename"]]]]
}

clean_parse_data <- function() {
  rm(list = ls(package_parse_data), envir = package_parse_data)
}

# Needed to work around https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16756
get_tokens <- function(srcref) {
  getParseData(srcref) %||% get_parse_data(attr(getSrcref(srcref), "srcfile"))
}
