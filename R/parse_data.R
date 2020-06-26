#' @importFrom utils getParseData getSrcref tail
impute_srcref <- function(x, parent_ref) {
  if (is.null(parent_ref)) return(NULL)
  if (!is.call(x)) return(NULL)

  pd <- get_tokens(parent_ref)
  pd_expr <-
    (
      (pd$line1 == parent_ref[[1L]] & pd$line2 == parent_ref[[3L]]) |
      (pd$line1 == parent_ref[[7L]] & pd$line2 == parent_ref[[8L]])
    ) &
    pd$col1 == parent_ref[[2L]] &
    pd$col2 == parent_ref[[4L]] &
    pd$token == "expr"
  pd_expr_idx <- which(pd_expr)
  if (length(pd_expr_idx) == 0L) return(NULL) # srcref not found in parse data

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

  make_srcref <- function(from, to = from) {
    if (length(from) == 0) {
      return(NULL)
    }

    srcref(
      attr(parent_ref, "srcfile"),
      c(pd_child$line1[from] - line_offset,
        pd_child$col1[from],
        pd_child$line2[to] - line_offset,
        pd_child$col2[to],
        pd_child$col1[from],
        pd_child$col2[to],
        pd_child$line1[from],
        pd_child$line2[to]
      )
    )
  }

  fun <- as.character(x[[1]])[1]

  if (fun == "function") {
    params <- x[[2]]
    params_srcref <- NULL
    if (length(params) > 0) {
      # all params have to have an srcref - even if it is NULL
      params_srcref <- rep(list(NULL), length(params))

      # in `function(a,b=a)`, both a and b are symbols, but we only
      # care about b as that one has expr in parse_data
      params_is_default <- sapply(params, function(y) !is.symbol(y) || as.character(y) != "")
      params_default <- which(params_is_default)
      # we rely on ordering: the default params and exprs from
      # parsing data shall be aligned
      params_expr <- which(pd_child$token == "expr")
      # last expr is the body of the function
      params_expr <- head(params_expr, -1)

      stopifnot(length(params_default) == length(params_expr))

      for (i in seq_along(params_default)) {
        idx <- params_default[i]
        params_srcref[[idx]] <- attr(params[[idx]], "srcref") %||% make_srcref(params_expr[i])
      }
    }

    body_srcref <- attr(x[[3]], "srcref") %||% make_srcref(nrow(pd_child))

    list(
      NULL,
      params_srcref,
      body_srcref,
      NULL
    )
  } else if (fun == "if") {
    src_ref <- list(
      NULL,
      make_srcref(3),
      make_srcref(5),
      make_srcref(7)
    )
    # the fourth component isn't used for an "if" without "else"
    src_ref[seq_along(x)]
  } else if (fun == "for") {
    list(
      NULL,
      NULL,
      make_srcref(2),
      make_srcref(3)
    )
  } else if (fun == "while") {
    list(
      NULL,
      make_srcref(3),
      make_srcref(5)
    )
  } else if (fun == "switch") {
    exprs <- tail(which(pd_child$token == "expr"), n = -1)

    # Add NULLs for drop through conditions
    token <- pd_child$token
    next_token <- c(tail(token, n = -1), NA_character_)
    drops <- which(token == "EQ_SUB" & next_token != "expr")

    exprs <- sort(c(exprs, drops))

    ignore_drop_through <- function(x) {
      if (x %in% drops) {
        return(NULL)
      }
      x
    }

    exprs <- lapply(exprs, ignore_drop_through)

    # Don't create srcrefs for ... conditions
    ignore_dots <- function(x) {
      if (identical("...", pd$text[pd$parent == pd_child$id[x]])) {
        return(NULL)
      }
      x
    }

    exprs <- lapply(exprs, ignore_dots)

    c(list(NULL), lapply(exprs, make_srcref))
  } else {
    NULL
  }
}

is_conditional_or_loop <- function(x) is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("if", "for", "else", "switch")

is_conditional_loop_or_block <- function(x) {
  is.call(x) &&
  (identical(x[[1L]], as.name("{")) ||
    (is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("if", "for", "switch", "while")))
}


package_parse_data <- new.env()

get_parse_data <- function(srcfile) {
  filename <- srcfile[["filename"]]
  keep <- !is.null(filename) && nchar(filename) > 0

  if (!keep || length(package_parse_data) == 0) {
    lines <- getSrcLines(srcfile, 1L, Inf)
    res <- lapply(split_on_line_directives(lines),
                  function(x) getParseData(parse(text = x, keep.source = TRUE), includeText = TRUE))

    if (keep) {
      for (i in seq_along(res)) {
        package_parse_data[[names(res)[[i]]]] <- res[[i]]
      }
    }
  }
  
  if (keep) {
    package_parse_data[[filename]]
  } else {
    res
  }
}

clean_parse_data <- function() {
  rm(list = ls(package_parse_data), envir = package_parse_data)
}

# Needed to work around https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16756
get_tokens <- function(srcref) {
  getParseData(srcref) %||% get_parse_data(attr(getSrcref(srcref), "srcfile"))
}
