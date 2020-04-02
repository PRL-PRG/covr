# This function inserts into existing code explicit branch counting code.
#
# Each branch will be wrapped into
# { if (TRUE) { count_branch(branch_id); NULL}; original_branch }

# For example:
#
# function(x) if (x) 1 else 2
#
# will be turned into:
#
# function(x) {
#   if (x) {
#     if (TRUE) {
#       covr:::count_branch("source.R1920f6bb5ecd8:1:18:1:32:18:32:1:1-1")
#       NULL
#     }
#     1
#   } else {
#     if (TRUE) {
#       covr:::count_branch("source.R1920f6bb5ecd8:1:18:1:32:18:32:1:1-2")
#       NULL
#      }
#      2
#   }
# }
#
#' @importFrom utils getParseData getSrcref tail
impute_branches <- function(x, parent_ref, parent_functions) {
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
    ref <- make_branch_srcref(nrow(pd_child))
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

  count_branch_call <- function(key) {
    call("if", TRUE,
         call("{",
              as.call(list(call(":::", as.symbol("covr"), as.symbol("count_branch")), key)),
              NULL))
  }

  # return early on the following keywords
  if (nrow(pd_child) == 1 && pd_child$token %in% c("NEXT", "BREAK")) {
    return(x)
  } else if (nrow(pd_child) == 3 && pd_child$token[2] %in% c("NS_GET", "NS_GET_INT")) {
    return(x)
  }

  fun <- as.character(x[[1]])[1]

  if ((fun %in% c("!", "~", "~", "+", "-", "*", "/", "^", "<", ">", ":=", "<=",
                 ">=", "==", "!=", "&", "&&", "|", "||", "$", "[", "[[", ":")) ||
        (startsWith(fun, "%") && endsWith(fun, "%"))) {
    if (length(x) == 3) {
      if (!is.null(x[[2]]))
        x[[2]] <- impute_branches(x[[2]], make_srcref(1), parent_functions)
      if (!is.null(x[[3]]))
        x[[3]] <- impute_branches(x[[3]], make_srcref(3), parent_functions)
    } else {
      if (!is.null(x[[1]]))
        x[[1]] <- impute_branches(x[[1]], make_srcref(2), parent_functions)
    }
  } else if (fun == "<-" || fun == "=") {
    if (!is.null(x[[3]]))
      x[[3]] <- impute_branches(x[[3]], make_srcref(3), parent_functions)
  } else if (fun == "->") {
    if (!is.null(x[[2]]))
      x[[2]] <- impute_branches(x[[2]], make_srcref(1), parent_functions)
  } else if (fun == "if") {
    # expression:
    # IF cond then_branch else_branch
    # parse data:
    # IF ( cond ) then_branch ELSE else_branch
    cond_srcref <- make_srcref(3)
    # here it is safe as condition cannot be NULL
    x[[2]] <- impute_branches(x[[2]], cond_srcref, parent_functions)

    # process THEN branch
    then_srcref <- make_branch_srcref(5)
    then_branch <- new_branch(then_srcref, parent_functions, parent_ref, FALSE)

    if (!is.null(x[[3]])) {
      x[[3]] <- impute_branches(x[[3]], then_srcref, parent_functions)
      then_srcref <- if (is_conditional_loop_or_block(x[[3]])) NULL else then_srcref
    }

    x[[3]] <- call("{", count_branch_call(then_branch), x[[3]])
    attr(x[[3]], "srcref") <- list(NULL, NULL, then_srcref)

    # process ELSE branch
    else_srcref <- if (length(x) == 4) {
      else_srcref <- make_branch_srcref(7)
      else_branch <- new_branch(else_srcref, parent_functions, parent_ref, FALSE)

      if (!is.null(x[[4]])) {
        x[[4]] <- impute_branches(x[[4]], else_srcref, parent_functions)
        else_srcref <- if (is_conditional_loop_or_block(x[[4]])) NULL else else_srcref
      }

      x[[4]] <- call("{", count_branch_call(else_branch), x[[4]])
      attr(x[[4]], "srcref") <- list(NULL, NULL, else_srcref)

      else_srcref
    } else {
      default_srcref <- make_default_branch_srcref()
      default_branch <- new_branch(default_srcref, parent_functions, parent_ref, TRUE)
      x <- as.call(c(as.list(x), count_branch_call(default_branch)))

      # since there was no ELSE branch - we do not want trace_calls to generate
      # any counters for this code
      NULL
    }

    attr(x, "srcref") <- list(NULL, cond_srcref, then_srcref, else_srcref)
  } else if (fun == "for") {
    # for parsing data contain 3 elements: FOR, forcond and expr for body
    # the forconf contains: symbol, in and the ectual expression which
    # want to include for the coverage
    cond_srcref <- {
      forcond_id <- pd_child$id[2]
      pd_expr <- pd[pd$parent==forcond_id & pd$token=="expr", ]

      stopifnot(nrow(pd_expr) == 1)

      make_srcref(1, pd=pd_expr)
    }

    body_srcref <- make_branch_srcref(3)
    body_branch <- new_branch(body_srcref, parent_functions, parent_ref, FALSE)

    if (!is.null(x[[3]])) {
      x[[3]] <- impute_branches(x[[3]], cond_srcref, parent_functions)
      cond_srcref <- if (is_conditional_loop_or_block(x[[3]])) NULL else cond_srcref
    }

    if (!is.null(x[[4]])) {
      x[[4]] <- impute_branches(x[[4]], body_srcref, parent_functions)
      body_srcref <- if (is_conditional_loop_or_block(x[[4]])) NULL else body_srcref
    }

    default_branch_srcref <- make_default_branch_srcref()
    default_branch <- new_branch(default_branch_srcref, parent_functions, parent_ref, TRUE)

    # In order to find out which of the two for-loop branches were taken
    # we need to inject a flag into the loop.
    # Not to collide with any existing variables, the flag will be named
    # as __0xABCD where the 0XABCD is string representation of a pointer
    # to the for-loop language object
    branch_guard_var <- as.name(paste0("__", .Call(covr_sexp_address, x)))
    # the assignemt, e.g. `__0xABCD` <- TRUE
    branch_guard_expr <- call("<-", branch_guard_var, TRUE)
    # after the loop body, a check is inserted to find out if the loop has
    # been executed or the default branch was taken, e.g.
    # if (!exists("__OxABCD", inherits=FALSE)) { ... }
    branch_check_expr <-
      call("if",
           call("!", call("exists", as.character(branch_guard_var), inherits=FALSE)),
           count_branch_call(default_branch)
      )

    x[[4]] <- call("{", count_branch_call(body_branch), branch_guard_expr, x[[4]])
    # we need to set all srcref to null except for the body
    # to make sure it is ignored by the expression couter
    attr(x[[4]], "srcref") <- list(NULL, NULL, NULL, body_srcref)

    # remove the body srcref from body position as it has its own set above
    attr(x, "srcref") <- list(NULL, NULL, cond_srcref, NULL)

    x <- call("{", x, branch_check_expr)
  } else if (fun == "while") {
    # x:
    # WHILE cond body
    # pd_child:
    # WHILE ( cond ) body
    x
  } else if (fun == "repeat" && pd_child$token[1] == "REPEAT") {
    # x:
    # REPEAT body
    # pd_child:
    # REPEAT expr
    # repeat always executes
    if (!is.null(x[[2]]))
      x[[2]] <- impute_branches(x[[2]], make_srcref(2), parent_functions)
  } else if (fun == "switch") {
    # from `?switch`:
    # switch works in two distinct ways depending whether the first
    # argument evaluates to a character string or a number.
    #
    # x:
    # SWITCH cond case1 case2 ... caseM
    # pd_child:
    # expr ( expr1 , expr2 , ... , exprN)
    #  or
    # expr ( expr, SYMBOL_SUB EQ_SUB expr1 , ... )
    cond_srcref <- make_srcref(3)
    # condition cannot be be NULL
    x[[2]] <- impute_branches(x[[2]], cond_srcref, parent_functions)
    cond_srcref <- if (is_conditional_loop_or_block(x[[2]])) NULL else cond_srcref

    # collect all, but the actual expression representing the switch itself
    exprs <- tail(which(pd_child$token == "expr"), n = -2)

    # Add NULLs for drop through conditions
    token <- pd_child$token
    next_token <- c(tail(token, n = -1), NA_character_)

    # remove ... like in function(x, ...) switch(x, ...)
    elipsis <- which(sapply(seq_along(pd_child$id), function(i) {
      identical("...", pd$text[pd$parent == pd_child$id[i]])
    }))

    drops <- which(token == "EQ_SUB" & next_token != "expr")
    exprs <- sort(c(exprs, drops))
    # a default in a switch is an unnamed argument
    defaults <- which(token == "','" & next_token != "SYMBOL_SUB") + 1
    defaults <- defaults[!(defaults %in% elipsis)]
    # more than one default is an error so this can only happen if all of the
    # arguments are unname - thus it is the case of integer-switch, which does
    # does not have any defaults
    has_defaults <- if (length(defaults) == 1 || length(elipsis) > 0) {
      TRUE
    } else {
      defaults <- integer()
      FALSE
    }

    if (!has_defaults) {
      default_srcref <- make_default_branch_srcref()
      default_branch <- new_branch(default_srcref, parent_functions, parent_ref, TRUE)

      switch_val <- as.name(paste0("__", .Call(covr_sexp_address, x), "__val"))

      branch_guard_var <- as.name(paste0("__", .Call(covr_sexp_address, x)))
      branch_guard_expr <- call("<-", branch_guard_var, TRUE)
      branch_check_expr <-
        call(
          "if",
          call("!", call("exists", as.character(branch_guard_var), inherits=FALSE)),
          count_branch_call(default_branch),
          switch_val
        )
    }

    for (i in seq_along(exprs)) {
      expr <- exprs[i]
      if (expr %in% drops || expr %in% elipsis) next;
      i <- i + 2

      srcref <- make_branch_srcref(expr)
      branch <- new_branch(srcref, parent_functions, parent_ref, expr %in% defaults)

      if (!is.null(x[[i]])) {
        x[[i]] <- impute_branches(x[[i]], srcref, parent_functions)
        srcref <- if (is_conditional_loop_or_block(x[[i]])) NULL else srcref
      }

      if (has_defaults) {
        new_expr <- call("{", count_branch_call(branch), x[[i]])
        ref <- list(NULL, NULL, srcref)
      } else {
        new_expr <- call("{", branch_guard_expr, count_branch_call(branch), x[[i]])
        ref <- list(NULL, NULL, NULL, srcref)
      }

      x[[i]] <- new_expr
      attr(x[[i]], "srcref") <- ref
    }

    # remove the body srcref from body position as it has its own set above
    refs <- c(list(NULL, cond_srcref), create_null_list(length(x)-2))
    attr(x, "srcref") <- refs

    if (!has_defaults) {
      x <- call("if", TRUE, call("{", call("<-", switch_val, x), branch_check_expr))
    }
  } else if (fun == "function") {
    # first update formals
    args <- if (!is.null(x[[2]])) {
      which(sapply(x[[2]], function(y) !is.symbol(y) || as.character(y) != ""))
    } else {
      integer(0)
    }
    exprs <- which(pd_child$token == "expr")

    stopifnot(length(args) == length(exprs) - 1)

    for (i in seq_along(args)) {
      arg <- args[i]

      arg_srcref <- make_srcref(exprs[i])
      if (!is.null(x[[2]][[arg]])) {
        x[[2]][[arg]] <- impute_branches(x[[2]][[arg]], arg_srcref, parent_functions)

        if (is.null(attr(x[[2]][[arg]], "srcref")) &&
              is.call(x[[2]][[arg]]) &&
              !is_conditional_loop_or_block(x[[2]][[arg]])) {
          attr(x[[2]][[arg]], "srcref") <- arg_srcref
        }
      }
    }

    # then update body
    body_srcref <- make_srcref(nrow(pd_child))
    if (!is.null(x[[3]])) {
      x[[3]] <- impute_branches(x[[3]], body_srcref, parent_functions)
    }

    # covr has to worry about three types of function bodies:
    # 1. function(...) { ... }
    # 2. function(...) if (...) ... # or for/while/switch
    # 3. function(...) single_expression
    #
    # for the 1. case the srcref will be set on the call to {
    # for the 2. case the srcref will be imputed from impute_branches above
    # for the 3. case the srcref will be set explicitly here by turning it into the first case
    if (is.null(attr(x[[3]], "srcref")) && !is_conditional_loop_or_block(x[[3]])) {
      x[[3]] <- call("{", x[[3]])
      attr(x[[3]], "srcref") <- list(NULL, body_srcref)
    }
  } else if (fun == "{") {
    refs <- attr(x, "srcref")
    stopifnot(length(x) == length(refs))
    for (i in seq_along(x)[-1]) {
      if (!is.null(x[[i]]))
        x[[i]] <- impute_branches(x[[i]], refs[[i]], parent_functions)
    }
    # prevent multiple `{` nesting
    # this could happen if the only expression in within the current `{`
    # is one of the control structure for which we impute source references
    if (length(x) == 2 && identical(x[[2]][[1]], as.name("{"))) {
      x <- x[[2]]
    }
  } else if (fun == "(") {
    body_srcref <- make_srcref(2)
    if (!is.null(x[[2]]))
      x[[2]] <- impute_branches(x[[2]], body_srcref, parent_functions)
  } else {
    # take care about the function name explicitly
    if (is.call(x[[1]])) {
      x[[1]] <- impute_branches(x[[1]], make_srcref(1), parent_functions)
    }

    # the arguments must be done manually as some can be empty, e.g. f(.=)
    arg_idx <- 2
    for (i in seq(3, nrow(pd_child)-1)) {
      token <- pd_child$token[i]
      if (token == "expr") {
        if (!is.null(x[[arg_idx]]))
          x[[arg_idx]] <- impute_branches(x[[arg_idx]], make_srcref(i), parent_functions)
      } else if (token == "','") {
        arg_idx <- arg_idx + 1
      }
    }
  }

  x
}

is_conditional_loop_or_block <- function(x) {
  is.call(x) &&
  (identical(x[[1L]], as.name("{")) ||
    (is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("if", "for", "switch", "while")))
}

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
