#' @export
as.data.frame.coverage <- function(x, row.names = NULL, optional = FALSE, sort = TRUE, ...) {
  column_names <- c("filename", "functions", "first_line", "first_byte", "last_line", "last_byte",
               "first_column", "last_column", "first_parsed",
               "last_parsed", "value")

  res <- setNames(c(list(character(0)), rep(list(numeric(0)), times = length(column_names) - 1)),
                  column_names)
  if (length(x)) {
    res$filename <- display_name(x)
    res$functions <- vcapply(x, function(xx) xx$functions[1])

    vals <- t(vapply(x,
                     function(xx) c(xx$srcref, xx$value),
                     numeric(9), USE.NAMES = FALSE))
    for (i in seq_len(NCOL(vals))) {
      res[[i + 2]] <- vals[, i]
    }
  }

  df <- data.frame(res, stringsAsFactors = FALSE, check.names = FALSE)

  if (sort) {
    # if we are sorting we no longer need to preserve the order of the input and can merge values together
    df <- merge_values(df)

    df <- df[order(df$filename, df$first_line, df$first_byte, df$last_line, df$last_byte), ]
  }

  rownames(df) <- NULL

  df
}

#' @export
as.data.frame.branch_coverage <- function(x,  ...) {
  column_names <- c("filename", "functions",
                    "first_line", "first_byte", "last_line", "last_byte",
                    "first_column", "last_column", "first_parsed", "last_parsed",
                    "value", "parent", "pos", "default")

  res <- setNames(c(list(character(0), character(0)),
                    rep(list(numeric(0)), times=8), # srcref
                    list(numeric(0), character(0), numeric(0), logical(0))),
                  column_names)
  if (length(x)) {
    res$filename <- display_name(x)
    res$functions <- vcapply(x, function(xx) xx$functions[1])
    res$parent <- vcapply(x, function(xx) key(xx$parent))
    res$pos <- viapply(x, function(xx) xx$pos)
    res$default <- vlapply(x, function(xx) xx$default)

    vals <- t(vapply(x,
                     function(xx) c(xx$srcref, xx$value),
                     numeric(9), USE.NAMES = FALSE))
    for (i in seq_len(NCOL(vals))) {
      res[[i + 2]] <- vals[, i]
    }
  }

  df <- data.frame(res, row.names=NULL, stringsAsFactors = FALSE, check.names = FALSE)

  df
}

merge_values <- function(x, sentinel = "___NA___") {
  # We can't use aggregate directly, because it doesn't allow missing values in
  # grouping variables...
  x$functions[is.na(x$functions)] <- sentinel
  res <- aggregate(value ~ ., x, sum)
  res$functions[res$functions == sentinel] <- NA_character_
  res
}
