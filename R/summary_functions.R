#' Provide percent coverage of package
#'
#' Calculate the total percent coverage from a coverage result object.
#' @param x the coverage object returned from [package_coverage()]
#' @param ... additional arguments passed to [tally_coverage()]
#' @return The total percentage as a `numeric(1)`.
#' @export
percent_coverage <- function(x, ...) {
  res <- tally_coverage(x, ...)

  (sum(res$value > 0) / length(res$value)) * 100
}

#' Tally coverage by line or expression
#'
#' @inheritParams percent_coverage
#' @param by whether to tally coverage by line or expression
#' @return a `data.frame` of coverage tallied by line or expression.
#' @export
tally_coverage <- function(x, by = c("line", "expression")) {

  # Rarely something goes wrong with the source references and we get all NAs
  # for them, so we omit them here
  df <- as.data.frame(x, sort=FALSE)

  all_na_rows <- rowSums(is.na(df)) == ncol(df)
  df <- df[!all_na_rows, ]

  if (NROW(df) == 0) {
    return(df)
  }

  by <- match.arg(by)

  switch(by,
         "line" = {

           # if it already has a line column it has already been tallied.
           if (!is.null(df$line)) {
             return(df)
           }

           # aggregate() can't cope with zero-length data frames anyway.
           if (nrow(df) == 0L) {
             return(NULL)
           }

           # results with NA functions (such as from compiled code) are dropped
           # unless NA is a level.
           df$functions <- addNA(df$functions)
           res <- expand_lines(df)

           res <- aggregate(value ~ filename + functions + line,
                                    data = res, FUN = min, na.action = na.pass)
           res$functions <- as.character(res$functions)

           # exclude blank lines from results
           if (inherits(x, "coverage")) {
             srcfiles <- unique(lapply(x, function(x) attr(x$srcref, "srcfile")))

             srcfile_names <- vcapply(srcfiles, `[[`, "filename")

             if (isTRUE(attr(x, "relative"))) {
               srcfile_names <- to_relative_path(srcfile_names, attr(x, "package")$path)
             }

             blank_lines <- compact(
               setNames(lapply(srcfiles, function(srcfile) attr(srcfile_lines(srcfile), "blanks")),
               srcfile_names))

             if (length(blank_lines)) {
               blank_lines <- utils::stack(blank_lines)


               non_blanks <- setdiff.data.frame(
                 res,
                 blank_lines,
                 by.x = c("filename", "line"),
                 by.y = c("ind", "values"))

               res <- res[non_blanks, ]
             }
             res
           }
           res[order(res$filename, res$line), ]
         },

         "expression" = df
         )
}

#' Provide locations of zero coverage
#'
#' When examining the test coverage of a package, it is useful to know if there are
#' any locations where there is **0** test coverage.
#'
#' @param x a coverage object returned [package_coverage()]
#' @param ... additional arguments passed to
#' [tally_coverage()]
#' @return A `data.frame` with coverage data where the coverage is 0.
#' @details if used within RStudio this function outputs the results using the
#' Marker API.
#' @export
zero_coverage <- function(x, ...) {
  coverage_data <- tally_coverage(x, ...)
  coverage_data <- coverage_data[coverage_data$value == 0, , drop = FALSE]

  res <- coverage_data[
    # need to use %in% rather than explicit indexing because
    # tally_coverage returns a df without the columns if
    # by is equal to "line"
    colnames(coverage_data) %in%
      c("filename",
        "functions",
        "line",
        "first_line",
        "last_line",
        "first_column",
        "last_column",
        "value")]

  if (getOption("covr.rstudio_source_markers", TRUE) &&
      rstudioapi::hasFun("sourceMarkers")) {
    markers <- markers(coverage_data)
    rstudioapi::callFun("sourceMarkers",
                        name = "covr",
                        markers = markers,
                        basePath = attr(x, "package")$path,
                        autoSelect = "first")
    invisible(res)
  } else {
    res
  }
}

branch_coverage <- function (x) {
  structure(
    attr(x, "branches"),
    relative = attr(x, "relative"),
    package = attr(x, "package"),
    class = c("branch_coverage", "coverage")
  )
}

#' Tally branch coverage
#'
#' @param x the branch coverage object to be printed
#' @return a `data.frame` of branch coverage
#' @export
tally_branch_coverage <- function (x) {
  branches <- branch_coverage(x)

  as.data.frame(branches, sort=FALSE)
}

#' Print a coverage object
#'
#' @param x the coverage object to be printed
#' @param group whether to group coverage by filename or function
#' @param by whether to count coverage by line or expression
#' @param include_branches whether to include branch coverage or not
#' @param ... additional arguments ignored
#' @return The coverage object (invisibly).
#' @export
print.coverage <- function(x, group = c("filename", "functions"), by = "line", include_branches = FALSE, ...) {
  if (length(x) == 0) {
    return()
  }
  group <- match.arg(group)

  type <- attr(x, "type")

  if (is.null(type) || type == "none") {
    type <- NULL
  }

  df <- tally_coverage(x, by = by)

  if (!NROW(df)) {
    return(invisible())
  }

  percents <- tapply(df$value, df[[group]], FUN = function(x) (sum(x > 0) / length(x)) * 100)

  overall_percentage <- percent_coverage(df, by = by)

  message(crayon::bold(
      paste(collapse = " ",
        c(attr(x, "package")$package, to_title(type), "Coverage: "))),
    format_percentage(overall_percentage))

  by_coverage <- percents[order(percents,
      names(percents))]

  for (i in seq_along(by_coverage)) {
    message(crayon::bold(paste0(names(by_coverage)[i], ": ")),
      format_percentage(by_coverage[i]))
  }

  if (include_branches) {
    print.branch_coverage(x, group=group)
  }

  invisible(x)
}

#' @export
print.coverages <- function(x, ...) {
  for (i in seq_along(x)) {
    # Add a blank line between consecutive coverage items
    if (i != 1) {
      message()
    }
    print(x[[i]], ...)
  }
  invisible(x)
}

#' @export
print.coverages <- function(x, ...) {
  for (i in seq_along(x)) {
    # Add a blank line between consecutive coverage items
    if (i != 1) {
      message()
    }
    print(x[[i]], ...)
  }
  invisible(x)
}

#' Print the branch coverage of a coverage object
#'
#' @param x the branch coverage object to be printed
#' @param group whether to group coverage by filename or function
#' @param ... additional arguments ignored
#' @return The branch coverage object (invisibly).
#' @export
print.branch_coverage <- function(x, group = c("filename", "functions"), ...) {
  df_br <- tally_branch_coverage(x)

  br_c <- branch_coverage(x)
  filenames <- display_name(x)
  br_filenames <- display_name(br_c)
  no_branch <- unique(filenames[!(filenames %in% br_filenames)])

  if(dim(df_br)[1] == 0) {
    message(crayon::bold(paste(collapse = " ",
                               c(attr(x, "package")$package, to_title(attr(x, "type")), "Branch Coverage: N/A"))))
  } else {
    br_percents <- tapply(df_br$value, df_br[[group]], FUN = function(x) (sum(x > 0) / length(x)) * 100)

    overall_br_percentage <- (sum(df_br$value > 0) / length(df_br$value)) * 100

    message(crayon::bold(paste(collapse = " ",
                               c(attr(x, "package")$package, to_title(attr(x, "type")), "Branch Coverage: "))),
            format_percentage(overall_br_percentage))

    by_coverage <- br_percents[order(br_percents,
                                     names(br_percents))]

    for (i in seq_along(by_coverage)) {
      message(crayon::bold(paste0(names(by_coverage)[i], ": ")),
              format_percentage(by_coverage[i]))
    }

    if(length(no_branch) != 0) {
      for (i in seq_along(no_branch)) {
        message(crayon::bold(paste0(no_branch[[i]], ": N/A")))
      }
    }
  }
  invisible(x)
}

format_percentage <- function(x) {
  color <- if (x >= 90) crayon::green
    else if (x >= 75) crayon::yellow
    else crayon::red

  color(sprintf("%02.2f%%", x))
}

markers <- function(x, ...) UseMethod("markers")

markers.coverages <- function(x, ...) {
  mrks <- unlist(lapply(unname(x), markers), recursive = FALSE)

  mrks <- mrks[order(
    vcapply(mrks, `[[`, "file"),
    viapply(mrks, `[[`, "line"),
    vcapply(mrks, `[[`, "message")
    )]

  # request source markers
  rstudioapi::callFun("sourceMarkers",
                      name = "covr",
                      markers = mrks,
                      basePath = NULL,
                      autoSelect = "first")
  invisible()
}

markers.coverage <- function(x, ...) {

  # generate the markers
  markers <- lapply(unname(x), function(xx) {
    filename <- get_source_filename(xx$srcref, full.names = TRUE)

    list(
      type = "warning",
      file = filename,
      line = xx$srcref[1],
      column = xx$srcref[2],
      message = sprintf("No %s Coverage!", to_title(attr(x, "type")))
    )
  })

}

markers.data.frame <- function(x, type = "test") { # nolint
  # generate the markers
  markers <- Map(function(filename, line, column) {
    list(
      type = "warning",
      file = filename,
      line = line,
      column = column %||% 1,
      message = sprintf("No %s Coverage!", to_title(type))
    )},
    x$filename,
    x$first_line %||% x$line,
    x$first_column %||% rep(list(NULL), NROW(x)),
    USE.NAMES = FALSE)
}

# Expand lines given as start and end ranges to enumerate each line
expand_lines <- function(x) {
  repeats <- (x$last_line - x$first_line) + 1L

  lines <- unlist(Map(seq, x$first_line, x$last_line)) %||% integer()

  res <- x[rep(seq_len(NROW(x)), repeats), c("filename", "functions", "value")]
  res$line <- lines
  rownames(res) <- NULL
  res
}
