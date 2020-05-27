# this does not handle LCOV_EXCL_START ect.
parse_gcov <- function(file, package_path = "", br = FALSE) {
  if (!file.exists(file)) {
    return(NULL)
  }

  lines <- readLines(file)
  source_file <- rex::re_matches(lines[1], rex::rex("Source:", capture(name = "source", anything)))$source

  # retrieve full path to the source files
  source_file <- normalize_path(source_file)

  # If the source file does not start with the package path or does not exist ignore it.
  if (!file.exists(source_file) || !grepl(rex::rex(start, package_path), source_file)) {
    return(NULL)
  }

  re <- rex::rex(any_spaces,
    capture(name = "coverage", some_of(digit, "-", "#", "=")),
    ":", any_spaces,
    capture(name = "line", digits),
    ":"
  )

  re_b <- rex::rex("branch  ",
                   capture(name = "branch", digits),
                   " taken ",
                   capture(name = "coverage", digits)
                   )

  re_b2 <- rex::rex("branch  ",
                    capture(name = "branch", digits),
                    " ",
                    capture(name = "coverage", "never executed")
                    )

  matches <- rex::re_matches(lines, re)

  if(br) {
    matches_b <- rex::re_matches(lines, re_b)
    matches_b2 <- rex::re_matches(lines, re_b2)
    matches_b <- cbind(matches_b, line = matches$line, stringsAsFactors = FALSE)
    matches_b2 <- cbind(matches_b2, line = matches$line, stringsAsFactors = FALSE)

    matches <- rbind(matches_b, matches_b2)

    matches <- matches %>%
      tidyr::fill(line)
  }

  # Exclude lines with no match to the pattern
  lines <- lines[!is.na(matches$coverage)]
  matches <- na.omit(matches)
  # gcov lines which have no coverage
  matches$coverage[matches$coverage == "#####"] <- 0 # nolint
  matches$coverage[matches$coverage == "never executed"] <- 0
  # gcov lines which have parse error, so make untracked
  matches$coverage[matches$coverage == "====="] <- "-"

  if(!br) {
    coverage_lines <- matches$line != "0" & matches$coverage != "-"
    matches <- matches[coverage_lines, ]
  }

  values <- as.numeric(matches$coverage)

  if (any(is.na(values))) {
    stop("values could not be coerced to numeric ", matches$coverage)
  }

  # There are no functions for gcov, so we set everything to NA
  functions <- rep(NA_character_, length(values))

  if(br) {
    br_coverages(source_file, matches, values, functions)
  } else {
    line_coverages(source_file, matches, values, functions)
  }
}

clean_gcov <- function(path) {
  src_dir <- file.path(path, "src")

  gcov_files <- list.files(src_dir,
                    pattern = rex::rex(or(".gcda", ".gcno", ".gcov"), end),
                    full.names = TRUE,
                    recursive = TRUE)

  unlink(gcov_files)
}

run_gcov <- function(path, quiet = TRUE,
                      gcov_path = getOption("covr.gcov", ""),
                      gcov_args = getOption("covr.gcov_args", NULL),
                      gcov_br = FALSE) {
  if (!nzchar(gcov_path)) {
    return()
  }

  src_path <- normalize_path(file.path(path, "src"))
  if (!file.exists(src_path)) {
     return()
  }

  gcov_inputs <- list.files(path, pattern = rex::rex(".gcno", end), recursive = TRUE, full.names = TRUE)
  run_gcov_one <- function(src) {
    if(gcov_br) {
      args = c(gcov_args, src, "-p", "-b", "-c", "-o", dirname(src))
    } else {
      args = c(gcov_args, src, "-p", "-o", dirname(src))
    }
    system_check(gcov_path,
      args = args,
      quiet = quiet, echo = !quiet)
    gcov_outputs <- list.files(path, pattern = rex::rex(".gcov", end), recursive = TRUE, full.names = TRUE)
    on.exit(unlink(gcov_outputs))
    unlist(lapply(gcov_outputs, parse_gcov, package_path = path, br = gcov_br), recursive = FALSE)
  }

  withr::with_dir(src_path, {
    compact(unlist(lapply(gcov_inputs, run_gcov_one), recursive = FALSE))
  })
}

line_coverages <- function(source_file, matches, values, functions) {

  # create srcfile reference from the source file
  src_file <- srcfilecopy(source_file, readLines(source_file))

  line_lengths <- vapply(src_file$lines[as.numeric(matches$line)], nchar, numeric(1))

  res <- Map(function(line, length, value, func) {
    src_ref <- srcref(src_file, c(line, 1, line, length))
    res <- list(srcref = src_ref, value = value, functions = func)
    class(res) <- "line_coverage"
    res
  },
  matches$line, line_lengths, values, functions)

  if (!length(res)) {
    return(NULL)
  }

  names(res) <- lapply(res, function(x) key(x$srcref))

  class(res) <- "line_coverages"

  res
}

br_coverages <- function(source_file, matches, values, functions) {

  src_file <- srcfilecopy(source_file, readLines(source_file))

  line_lengths <- vapply(src_file$lines[as.numeric(matches$line)], nchar, numeric(1))

  res <- Map(function (br, line, length, value, func) {
    src_ref <- srcref(src_file, c(line, 1, line, length))
    res <- list(srcref = src_ref, value = value, functions = func, parent = NULL, pos = 1L, default = as.logical(br))
    class(res) <- "br_coverage"
    res},
    matches$branch, matches$line, line_lengths, values, functions)

  if (!length(res)) {
    return(NULL)
  }

  names(res) <- lapply(res, function(x) key(x$srcref))

  class(res) <- "br_coverages"

  res
}


