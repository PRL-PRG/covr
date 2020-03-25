do_code_coverage <- function(code, test) {
  sort <- function(df) df[order(df$first_line, df$first_byte), ]

  cc <- code_coverage(code, test)

  coverage <- cc
  coverage <- coverage[
    order(
      sapply(coverage, function(x) x$srcref[1L]),
      sapply(coverage, function(x) x$srcref[5L])
    )
  ]

  branch_coverage <- branch_coverage(cc)
  branch_coverage <- branch_coverage[
    order(
      sapply(branch_coverage, function(x) x$srcref[1L]),
      sapply(branch_coverage, function(x) x$srcref[5L]),
      sapply(branch_coverage, function(x) key(x$parent))
    )
  ]

  counters <- sapply(coverage, function(x) as.character(x$srcref))
  names(counters) <- NULL
  branch_counters <- sapply(branch_coverage, function(x) as.character(x$srcref))
  names(branch_counters) <- NULL

  expressions <- tally_coverage(cc, by="expression")
  expressions <- expressions[order(expressions$first_line, expressions$first_byte), ]

  branches <- tally_branch_coverage(cc)
  branches <- branches[order(branches$first_line, branches$first_byte, branches$parent), ]

  list(
    coverage=coverage,
    counters=counters,
    branch_counters=branch_counters,
    expressions=expressions,
    branches=branches
  )
}
