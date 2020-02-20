test_that("test1", {
    code <- "f <- function(x) {
                    if (x > 0) {
                      print(x)
                    }
                   }"
    test <- "f(1)"

    cc <- code_coverage(code, test)
    df <- tally_branch_coverage(cc)

    ## there shall be one branch
    expect_equal(nrow(df), 1)
    ## one of which are executed
    expect_equal(sum(df$value), 1)
})

test_that("test2", {
  code <- "f <- function(x) {
                    if (x > 0) {
                      print(x)
                    }
                   }"
  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be one branch
  expect_equal(nrow(df), 1)
  ## none of which are executed
  expect_equal(sum(df$value), 0)
})

test_that("test3", {
  code <- "f <- function(x) {
                  if (x > 0) {
                    if (x > 2) {
                      print(x)
                    } else {
                      2
                    }
                   }
                 }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be three branches
  expect_equal(nrow(df), 3)
  ## two of which are executed
  expect_equal(sum(df$value), 2)
})

test_that("test3", {
  code <- "f <- function(x) {
                  if (x > 0) {
                    if (x > 2) {
                      print(x)
                    }
                  } else {
                      2
                  }
                 }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be three branch
  expect_equal(nrow(df), 3)
  ## one of which are executed
  expect_equal(sum(df$value), 1)
})

test_that("test3", {
  code <- "f <- function(x) {
                  if (x > 0) {
                    if (x > 2) {
                      print(x)
                    } else {
                      2
                  }
                 } 
                }"

  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be three branch
  expect_equal(nrow(df), 3)
  ## none of which are executed
  expect_equal(sum(df$value), 0)
})
