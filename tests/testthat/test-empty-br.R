test_that("test1", {
    code <- "f <- function(x) {}"

    test <- "f(1)"
    ## return NULL; branch coverage 0%

    cc <- code_coverage(code, test)
    df <- tally_branch_coverage(cc)

    ## there shall be no branch
    expect_equal(nrow(df), 0)
})

test_that("test2", {
  code <- "add1 <- function(x) {
             x <- x + 1
           }"

  test <- "add1(1)"
  ## return 2; branch coverage 0%

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be no branch
  expect_equal(nrow(df), 0)
})
