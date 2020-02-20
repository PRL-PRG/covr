test_that("test1", {
    code <- "f <- function(x) {
        if (x > 0) {
            1
        } else {
            g(2)
        }
    }
    g <- function(x) {2}"

    test <- "f(0)" ## returns 2; branch coverage 1/2

    cc <- code_coverage(code,test)
    df <- tally_branch_coverage(cc)

    ## there shall be two branches
    expect_equal(nrow(df), 2)
    ## one of which is executed
    expect_equal(sum(df$value), 1)
})

test_that("test2", {
  code <- "f <- function(x) {
        if (x > 0) {
            1
        } else {
            g(2)
        }
    }
    g <- function(x) { if(x > 2) 2 else 4}"

  test <- "f(0)"  ## returns 4; branch coverage 2/4?

  cc <- code_coverage(code,test)
  df <- tally_branch_coverage(cc)

  ## there shall be four branches
  expect_equal(nrow(df), 4)
  ## two of which is executed
  expect_equal(sum(df$value), 2)
})

