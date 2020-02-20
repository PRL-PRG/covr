test_that("test", {
  code <- "many_branches <- function(x) {
  x <- x + 1
  if(x == 0 || x == 1) {
    y <- 2
    if (x + y > 5) {
      print (x+y > 5)
    } else {
      print (x+y <= 5)
    }
  } else {
    if ( if(x > 1) {
      TRUE
    } else {
      FALSE
    }) {
      x <- 5
      if (x/2 == 0) {
        print (x/2 == 0)
      } else {
        if (g(x)) {
          x
        }
      }
    }
  }
}

  g <- function(x) TRUE"

  test <- "many_branches(2)"

  cc <- code_coverage(code,test)
  df <- tally_branch_coverage(cc)

  ## there shall be ten branches
  expect_equal(nrow(df), 10)
  ## five of which are executed
  expect_equal(sum(df$value), 5)
})
