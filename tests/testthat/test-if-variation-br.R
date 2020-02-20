test_that("test", {
  code <- "f <- function(x) {
    x <- x + 1
    if(x > 5) {
      x
    } else {
      0
    }
  }"

  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be two branches
  expect_equal(nrow(df), 2)
  ## but only one gets value 1
  expect_equal(sum(df$value), 1)
})

test_that("test2", {
  code <- "f <- function(x) {
    if(x>5)x else 0
  }"

  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be two branches
  expect_equal(nrow(df), 2)
  ## but only one gets value 1
  expect_equal(sum(df$value), 1)
})

test_that("test3", {
  code <- "f <- function(x) {
    if(x>5){x}else{0}
  }"

  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be two branches
  expect_equal(nrow(df), 2)
  ## but only one gets value 1
  expect_equal(sum(df$value), 1)
})
