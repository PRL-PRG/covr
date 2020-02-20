test_that("test1", {
    code <- "f <- function(x) {
if (x > 0) {
  if (x         >

2) {
    print(x)
  } else {
     2}
 } else {                           3}
}"

    test <- "f(1)"

    cc <- code_coverage(code, test)
    df <- tally_branch_coverage(cc)

    ## there shall be four branches
    expect_equal(nrow(df), 4)
    ## two of which are executed
    expect_equal(sum(df$value), 2)
})

test_that("test2", {
  code <- "f <- function(x) {
    if
    (x > 0)
    {
    if
    (x > 2)
    {
    print(x)
    }
    else
    {
    2
    }
    }
    else
    {
    3
    }
}"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be four branches
  expect_equal(nrow(df), 4)
  ## two of which are executed
  expect_equal(sum(df$value), 2)
})
