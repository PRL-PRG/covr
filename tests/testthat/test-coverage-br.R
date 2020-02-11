test_that("test", {
  code <- "f <- function(x) {
  if (x > 0) {
   1
  } else {
  2
  }
}"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  print(cc)
})
