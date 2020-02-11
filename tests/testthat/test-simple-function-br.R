## test_that("test", {
code <- "f <- function(x) {
   x <- x+1; print(x)
}"

test <- "f(1)"
## return 2; no branch

cc <- code_coverage(code, test)

print.branch_coverage(cc)   ## prints "no branch found"

print(cc)
## })
