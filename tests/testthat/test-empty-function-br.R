## test_that("test", {
    code <- "f <- function(x) {}"

    test <- "f(1)"
    ## return NULL; branch coverage 0%

    cc <- code_coverage(code, test)

    print.branch_coverage(cc)   ## prints "no branch found"

    print(cc)  ## does not print anything
## })
