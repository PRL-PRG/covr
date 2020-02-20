## test_that("test1", {
    source_code1 <- "f <- function(x) {
                            if (x > 0) {
                              if (x > 2) {
                                print(x)
                              } 
                            } else {
                                3
                            }
                          }"
    test_code1 <- "f(1)" ## 2/4 exp (returning NULL); 1/3 br
    source_code2 <- "g <- function(x) {
                            x <- x + 1
                          }"
    test_code2 <- "g(1)" ## no branch

    ## source_code2 <- "g <- function(x) {
    ##                         if (x > 0) {
    ##                           if (x > 2) {
    ##                             print(x)
    ##                           } else {
    ##                              3
    ##                           }
    ##                         }
    ##                       }"
    ## test_code2 <- "g(1)" ## 3/4 exp (returning 3);  2/3 br

    src1 <- tempfile("source1.R")
    test1 <- tempfile("test1.R")
    src2 <- tempfile("source2.R")
    test2 <- tempfile("test2.R")

    cat(source_code1, file = src1)
    cat(test_code1, file = test1)
    cat(source_code2, file = src2)
    cat(test_code2, file = test2)

    on.exit(file.remove(src1, test1, src2, test2))

    cc <- file_coverage(c(src1, src2), c(test1, test2))
    browser()

    df <- tally_branch_coverage(cc)

    ## there shall be six branches
    expect_equal(nrow(df), 6)
    ## three of which are executed
    expect_equal(sum(df$value), 3)
## })

## test_that("test2", {
##     source_code1 <- "f <- function(x) {
##                             if (x > 0) {
##                               if (x > 2) {
##                                 print(x)
##                               } else {
##                                 x <- 10
##                                 x
##                               }
##                             } else {
##                                 if(x == -1) {
##                                   x * 10
##                                 } else {
##                                   5
##                                 }
##                             }
##                           }"
##     test_code1 <- "f(-1)" ## 3/8 exp (returning -10); 2/6 br
##     source_code2 <- "g <- function(x) {
##                             if (x > 0) {
##                               if (x > 2) {
##                                 if (x > 5) {
##                                   print(x)
##                                 } else {
##                                    x * x
##                                 }
##                               } else {
##                                  3
##                               }
##                             } else {
##                               if (x == 10) {
##                                 10
##                               }
##                             }
##                           }"
##     test_code2 <- "g(3)" ## 4/8 exp (returning 3);  3/7 br

##     src1 <- tempfile("source1.r")
##     test1 <- tempfile("test1.r")
##     src2 <- tempfile("source2.r")
##     test2 <- tempfile("test2.r")

##     cat(source_code1, file = src1)
##     cat(test_code1, file = test1)
##     cat(source_code2, file = src2)
##     cat(test_code2, file = test2)

##     on.exit(file.remove(src1, test1, src2, test2))

##     cc <- file_coverage(c(src1, src2), c(test1, test2))
##     df <- tally_branch_coverage(cc)

##     ## there shall be two branches
##     expect_equal(nrow(df), 13)
##     ## five of which are executed
##     expect_equal(sum(df$value), 5)
## })

## test_that("test3", {
##     source_code1 <- "f <- function(x) {
##                             if (x > 0) {
##                               if (x > 2) {
##                                 print(x)
##                               } else {
##                                 x <- 10
##                                 x
##                               }
##                             } else {
##                                 if(x == -1) {
##                                   x * 10
##                                 } else {
##                                   5
##                                 }
##                             }
##                           }"
##     test_code1 <- "f(3)"  ## 3/8 exp (returning 3); 2/6 br
##     source_code2 <- "g <- function(x) {
##                             if (x > 0) {
##                               if (x > 2) {
##                                 if (x > 5) {
##                                   print(x)
##                                 } else {
##                                    x * x
##                                 }
##                               } else {
##                                  3
##                               }
##                             } else {
##                               if (x == 10) {
##                                 10
##                               }
##                             }
##                           }"
##     test_code2 <- "g(0)" ## 2/8 exp (returning NULL);  1/7 br

##     src1 <- tempfile("source1.R")
##     test1 <- tempfile("test1.R")
##     src2 <- tempfile("source2.R")
##     test2 <- tempfile("test2.R")

##     cat(source_code1, file = src1)
##     cat(test_code1, file = test1)
##     cat(source_code2, file = src2)
##     cat(test_code2, file = test2)

##     on.exit(file.remove(src1, test1, src2, test2))

##     cc <- file_coverage(c(src1, src2), c(test1, test2))
##     df <- tally_branch_coverage(cc)

##     ## there shall be two branches
##     expect_equal(nrow(df), 13)
##     ## three of which are executed
##     expect_equal(sum(df$value), 3)
## })
