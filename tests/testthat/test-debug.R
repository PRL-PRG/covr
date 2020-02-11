test_that("test", {
    source_code1 <- "f <- function(x) {
                            if (x > 0) {
                              if (x > 2) {
                                print(x)
                              } else {
                                x <- 10
                                x
                              }
                            } else {
                                if(x == -1) {
                                  x * 10
                                } else {
                                  5
                                }
                            }
                          }"
    ## test_code1 <- "f(-1)" ## 3/8 exp (returning -10); 2/6 br

    test_code1 <- "f(1)" ## 4/8 exp (returning 10); 2/6 br

    src1 <- tempfile("source1.R")
    test1 <- tempfile("test1.R")

    cat(source_code1, file = src1)
    cat(test_code1, file = test1)

    cc <- file_coverage(src1, test1)

    print(cc)

    on.exit(file.remove(src1, test1))
})
