test_that("test", {
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
                            if (x > 0) {
                              if (x > 2) {
                                print(x)
                              } else {
                                 3
                              }
                            }
                          }"
    test_code2 <- "g(1)" ## 3/4 exp (returning 3);  2/3 br

    src1 <- tempfile("source1.R")
    test1 <- tempfile("test1.R")
    src2 <- tempfile("source2.R")
    test2 <- tempfile("test2.R")
    
    cat(source_code1, file = src1)
    cat(test_code1, file = test1)
    cat(source_code2, file = src2)
    cat(test_code2, file = test2)

    cc <- file_coverage(c(src1, src2), c(test1, test2))

    print(cc)

    on.exit(file.remove(src1, test1, src2, test2))
})
