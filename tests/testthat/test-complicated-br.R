## test_that("test", {
    code <- "f <- function(x) {
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
              print (\"hooray\")
            }
          }
       }
     }
}

g <- function(x) TRUE"

    test <- "f(2)"


    cc <- code_coverage(code, test)
    ## branch coverage: 5/10
    print.branch_coverage(cc, group="filename")
## })
