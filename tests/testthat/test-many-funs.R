test_that("test", {
    code <- "f <- function(x) {
if (x > 0) {
  if (x > 2) {
    print(x)
  } else {
     h(3)
  }
 } else {g(0)}
}

h <- function(x) {
             if (x > 0) {
               if (x > 2) {
                 print(x)
               }
            } else {
              3
            }
          }

g <- function(x) {
             if (x > 0) {
               if (x > 2) {
                 print(x)
               }
            } else {
              3
            }
          }
"

    test <- "f(1)"

    cc <- code_coverage(code, test)
    print(cc)
})

## test_that("test 2", {

##   code <- "g <- function(x) {
##              if (x > 0) {
##                if (x > 2) {
##                  print(x)
##                }
##             } else {
##               3
##             }
##           }"
##   test <- "g(3)"

##   cc <- code_coverage(code, test)
##   print(cc)

## })

## test_that("test 3", {

##     code <- "h <- function(x) {
##              if (x > 0) {
##                if (x > 2) {
##                  print(x)
##                }
##             } else {
##               3
##             }
##           }"
##     test <- "h(0)"

##     cc <- code_coverage(code, test)
##     print(cc)

## })
