test_that("test", {
    code <- "f <- function(x) {
if (x > 0) {
  if (x         >

2) {
    print(x)
  } else {
     2
  }
 } else {3}
}"

    test <- "f(1)"

    cc <- code_coverage(code, test)
    print(cc)
})

## test_that("test 2", {

##   code <- "f <- function(x) {
##              if (x > 0) {
##                if (x > 2) {
##                  print(x)
##                }
##             } else {
##               3
##             }
##           }"


## })
