## test_that("test", {
code <- "f <- function (i) {
                while (i < 10) {
                 print(i)
                 if (i == 2){
                   i <- i * 2
                 }
                 i = i+1
               }
              }"

test <- "f(1)"


cc <- code_coverage(code, test)

print.branch_coverage(cc, group="filename")
print(cc)
## })
