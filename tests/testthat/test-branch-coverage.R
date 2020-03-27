test_that("tracing of default arguments in function", {
  code <- "f <- function(x=1, y=sin(1), z=if (x > 0) y + 1) z"
  cc <- do_code_coverage(code, "f()")

  expect_equal(cc$counters, c("sin(1)", "x > 0", "y + 1", "z"))
  expect_equal(cc$branch_counters, c("y + 1", ""))
  expect_equal(cc$expressions$value, c(1, 1, 1, 1))
  expect_equal(cc$branches$value, c(1, 0))
})

test_that("tracing if default arguments in nested functions", {
  code <- "f <- function() function(x=1, y=sin(1), z=if (x > 0) y + 1) z"
  cc <- do_code_coverage(code, "f()()")

  expect_equal(cc$counters, c("function(x=1, y=sin(1), z=if (x > 0) y + 1) z", "sin(1)", "x > 0", "y + 1", "z"))
  expect_equal(cc$branch_counters, c("y + 1", ""))
  expect_equal(cc$expressions$value, c(1, 1, 1, 1, 1))
  expect_equal(cc$branches$value, c(1, 0))
})

test_that("tracing if default arguments in lambdas", {
  code <- "f <- function() lapply(0:1, function(x, y=sin(1), z=if (x > 0) y + 1) z)"
  cc <- do_code_coverage(code, "f()")

  expect_equal(cc$counters, c(
    "lapply(0:1, function(x, y=sin(1), z=if (x > 0) y + 1) z)",
    "sin(1)",
    "x > 0",
    "y + 1",
    "z")
  )
  expect_equal(cc$branch_counters, c("y + 1", ""))
  expect_equal(cc$expressions$value, c(1, 1, 2, 1, 2))
  expect_equal(cc$branches$value, c(1, 1))
})

test_that("return a function with if returning either a function or a call with lambda", {
  code <- "f <- function() function(x) if (x) function(y) y + 1 else function(y) y + lapply(x, function(z) z + 1)"
  cc <- do_code_coverage(code, "f()(TRUE)(1)")

  expect_equal(cc$counters, c(
    "function(x) if (x) function(y) y + 1 else function(y) y + lapply(x, function(z) z + 1)",
    "x",
    "function(y) y + 1",
    "y + 1",
    "function(y) y + lapply(x, function(z) z + 1)",
    "y + lapply(x, function(z) z + 1)",
    "z + 1"
  ))
  expect_equal(cc$branch_counters, c(
    "function(y) y + 1",
    "function(y) y + lapply(x, function(z) z + 1)"
  ))
  expect_equal(cc$expressions$value, c(1, 1, 1, 1, 0, 0, 0))
  expect_equal(cc$branches$value, c(1, 0))
})

test_that("return a function", {
  code <- "f <- function() function(x) if (x) 1 else function() { x + 1; 2 }"

  cc <- do_code_coverage(code, "f()(FALSE)")
  expect_equal(cc$counters, c(
    "function(x) if (x) 1 else function() { x + 1; 2 }",
    "x",
    "1",
    "function() { x + 1; 2 }",
    "x + 1",
    "2"
  ))
  expect_equal(cc$branch_counters, c("1", "function() { x + 1; 2 }"))
  expect_equal(cc$expressions$value, c(1, 1, 0, 1, 0, 0))
  expect_equal(cc$branches$value, c(0, 1))
})

test_that("return", {
  code <- "f <- function(x) return(if (x > 0) 1)"
  cc <- do_code_coverage(code, "f(0)")
  expect_equal(cc$counters, c("return(if (x > 0) 1)", "x > 0", "1"))
  expect_equal(cc$branch_counters, c("1", ""))
  expect_equal(cc$expressions$value, c(1, 1, 0))
  expect_equal(cc$branches$value, c(0, 1))
})

test_that("repeat", {
  code <- "f <- function(x) repeat { if (x > 0) break; x <- x - 1 }"
  cc <- do_code_coverage(code, "f(1)")
  expect_equal(cc$counters, c("repeat { if (x > 0) break; x <- x - 1 }", "x > 0", "break", "x <- x - 1"))
  expect_equal(cc$branch_counters, c("break", ""))
  expect_equal(cc$expressions$value, c(1, 1, 1, 0))
  expect_equal(cc$branches$value, c(1, 0))
})

test_that(":: and ::: can be traced", {
  code <- "f <- function() tools::md5sum('a')"
  cc <- do_code_coverage(code, "f()")
  expect_equal(cc$counters, c("tools::md5sum('a')"))
  expect_equal(cc$expressions$value, 1)
  expect_length(cc$branches$value, 0)
})

test_that("for with break and next", {
  code <- "f <- function(x) for (i in x) if (i > 0) next else break"

  cc <- do_code_coverage(code, "f(1)")
  expect_equal(cc$counters, c("x", "i > 0", "next", "break"))
  expect_equal(cc$branch_counters, c("if (i > 0) next else break", "next", "break", ""))
  expect_equal(cc$expressions$value, c(1, 1, 1, 0))
  expect_equal(cc$branches$value, c(1, 1, 0, 0))
})

test_that("lambda functions", {
  code <- "f <- function(x) if (TRUE) lapply(x, function(y,z=if(y>1)y+1 else y) if (z>1) 1) else 2"
  cc <- do_code_coverage(code, "f(1)")
  expect_equal(cc$counters, c(
    "TRUE",
    "lapply(x, function(y,z=if(y>1)y+1 else y) if (z>1) 1)",
    "y>1",
    "y+1",
    "y",
    "z>1",
    "1",
    "2"
  ))
  expect_equal(cc$branch_counters, c(
    "lapply(x, function(y,z=if(y>1)y+1 else y) if (z>1) 1)",
    "y+1",
    "y",
    "1",
    "",
    "2"
  ))
  expect_equal(cc$expressions$value, c(1, 1, 1, 0, 1, 1, 0, 0))
  expect_equal(cc$branches$value, c(1, 0, 1, 0, 1, 0))
})

test_that("... in function params", {
  code <- "f <- function(x, ...) if (length(list(...)) > x) 1 else 2"

  cc <- do_code_coverage(code, "f(0, 1, 2)")
  expect_equal(cc$counters, c("length(list(...)) > x", "1", "2"))
  expect_equal(cc$expressions$value, c(1, 1, 0))
  expect_equal(cc$branches$value, c(1, 0))
})

test_that("basic if-else", {
  code <- "f <- function(x) if (x) 1 else 2"

  cc <- do_code_coverage(code, "f(TRUE)")
  expect_equal(cc$counters, c("x", "1", "2"))
  expect_equal(cc$expressions$value, c(1, 1, 0))
  expect_equal(cc$branches$value, c(1, 0))

  cc <- do_code_coverage(code, "f(FALSE)")
  expect_equal(cc$expressions$value, c(1, 0, 1))
  expect_equal(cc$branches$value, c(0, 1))

  cc <- do_code_coverage(code, "f(TRUE);f(FALSE)")
  expect_equal(cc$expressions$value, c(2, 1, 1))
  expect_equal(cc$branches$value, c(1, 1))
})

test_that("nested if-else", {
  code <- "f <- function(x) if (x==1) 1 else if (x==2) 2 else 3"

  cc <- do_code_coverage(code, "f(1)")
  expect_equal(cc$counters, c("x==1", "1", "x==2", "2", "3"))
  expect_equal(cc$expressions$value, c(1, 1, 0, 0, 0))
  expect_equal(cc$branches$value, c(1, 0, 0 ,0))

  cc <- do_code_coverage(code, "f(2)")
  expect_equal(cc$expressions$value, c(1, 0, 1, 1, 0))
  expect_equal(cc$branches$value, c(0, 1, 1, 0))

  cc <- do_code_coverage(code, "f(3)")
  expect_equal(cc$expressions$value, c(1, 0, 1, 0, 1))
  expect_equal(cc$branches$value, c(0, 1, 0, 1))

  cc <- do_code_coverage(code, "f(1);f(2);f(3)")
  expect_equal(cc$expressions$value, c(3, 1, 2, 1, 1))
  expect_equal(cc$branches$value, c(1, 1, 1, 1))
})

## test_that("if in an infix call", {
##   code <- "f <- function(x) { z <- x[[if (length(x) > 0) 1]]; z+1 }"

##   cc <- do_code_coverage(code, "f(1)")
##   expect_equal(cc$counters, c("z <- x[[if (length(x) > 0) 1]]", "length(x) > 0", "1", "z+1"))
##   expect_equal(cc$expressions$value, c(1, 1, 1, 1))
##   expect_equal(cc$branch_counters, c("1", ""))
##   expect_equal(cc$branches$value, c(1, 0))

##   cc <- do_code_coverage(code, "f(NULL)")
##   expect_equal(cc$expressions$value, c(1, 1, 0, 1))
##   expect_equal(cc$branches$value, c(0, 1))
## })

## test_that("basic if-else wrapped in {}", {
##   code <- "f <- function(x) { if (x) 1 else 2 }"

##   cc <- do_code_coverage(code, "f(TRUE)")
##   expect_equal(cc$counters, c("x", "1", "2"))
##   expect_equal(cc$expressions$value, c(1, 1, 0))
##   expect_equal(cc$branches$value, c(1, 0))

##   cc <- do_code_coverage(code, "f(FALSE)")
##   expect_equal(cc$expressions$value, c(1, 0, 1))
##   expect_equal(cc$branches$value, c(0, 1))

##   cc <- do_code_coverage(code, "f(TRUE);f(FALSE)")
##   expect_equal(cc$expressions$value, c(2, 1, 1))
##   expect_equal(cc$branches$value, c(1, 1))
## })

## test_that("basic if-else with branches wrapped in {}", {
##   code <- "f <- function(x) { if (x) { 1;2 } else { 3;4 } }"

##   cc <- do_code_coverage(code, "f(TRUE)")
##   expect_equal(cc$counters, c("x", "1", "2", "3", "4"))
##   expect_equal(cc$expressions$value, c(1, 1, 1, 0, 0))
##   expect_equal(cc$branches$value, c(1, 0))

##   cc <- do_code_coverage(code, "f(FALSE)")
##   expect_equal(cc$expressions$value, c(1, 0, 0, 1, 1))
##   expect_equal(cc$branches$value, c(0, 1))

##   cc <- do_code_coverage(code, "f(TRUE);f(FALSE)")
##   expect_equal(cc$expressions$value, c(2, 1, 1, 1, 1))
##   expect_equal(cc$branches$value, c(1, 1))
## })

## test_that("basic implicit 2nd btanch in if-then", {
##   code <- "f <- function(x) if (x) 1"

##   cc <- do_code_coverage(code, "f(TRUE)")
##   expect_equal(cc$counters, c("x", "1"))
##   expect_equal(cc$expressions$value, c(1, 1))
##   expect_equal(cc$branches$value, c(1, 0))

##   cc <- do_code_coverage(code, "f(FALSE)")
##   expect_equal(cc$expressions$value, c(1, 0))
##   expect_equal(cc$branches$value, c(0, 1))

##   cc <- do_code_coverage(code, "f(TRUE);f(FALSE)")
##   expect_equal(cc$expressions$value, c(2, 1))
##   expect_equal(cc$branches$value, c(1, 1))
## })

## #code <- "f <- function() for(i in 1) { i; i+1 }"
## #code <- "f <- function() for(i in 1) i"
## test_that("for loop with body if", {
##   code <- "f <- function(x) for(i in x) if (i) 1"

##   cc <- do_code_coverage(code, "f(0)")
##   expect_equal(cc$counters, c("x", "i", "1"))
##   expect_equal(cc$expressions$value, c(1, 1, 0))
##   expect_equal(cc$branches$value, c(1, 0, 0, 1))

##   cc <- do_code_coverage(code, "f(1)")
##   expect_equal(cc$expressions$value, c(1, 1, 1))
##   expect_equal(cc$branches$value, c(1, 1, 0, 0))

##   cc <- do_code_coverage(code, "f(NULL)")
##   expect_equal(cc$expressions$value, c(1, 0, 0))
##   expect_equal(cc$branches$value, c(0, 0, 1, 0))
## })

## test_that("for loop with nested if in cond", {
##   code <- "f <- function(x) for(i in if (!x) 1) i"

##   cc <- do_code_coverage(code, "f(TRUE)")
##   expect_equal(cc$counters, c("!x", "1", "i"))
##   expect_equal(cc$expressions$value, c(1, 0, 0))
##   expect_equal(cc$branches$value, c(0, 1, 0, 1))

##   cc <- do_code_coverage(code, "f(FALSE)")
##   expect_equal(cc$expressions$value, c(1, 1, 1))
##   expect_equal(cc$branches$value, c(1, 0, 1, 0))

##   cc <- do_code_coverage(code, "f(FALSE);f(TRUE)")
##   expect_equal(cc$expressions$value, c(2, 1, 1))
##   expect_equal(cc$branches$value, c(1, 1, 1, 1))
## })

## test_that("for loop with nested if in cond", {
##   code <- "f <- function(x) for(i in { if (!x) { 1+1 } }) { if (i==2) 3 }"

##   cc <- do_code_coverage(code, "f(TRUE)")
##   expect_equal(cc$counters, c("!x", "1+1", "i==2", "3"))
##   expect_equal(cc$expressions$value, c(1, 0, 0, 0))
##   expect_equal(cc$branch_counters, c("{ 1+1 }", "", "{ if (i==2) 3 }", "3", "", ""))
##   expect_equal(cc$branches$value, c(0, 1, 0, 0, 0, 1))

##   cc <- do_code_coverage(code, "f(FALSE)")
##   expect_equal(cc$expressions$value, c(1, 1, 1, 1))
##   expect_equal(cc$branches$value, c(1, 0, 1, 1, 0, 0))

##   cc <- do_code_coverage(code, "f(FALSE);f(TRUE)")
##   expect_equal(cc$expressions$value, c(2, 1, 1, 1))
##   expect_equal(cc$branches$value, c(1, 1, 1, 1, 0, 1))
## })

test_that("for loop that doesn't loop", {
  code <- "f <- function(x) { for (i numeric(0)) {print(42)}  }"

})

test_that("basic while", {
  code <- "f <- function(x) { while (x > 0) {x <- x - 1} }"

  cc <- do_code_coverage(code, "f(1)")

  expect_equal(cc$counters, c("x > 0", "x <- x - 1"))
  expect_equal(cc$branch_counters, c("{x <- x - 1}", ""))
  expect_equal(cc$expressions$value, c(2, 1))
  expect_equal(cc$branches$value, c(1, 0)) 
})

test_that("basic while no { }", {
  code <- "f <- function(x) while (x > 0) x <- x - 1"

  cc <- do_code_coverage(code, "f(1)")

  expect_equal(cc$counters, c("x > 0", "x <- x - 1"))
  expect_equal(cc$branch_counters, c("x <- x - 1", ""))
  expect_equal(cc$expressions$value, c(2, 1))
  expect_equal(cc$branches$value, c(1, 0))
})

test_that("while body not in { }", {
  code <- "f <- function(x) { while (TRUE) break }"

  cc <- do_code_coverage(code, "f(0)")

  expect_equal(cc$counters, c("TRUE", "break"))
  expect_equal(cc$branch_counters, c("break", ""))
  expect_equal(cc$expressions$value, c(1, 1))
  expect_equal(cc$branches$value, c(1, 0))
})

test_that("while FALSE", {
  code <- "f <- function(x) { while (FALSE) {x <- x + 1} }"

  cc <- do_code_coverage(code, "f(0)")

  expect_equal(cc$counters, c("FALSE", "x <- x + 1"))
  expect_equal(cc$branch_counters, c("{x <- x + 1}", ""))
  expect_equal(cc$expressions$value, c(1, 0))
  expect_equal(cc$branches$value, c(0, 1))
})

test_that("empty while", {
  code <- "f <- function(x) while (x > 2) {}"

  cc <- do_code_coverage(code, "f(1)")

  expect_equal(cc$counters, c("x > 2"))
  expect_equal(cc$branch_counters, c("{}", ""))
  expect_equal(cc$expressions$value, c(1))
  expect_equal(cc$branches$value, c(0, 1))
})

test_that("while with break and next", {
  code <- "f <- function(x) while (x < 5) { if (x < 3) break else x <- x + 1; print(x) }"

  cc <- do_code_coverage(code, "f(3)")
  expect_equal(cc$counters, c("x < 5", "x < 3", "break", "x <- x + 1", "print(x)"))
  expect_equal(cc$branch_counters, c("{ if (x < 3) break else x <- x + 1; print(x) }", "break", "x <- x + 1", ""))
  expect_equal(cc$expressions$value, c(3, 2, 0, 2, 2))
  expect_equal(cc$branches$value, c(1, 0, 1, 0))
                                        #without ambiguous branch
  code <- "f <- function(x) while (x < 5) { if (x < 3) {break} else {x <- x + 1; print(x)} }"

  cc <- do_code_coverage(code, "f(3)")
  expect_equal(cc$counters, c("x < 5", "x < 3", "break", "x <- x + 1", "print(x)"))
  expect_equal(cc$branch_counters, c("{ if (x < 3) {break} else {x <- x + 1; print(x)} }", "{break}", "{x <- x + 1; print(x)}", ""))
  expect_equal(cc$expressions$value, c(3, 2, 0, 2, 2))
  expect_equal(cc$branches$value, c(1, 0, 1, 0))
})

## test_that("switch with drop through and default", {
##   code <- "f <- function(x) switch(x, a=, b=2, 4, c=3)"

##   cc <- do_code_coverage(code, "f('a')")
##   expect_equal(cc$counters, c("x", "2", "4", "3"))
##   expect_equal(cc$branch_counters, c("2", "4", "3"))
##   expect_equal(cc$expressions$value, c(1, 1, 0, 0))
##   expect_equal(cc$branches$value, c(1, 0, 0))
##   cc <- do_code_coverage(code, "f('b')")
##   expect_equal(cc$expressions$value, c(1, 1, 0, 0))
##   expect_equal(cc$branches$value, c(1, 0, 0))

##   cc <- do_code_coverage(code, "f('d')")
##   expect_equal(cc$expressions$value, c(1, 0, 1, 0))
##   expect_equal(cc$branches$value, c(0, 1, 0))

##   cc <- do_code_coverage(code, "f('c')")
##   expect_equal(cc$expressions$value, c(1, 0, 0, 1))
##   expect_equal(cc$branches$value, c(0, 0, 1))
## })

## test_that("switch without default", {
##   code <- "f <- function(x) switch(x, a=1, b=2, c=3)"

##   cc <- do_code_coverage(code, "f('d')")
##   expect_equal(cc$counters, c("x", "1", "2", "3"))
##   expect_equal(cc$expressions$value, c(1, 0, 0, 0))
##   expect_equal(cc$branches$value, c(0, 0, 0, 1))

##   cc <- do_code_coverage(code, "f('a');f('d')")
##   expect_equal(cc$expressions$value, c(2, 1, 0, 0))
##   expect_equal(cc$branches$value, c(1, 0, 0, 1))
## })

## test_that("switch with elipsis", {
##   code <- "f <- function(x, ...) switch(x, a=, b=2, ...)"

##   cc <- do_code_coverage(code, "f('c', c=4)")
##   expect_equal(cc$counters, c("x", "2"))
##   expect_equal(cc$branch_counters, c("2"))
##   expect_equal(cc$expressions$value, c(1, 0))
##   expect_equal(cc$branches$value, 0)
## })

## test_that("not loosing NULLs", {
##   code <- "
##     f <- function() if (TRUE) NULL else 2
##     g <- function() if (is.null(f())) 3 else 4
##   "
##   cc <- do_code_coverage(code, "g()")
##   expect_equal(cc$counters, c("TRUE", "NULL", "2", "is.null(f())", "3", "4"))
##   expect_equal(cc$branch_counters, c("NULL", "2", "3", "4"))
##   expect_equal(cc$expressions$value, c(1, 1, 0, 1, 1, 0))
##   expect_equal(cc$branches$value, c(1, 0, 1, 0))
## })

## test_that("switch with index", {
##   code <- "f <- function(x) switch (x+1,'a','b','c')"

##   cc <- do_code_coverage(code, "f(1)")
##   expect_equal(cc$counters, c("x+1", "'a'", "'b'", "'c'"))
##   expect_equal(cc$branch_counters, c("'a'", "'b'", "'c'", ""))
##   expect_equal(cc$expressions$value, c(1, 0, 1, 0))
##   expect_equal(cc$branches$value, c(0, 1, 0, 0))

##   cc <- do_code_coverage(code, "f(5)")
##   expect_equal(cc$expressions$value, c(1, 0, 0, 0))
##   expect_equal(cc$branches$value, c(0, 0, 0, 1))
## })

## ## test_that("implicit 2nd branch in if-then", {
##     ## code <- "
##     ## h <- function(...) if (2>1) list(...)
##     ## f <- function(x, y=if(length(x)>1)1 else 2) {
##     ##   zz <- function(a,b,c=b,x=as.name('X'),y=2,...,z1=if(TRUE)1,z2) x+y
##     ##   z <- x[if (length(x)>0) -1 else -2]
##     ##   y <- x[
##     ##     if (x > 0) {      # 1. branch
##     ##       1
##     ##     } else { if (x == 0) { # 2. branch, 3. branch
##     ##       2
##     ##     }
##     ##     4                    # 4. branch
##     ##   }]

##     ##   h(5, if(length(x) < 0) 6 else 7, if(length(x) > 0) 8)

##     ##   y+1
##     ## }

## ##     g <- function() {
## ##       1
## ##     }
## ##     "

## ##     test <- "f(g())"

## ##     cc <- code_coverage(code, test)
## ##     browser()

## ##     df <- tally_branch_coverage(cc)
## ##     expect_equal(nrow(df), 10)

## ##     df_sorted <- df[order(df$first_line, df$first_byte), ]
## ##     expect_equal(df_sorted[, "value"], c(1, 0, 1, 0, 0, 0, 0, 1, 1, 0))
## ## })


## ## #######################################################
## ## #              if branch coverage test                #
## ## #######################################################
## ## test_that("empty_function", {
## ##     code <- "f <- function(x) {}"

## ##     test <- "f(1)"
## ##     ## return NULL; branch coverage 0%

## ##     cc <- code_coverage(code, test)
## ##     df <- tally_branch_coverage(cc)

## ##     ## there shall be no branch
## ##     expect_equal(nrow(df), 0)
## ## })

## ## test_that("no_branch", {
## ##   code <- "add1 <- function(x) {
## ##              x <- x + 1
## ##            }"

## ##   test <- "add1(1)"
## ##   ## return 2; branch coverage 0%

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be no branch
## ##   expect_equal(nrow(df), 0)
## ## })

## ## test_that("many_branches", {
## ##   code <- "many_branches <- function(x) {
## ##   x <- x + 1
## ##   if(x == 0 || x == 1) {
## ##     y <- 2
## ##     if (x + y > 5) {
## ##       print (x+y > 5)
## ##     } else {
## ##       print (x+y <= 5)
## ##     }
## ##   } else {
## ##     if ( if(x > 1) {
## ##       TRUE
## ##     } else {
## ##       FALSE
## ##     }) {
## ##       x <- 5
## ##       if (x/2 == 0) {
## ##         print (x/2 == 0)
## ##       } else {
## ##         if (g(x)) {
## ##           x
## ##         }
## ##       }
## ##     }
## ##   }
## ## }

## ##   g <- function(x) TRUE"

## ##   test <- "many_branches(2)"

## ##   cc <- code_coverage(code,test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be ten branches
## ##   expect_equal(nrow(df), 10)
## ##   ## five of which are executed
## ##   expect_equal(sum(df$value), 5)
## ## })

## ## test_that("app_in_branch1", {
## ##   code <- "f <- function(x) {
## ##         if (x > 0) {
## ##             1
## ##         } else {
## ##             g(2)
## ##         }
## ##     }
## ##     g <- function(x) {2}"

## ##   test <- "f(0)" ## returns 2; branch coverage 1/2

## ##   cc <- code_coverage(code,test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be two branches
## ##   expect_equal(nrow(df), 2)
## ##   ## one of which is executed
## ##   expect_equal(sum(df$value), 1)
## ## })

## ## test_that("app_in_branch2", {
## ##   code <- "f <- function(x) {
## ##         if (x > 0) {
## ##             1
## ##         } else {
## ##             g(2)
## ##         }
## ##     }
## ##     g <- function(x) { if(x > 2) 2 else 4}"

## ##   test <- "f(0)"  ## returns 4; branch coverage 2/4?

## ##   cc <- code_coverage(code,test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be four branches
## ##   expect_equal(nrow(df), 4)
## ##   ## two of which is executed
## ##   expect_equal(sum(df$value), 2)
## ## })

## ## test_that("if_syntax_variation1", {
## ##   code <- "f <- function(x) {
## ##     x <- x + 1
## ##     if(x > 5) {
## ##       x
## ##     } else {
## ##       0
## ##     }
## ##   }"

## ##   test <- "f(0)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be two branches
## ##   expect_equal(nrow(df), 2)
## ##   ## but only one gets value 1
## ##   expect_equal(sum(df$value), 1)
## ## })

## ## test_that("if_syntax_variation2", {
## ##   code <- "f <- function(x) {
## ##     if(x>5)x else 0
## ##   }"

## ##   test <- "f(0)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be two branches
## ##   expect_equal(nrow(df), 2)
## ##   ## but only one gets value 1
## ##   expect_equal(sum(df$value), 1)
## ## })

## ## test_that("if_syntax_variation3", {
## ##   code <- "f <- function(x) {
## ##     if(x>5){x}else{0}
## ##   }"

## ##   test <- "f(0)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be two branches
## ##   expect_equal(nrow(df), 2)
## ##   ## but only one gets value 1
## ##   expect_equal(sum(df$value), 1)
## ## })

## ## test_that("if_syntax_variation3", {
## ##   code <- "f <- function(x) {
## ## if (x > 0) {
## ##   if (x         >

## ## 2) {
## ##     print(x)
## ##   } else {
## ##      2}
## ##  } else {                           3}
## ## }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be four branches
## ##   expect_equal(nrow(df), 4)
## ##   ## two of which are executed
## ##   expect_equal(sum(df$value), 2)
## ## })

## ## test_that("if_syntax_variation4", {
## ##   code <- "f <- function(x) {
## ##     if
## ##     (x > 0)
## ##     {
## ##     if
## ##     (x > 2)
## ##     {
## ##     print(x)
## ##     }
## ##     else
## ##     {
## ##     2
## ##     }
## ##     }
## ##     else
## ##     {
## ##     3
## ##     }
## ## }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be four branches
## ##   expect_equal(nrow(df), 4)
## ##   ## two of which are executed
## ##   expect_equal(sum(df$value), 2)
## ## })

## ## test_that("multiple_files1", {
## ##     source_code1 <- "f <- function(x) {
## ##                             if (x > 0) {
## ##                               if (x > 2) {
## ##                                 print(x)
## ##                               }
## ##                             } else {
## ##                                 3
## ##                             }
## ##                           }"
## ##     test_code1 <- "f(1)" ## 2/4 exp (returning NULL); 1/3 br
## ##     source_code2 <- "g <- function(x) {
## ##                             x <- x + 1
## ##                           }"
## ##     test_code2 <- "g(1)" ## no branch

## ##     src1 <- tempfile("source1.R")
## ##     test1 <- tempfile("test1.R")
## ##     src2 <- tempfile("source2.R")
## ##     test2 <- tempfile("test2.R")

## ##     cat(source_code1, file = src1)
## ##     cat(test_code1, file = test1)
## ##     cat(source_code2, file = src2)
## ##     cat(test_code2, file = test2)

## ##     on.exit(file.remove(src1, test1, src2, test2))

## ##     cc <- file_coverage(c(src1, src2), c(test1, test2))
## ##     df <- tally_branch_coverage(cc)

## ##     ## there shall be three branches
## ##     expect_equal(nrow(df), 3)
## ##     ## three of which are executed
## ##     expect_equal(sum(df$value), 1)
## ## })

## ## test_that("multiple_files2", {
## ##     source_code1 <- "f <- function(x) {
## ##                             if (x > 0) {
## ##                               if (x > 2) {
## ##                                 print(x)
## ##                               } else {
## ##                                 x <- 10
## ##                                 x
## ##                               }
## ##                             } else {
## ##                                 if(x == -1) {
## ##                                   x * 10
## ##                                 } else {
## ##                                   5
## ##                                 }
## ##                             }
## ##                           }"
## ##     test_code1 <- "f(-1)" ## 3/8 exp (returning -10); 2/6 br
## ##     source_code2 <- "g <- function(x) {
## ##                             if (x > 0) {
## ##                               if (x > 2) {
## ##                                 if (x > 5) {
## ##                                   print(x)
## ##                                 } else {
## ##                                    x * x
## ##                                 }
## ##                               } else {
## ##                                  3
## ##                               }
## ##                             } else {
## ##                               if (x == 10) {
## ##                                 10
## ##                               }
## ##                             }
## ##                           }"
## ##     test_code2 <- "g(3)" ## 4/8 exp (returning 3);  3/7 br

## ##     src1 <- tempfile("source1.r")
## ##     test1 <- tempfile("test1.r")
## ##     src2 <- tempfile("source2.r")
## ##     test2 <- tempfile("test2.r")

## ##     cat(source_code1, file = src1)
## ##     cat(test_code1, file = test1)
## ##     cat(source_code2, file = src2)
## ##     cat(test_code2, file = test2)

## ##     on.exit(file.remove(src1, test1, src2, test2))

## ##     cc <- file_coverage(c(src1, src2), c(test1, test2))
## ##     df <- tally_branch_coverage(cc)

## ##     ## there shall be two branches
## ##     expect_equal(nrow(df), 13)
## ##     ## five of which are executed
## ##     expect_equal(sum(df$value), 5)
## ## })

## ## test_that("multiple_files3", {
## ##     source_code1 <- "f <- function(x) {
## ##                             if (x > 0) {
## ##                               if (x > 2) {
## ##                                 print(x)
## ##                               } else {
## ##                                 x <- 10
## ##                                 x
## ##                               }
## ##                             } else {
## ##                                 if(x == -1) {
## ##                                   x * 10
## ##                                 } else {
## ##                                   5
## ##                                 }
## ##                             }
## ##                           }"
## ##     test_code1 <- "f(3)"  ## 3/8 exp (returning 3); 2/6 br
## ##     source_code2 <- "g <- function(x) {
## ##                             if (x > 0) {
## ##                               if (x > 2) {
## ##                                 if (x > 5) {
## ##                                   print(x)
## ##                                 } else {
## ##                                    x * x
## ##                                 }
## ##                               } else {
## ##                                  3
## ##                               }
## ##                             } else {
## ##                               if (x == 10) {
## ##                                 10
## ##                               }
## ##                             }
## ##                           }"
## ##     test_code2 <- "g(0)" ## 2/8 exp (returning NULL);  1/7 br

## ##     src1 <- tempfile("source1.R")
## ##     test1 <- tempfile("test1.R")
## ##     src2 <- tempfile("source2.R")
## ##     test2 <- tempfile("test2.R")

## ##     cat(source_code1, file = src1)
## ##     cat(test_code1, file = test1)
## ##     cat(source_code2, file = src2)
## ##     cat(test_code2, file = test2)

## ##     on.exit(file.remove(src1, test1, src2, test2))

## ##     cc <- file_coverage(c(src1, src2), c(test1, test2))
## ##     df <- tally_branch_coverage(cc)

## ##     ## there shall be two branches
## ##     expect_equal(nrow(df), 13)
## ##     ## three of which are executed
## ##     expect_equal(sum(df$value), 3)
## ## })

## ## test_that("no_else1", {
## ##     code <- "f <- function(x) {
## ##                     if (x > 0) {
## ##                       print(x)
## ##                     }
## ##                    }"
## ##     test <- "f(1)"

## ##     cc <- code_coverage(code, test)
## ##     df <- tally_branch_coverage(cc)

## ##     ## there shall be one branch
## ##     expect_equal(nrow(df), 1)
## ##     ## one of which is executed
## ##     expect_equal(sum(df$value), 1)
## ## })

## ## test_that("no_else2", {
## ##   code <- "f <- function(x) {
## ##                     if (x > 0) {
## ##                       print(x)
## ##                     }
## ##                    }"
## ##   test <- "f(0)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be one branch
## ##   expect_equal(nrow(df), 1)
## ##   ## none of which is executed
## ##   expect_equal(sum(df$value), 0)
## ## })

## ## test_that("no_else_nested1", {
## ##   code <- "f <- function(x) {
## ##                   if (x > 0) {
## ##                     if (x > 2) {
## ##                       print(x)
## ##                     } else {
## ##                       2
## ##                     }
## ##                    }
## ##                  }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be three branches
## ##   expect_equal(nrow(df), 3)
## ##   ## two of which are executed
## ##   expect_equal(sum(df$value), 2)
## ## })

## ## test_that("no_else_nested2", {
## ##   code <- "f <- function(x) {
## ##                   if (x > 0) {
## ##                     if (x > 2) {
## ##                       print(x)
## ##                     }
## ##                   } else {
## ##                       2
## ##                   }
## ##                  }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be three branch
## ##   expect_equal(nrow(df), 3)
## ##   ## one of which is executed
## ##   expect_equal(sum(df$value), 1)
## ## })

## ## test_that("no_else_nested3", {
## ##   code <- "f <- function(x) {
## ##                   if (x > 0) {
## ##                     if (x > 2) {
## ##                       print(x)
## ##                     } else {
## ##                       2
## ##                   }
## ##                  }
## ##                 }"

## ##   test <- "f(0)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   ## there shall be three branch
## ##   expect_equal(nrow(df), 3)
## ##   ## none of which is executed
## ##   expect_equal(sum(df$value), 0)
## ## })

## ## #######################################################
## ## #            while branch coverage test               #
## ## #######################################################
## ## test_that("simple", {
## ##     code <- "f <- function (i) {
## ##              while (i < 6) {
## ##                print(i)
## ##                i = i+1
## ##              }
## ##       }"

## ##     test <- "f(1)"

## ##     cc <- code_coverage(code, test)
## ##     df <- tally_branch_coverage(cc)

## ##     #there shall be one branch
## ##     expect_equal(nrow(df), 1)

## ##     # 100%
## ##     expect_equal((sum(df$value > 0) / length(df$value)) * 100, 100)
## ## })

## ## test_that("nested-while", {
## ##   code <- "f <- function (i) {
## ##              while (i < 3) {
## ##                i = i + 2
## ##                print(i)
## ##                while(FALSE){
## ##                  print(73)
## ##                }
## ##              }
## ##       }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   # there shall be two branches
## ##   expect_equal(nrow(df), 2)

## ##   # 50%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 50)
## ## })

## ## test_that("one-line-while", {
## ##   code <- "f <- function (x) {
## ##              while(x < 5) {x <- x+1; print(x);}
## ##       }"

## ##   test <- "f(3)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   # there shall be one branch
## ##   expect_equal(nrow(df), 1)

## ##   # 100%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 100)
## ## })

## ## test_that("while-if-mix", {
## ##   code <- "f <- function (x) {
## ##              while(x < 5) {x <- x+1; if (x == 3) break; print(x);}
## ##       }"

## ##   test <- "f(3)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   # there shall be two branches
## ##   expect_equal(nrow(df), 2)

## ##   # 50%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 50)
## ## })

## ## test_that("if-while-mix", {
## ##   code <- "f <- function (x) {
## ##              if(x > 5) {
## ##                while(x < 10) {
## ##                  x <- x+1; if (x == 3) break; print(x);
## ##                }
## ##              } else {
## ##                print(x)
## ##              }
## ##       }"

## ##   test <- "f(6)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   # there shall be one branch
## ##   expect_equal(nrow(df), 4)

## ##   # 50%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 50)
## ## })

## ## #######################################################
## ## #              for branch coverage test               #
## ## #######################################################
## ## test_that("non-interative", {
## ##   code <- "f <- function(x) {
## ##        print(x)
## ##        for(i in numeric()) {
## ##                print(73)
## ##              }
## ##      }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   # there shall be one branch
## ##   expect_equal(nrow(df), 1)

## ##   ## 0%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 0)
## ## })

## ## test_that("simple", {
## ##   code <- "f <- function(x) {
## ##        for(i in 1:10) {
## ##          print(x)
## ##        }
## ##      }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   # there shall be one branch
## ##   expect_equal(nrow(df), 1)

## ##   ## 0%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 100)
## ## })

## ## test_that("nested", {
## ##   code <- "f <- function(x) {
## ##        for(i in 1:10) {
## ##          for(j in 1:5) {
## ##            print(i+j)
## ##          }
## ##        }
## ##        x+1
## ##      }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   # there shall be two branches
## ##   expect_equal(nrow(df), 2)

## ##   ## 0%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 100)
## ## })

## ## test_that("nested", {
## ##   code <- "f <- function(x) {
## ##        for(i in 1:10) {
## ##          for(j in numeric()) {
## ##            print(j)
## ##          }
## ##          x <- x+1
## ##        }
## ##        print(x)
## ##      }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   # there shall be two branches
## ##   expect_equal(nrow(df), 2)

## ##   ## 50%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 50)
## ## })

## ## test_that("one_line_for", {
## ##   code <- "f <- function(x) {
## ##        for(i in 1:10) { print(i) }

## ##        for(j in 1:5) { print(j)}
## ##      }"

## ##   test <- "f(1)"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   #there shall be two branches
## ##   expect_equal(nrow(df), 2)

## ##   ## 50%
## ##   expect_equal((sum(df$value > 0) / length(df$value)) * 100, 100)
## ## })

## ## #######################################################
## ## #             switch branch coverage test             #
## ## #######################################################
## ## test_that("simple_switch", {
## ##   code <- "centre <- function(x, type) {
## ##             switch(type,
## ##                    mean = mean(x),
## ##                    median = median(x),
## ##                    trimmed = mean(x, trim = .1))
## ## }"

## ##   test <- "centre(c(1,2,3), \"mean\")"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   #there shall be three branches
## ##   expect_equal(nrow(df), 3)

## ##   ## one of which is executed
## ##   expect_equal(sum(df$value), 1)
## ## })

## ## test_that("if_switch_mix", {
## ##   code <- "monthly_allowance <- function(x) {
## ##              if(x == \"dog\") {
## ##                print(\"5 bones\")
## ##              } else if (x == \"cat\") {
## ##                print(\"10 fish\")
## ##              }

## ##              switch(as.character(x),
## ##                     \"hyeyoung\" = {print (\"1000 czk\")},
## ##                     \"charlie\" = {print (\"500 czk\")})
## ##   }"

## ##   test <- "monthly_allowance (\"dog\")"

## ##   cc <- code_coverage(code, test)
## ##   df <- tally_branch_coverage(cc)

## ##   #there shall be five branches
## ##   expect_equal(nrow(df), 5)

## ##   ## one of which is executed
## ##   expect_equal(sum(df$value), 1)
## ## })
