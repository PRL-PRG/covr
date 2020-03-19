#######################################################
#              if branch coverage test                #
#######################################################
test_that("empty_function", {
    code <- "f <- function(x) {}"

    test <- "f(1)"
    ## return NULL; branch coverage 0%

    cc <- code_coverage(code, test)
    df <- tally_branch_coverage(cc)

    ## there shall be no branch
    expect_equal(nrow(df[[1]]), 0)
})

test_that("no_branch", {
  code <- "add1 <- function(x) {
             x <- x + 1
           }"

  test <- "add1(1)"
  ## return 2; branch coverage 0%

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be no branch
  expect_equal(nrow(df[[1]]), 0)
})

test_that("many_branches", {
  code <- "many_branches <- function(x) {
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
          x
        }
      }
    }
  }
}

  g <- function(x) TRUE"

  test <- "many_branches(2)"

  cc <- code_coverage(code,test)
  df <- tally_branch_coverage(cc)

  ## there shall be ten branches
  expect_equal(nrow(df[[1]]), 10)
  ## five of which are executed
  expect_equal(sum(df[[1]]$value), 5)
})

test_that("app_in_branch1", {
  code <- "f <- function(x) {
        if (x > 0) {
            1
        } else {
            g(2)
        }
    }
    g <- function(x) {2}"

  test <- "f(0)" ## returns 2; branch coverage 1/2

  cc <- code_coverage(code,test)
  df <- tally_branch_coverage(cc)

  ## there shall be two branches
  expect_equal(nrow(df[[1]]), 2)
  ## one of which is executed
  expect_equal(sum(df[[1]]$value), 1)
})

test_that("app_in_branch2", {
  code <- "f <- function(x) {
        if (x > 0) {
            1
        } else {
            g(2)
        }
    }
    g <- function(x) { if(x > 2) 2 else 4}"

  test <- "f(0)"  ## returns 4; branch coverage 2/4?

  cc <- code_coverage(code,test)
  df <- tally_branch_coverage(cc)

  ## there shall be four branches
  expect_equal(nrow(df[[1]]), 4)
  ## two of which is executed
  expect_equal(sum(df[[1]]$value), 2)
})

test_that("if_syntax_variation1", {
  code <- "f <- function(x) {
    x <- x + 1
    if(x > 5) {
      x
    } else {
      0
    }
  }"

  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be two branches
  expect_equal(nrow(df[[1]]), 2)
  ## but only one gets value 1
  expect_equal(sum(df[[1]]$value), 1)
})

test_that("if_syntax_variation2", {
  code <- "f <- function(x) {
    if(x>5)x else 0
  }"

  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be two branches
  expect_equal(nrow(df[[1]]), 2)
  ## but only one gets value 1
  expect_equal(sum(df[[1]]$value), 1)
})

test_that("if_syntax_variation3", {
  code <- "f <- function(x) {
    if(x>5){x}else{0}
  }"

  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be two branches
  expect_equal(nrow(df[[1]]), 2)
  ## but only one gets value 1
  expect_equal(sum(df[[1]]$value), 1)
})

test_that("if_syntax_variation3", {
  code <- "f <- function(x) {
if (x > 0) {
  if (x         >

2) {
    print(x)
  } else {
     2}
 } else {                           3}
}"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be four branches
  expect_equal(nrow(df[[1]]), 4)
  ## two of which are executed
  expect_equal(sum(df[[1]]$value), 2)
})

test_that("if_syntax_variation4", {
  code <- "f <- function(x) {
    if
    (x > 0)
    {
    if
    (x > 2)
    {
    print(x)
    }
    else
    {
    2
    }
    }
    else
    {
    3
    }
}"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be four branches
  expect_equal(nrow(df[[1]]), 4)
  ## two of which are executed
  expect_equal(sum(df[[1]]$value), 2)
})

test_that("multiple_files1", {
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
    df <- tally_branch_coverage(cc)

    ## there shall be three branches
    expect_equal(nrow(df[[1]]), 3)
    ## three of which are executed
    expect_equal(sum(df[[1]]$value), 1)
})

test_that("multiple_files2", {
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
    test_code1 <- "f(-1)" ## 3/8 exp (returning -10); 2/6 br
    source_code2 <- "g <- function(x) {
                            if (x > 0) {
                              if (x > 2) {
                                if (x > 5) {
                                  print(x)
                                } else {
                                   x * x
                                }
                              } else {
                                 3
                              }
                            } else {
                              if (x == 10) {
                                10
                              }
                            }
                          }"
    test_code2 <- "g(3)" ## 4/8 exp (returning 3);  3/7 br

    src1 <- tempfile("source1.r")
    test1 <- tempfile("test1.r")
    src2 <- tempfile("source2.r")
    test2 <- tempfile("test2.r")

    cat(source_code1, file = src1)
    cat(test_code1, file = test1)
    cat(source_code2, file = src2)
    cat(test_code2, file = test2)

    on.exit(file.remove(src1, test1, src2, test2))

    cc <- file_coverage(c(src1, src2), c(test1, test2))
    df <- tally_branch_coverage(cc)

    ## there shall be two branches
    expect_equal(nrow(df[[1]]), 13)
    ## five of which are executed
    expect_equal(sum(df[[1]]$value), 5)
})

test_that("multiple_files3", {
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
    test_code1 <- "f(3)"  ## 3/8 exp (returning 3); 2/6 br
    source_code2 <- "g <- function(x) {
                            if (x > 0) {
                              if (x > 2) {
                                if (x > 5) {
                                  print(x)
                                } else {
                                   x * x
                                }
                              } else {
                                 3
                              }
                            } else {
                              if (x == 10) {
                                10
                              }
                            }
                          }"
    test_code2 <- "g(0)" ## 2/8 exp (returning NULL);  1/7 br

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
    df <- tally_branch_coverage(cc)

    ## there shall be two branches
    expect_equal(nrow(df[[1]]), 13)
    ## three of which are executed
    expect_equal(sum(df[[1]]$value), 3)
})

test_that("no_else1", {
    code <- "f <- function(x) {
                    if (x > 0) {
                      print(x)
                    }
                   }"
    test <- "f(1)"

    cc <- code_coverage(code, test)
    df <- tally_branch_coverage(cc)

    ## there shall be one branch
    expect_equal(nrow(df[[1]]), 1)
    ## one of which is executed
    expect_equal(sum(df[[1]]$value), 1)
})

test_that("no_else2", {
  code <- "f <- function(x) {
                    if (x > 0) {
                      print(x)
                    }
                   }"
  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be one branch
  expect_equal(nrow(df[[1]]), 1)
  ## none of which is executed
  expect_equal(sum(df[[1]]$value), 0)
})

test_that("no_else_nested1", {
  code <- "f <- function(x) {
                  if (x > 0) {
                    if (x > 2) {
                      print(x)
                    } else {
                      2
                    }
                   }
                 }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be three branches
  expect_equal(nrow(df[[1]]), 3)
  ## two of which are executed
  expect_equal(sum(df[[1]]$value), 2)
})

test_that("no_else_nested2", {
  code <- "f <- function(x) {
                  if (x > 0) {
                    if (x > 2) {
                      print(x)
                    }
                  } else {
                      2
                  }
                 }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be three branch
  expect_equal(nrow(df[[1]]), 3)
  ## one of which is executed
  expect_equal(sum(df[[1]]$value), 1)
})

test_that("no_else_nested3", {
  code <- "f <- function(x) {
                  if (x > 0) {
                    if (x > 2) {
                      print(x)
                    } else {
                      2
                  }
                 } 
                }"

  test <- "f(0)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  ## there shall be three branch
  expect_equal(nrow(df[[1]]), 3)
  ## none of which is executed
  expect_equal(sum(df[[1]]$value), 0)
})

#######################################################
#            while branch coverage test               #
#######################################################
test_that("simple", {
    code <- "f <- function (i) {
             while (i < 6) {
               print(i)
               i = i+1
             }
      }"

    test <- "f(1)"

    cc <- code_coverage(code, test)
    df <- tally_branch_coverage(cc)

    #there shall be one branch
    expect_equal(nrow(df[[1]]), 1)

    # 100%
    expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 100)
})

test_that("nested-while", {
  code <- "f <- function (i) {
             while (i < 3) {
               i = i + 2
               print(i)
               while(FALSE){
                 print(73)
               }
             }
      }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  # there shall be two branches
  expect_equal(nrow(df[[1]]), 2)

  # 50%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 50)
})

test_that("one-line-while", {
  code <- "f <- function (x) {
             while(x < 5) {x <- x+1; print(x);}
      }"

  test <- "f(3)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  # there shall be one branch
  expect_equal(nrow(df[[1]]), 1)

  # 100%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 100)
})

test_that("while-if-mix", {
  code <- "f <- function (x) {
             while(x < 5) {x <- x+1; if (x == 3) break; print(x);}
      }"

  test <- "f(3)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  # there shall be two branches
  expect_equal(nrow(df[[1]]), 2)

  # 50%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 50)
})

test_that("if-while-mix", {
  code <- "f <- function (x) {
             if(x > 5) {
               while(x < 10) {
                 x <- x+1; if (x == 3) break; print(x);
               }
             } else {
               print(x)
             }
      }"

  test <- "f(6)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  # there shall be one branch
  expect_equal(nrow(df[[1]]), 4)

  # 50%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 50)
})

#######################################################
#              for branch coverage test               #
#######################################################
test_that("non-interative", {
  code <- "f <- function(x) {
       print(x)
       for(i in numeric()) {
               print(73)
             }
     }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  # there shall be one branch
  expect_equal(nrow(df[[1]]), 1)

  ## 0%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 0)
})

test_that("simple", {
  code <- "f <- function(x) {
       for(i in 1:10) {
         print(x)
       }
     }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  # there shall be one branch
  expect_equal(nrow(df[[1]]), 1)

  ## 0%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 100)
})

test_that("nested", {
  code <- "f <- function(x) {
       for(i in 1:10) {
         for(j in 1:5) {
           print(i+j)
         }
       }
       x+1
     }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  # there shall be two branches
  expect_equal(nrow(df[[1]]), 2)

  ## 0%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 100)
})

test_that("nested", {
  code <- "f <- function(x) {
       for(i in 1:10) {
         for(j in numeric()) {
           print(j)
         }
         x <- x+1
       }
       print(x)
     }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  # there shall be two branches
  expect_equal(nrow(df[[1]]), 2)

  ## 50%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 50)
})

test_that("one_line_for", {
  code <- "f <- function(x) {
       for(i in 1:10) { print(i) }

       for(j in 1:5) { print(j)}
     }"

  test <- "f(1)"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  #there shall be two branches
  expect_equal(nrow(df[[1]]), 2)

  ## 50%
  expect_equal((sum(df[[1]]$value > 0) / length(df[[1]]$value)) * 100, 100)
})

#######################################################
#             switch branch coverage test             #
#######################################################
test_that("simple_switch", {
  code <- "centre <- function(x, type) {
            switch(type,
                   mean = mean(x),
                   median = median(x),
                   trimmed = mean(x, trim = .1))
}"

  test <- "centre(c(1,2,3), \"mean\")"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  #there shall be three branches
  expect_equal(nrow(df[[1]]), 3)

  ## one of which is executed
  expect_equal(sum(df[[1]]$value), 1)
})

test_that("if_switch_mix", {
  code <- "monthly_allowance <- function(x) {
             if(x == \"dog\") {
               print(\"5 bones\")
             } else if (x == \"cat\") {
               print(\"10 fish\")
             }

             switch(as.character(x),
                    \"hyeyoung\" = {print (\"1000 czk\")},
                    \"charlie\" = {print (\"500 czk\")})
  }"

  test <- "monthly_allowance (\"dog\")"

  cc <- code_coverage(code, test)
  df <- tally_branch_coverage(cc)

  #there shall be five branches
  expect_equal(nrow(df[[1]]), 5)

  ## one of which is executed
  expect_equal(sum(df[[1]]$value), 1)
})
