test_that("function_inside_branch", {
    f <- function(x) {
        if (x > 0) {
            g <- function(x) {2}
            unclass(attr(g, "srcref"))
        } else {
            2
        }
    }

  s1 <- attr(f, "srcref")
  s2 <- f(1)
  expect_equal(srcref_contains (s1, s2), TRUE)
})

test_that("function_outside_branch", {
  f <- function(x) {
    if (x > 0) {
      1
    } else {
      2
    }
  }
  g <- function(x) {2}

  s1 <- unclass(attr(f, "srcref"))
  s2 <- unclass(attr(g, "srcref"))
  expect_equal(srcref_contains (s1, s2), FALSE)
})

test_that("app_inside_condition", {
  f <- function(x) {
    if (x > 0) {
      if (g(x) > 2) {
        0
      } else {
        50
      }
    } else {
      2
    }
  }
  g <- function(x) {                                    2}

  s1 <- unclass(attr(f, "srcref"))
  s2 <- unclass(attr(g, "srcref"))
  expect_equal(srcref_contains (s1, s2), FALSE)
})

test_that("nested", {
  a <- 0
  f <- function(x) {
    if (x > 0) {
      if (g(x) > 2) {
        0
      } else {
        50
      }
    } else {
      g <- function(
                    x) {
        2}
      a <- unclass(attr(g, "srcref"))
      a
    }
  }

  s1 <- unclass(attr(f, "srcref"))
  s2 <- f(0)
  expect_equal(srcref_contains (s1, s2), TRUE)
})
