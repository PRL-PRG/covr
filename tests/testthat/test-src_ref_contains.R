test_that("test", {
    f <- function(x) {
        if (x > 0) {
            g <- function(x) {2}
            unclass(attr(g, "srcref"))
        } else {
            2
        }
    }

  s1 <- unclass(attr(f, "srcref"))
  s2 <- f(1)
  expect_equal(src_ref_contains (s1, s2), TRUE)
})
