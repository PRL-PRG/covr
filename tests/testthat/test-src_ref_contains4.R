test_that("test", {
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
    expect_equal(src_ref_contains (s1, s2), TRUE)
})
