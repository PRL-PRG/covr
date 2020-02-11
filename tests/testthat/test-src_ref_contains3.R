test_that("test", {
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
    expect_equal(src_ref_contains (s1, s2), FALSE)
})
