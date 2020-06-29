
context("increment")

## No triangles ---------------------------------------------------------------

test_that("increment iterator works when 'self' is increments and dimensions of 'self' and 'oth' identical", {
    spec <- SpecIterIncrement(dim_self = 4:3,
                              dim_oth = 4:3,
                             map_dim = 1:2,
                             comp_type_self = "increment")
    iter <- iter_create_increment(spec)
    for (i in 1:12) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(c(1L, i, 0L), iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})

test_that("increment iterator works when 'self' is decrements and dimensions of 'self' and 'oth' identical", {
    spec <- SpecIterIncrement(dim_self = 4:3,
                              dim_oth = 4:3,
                              map_dim = 1:2,
                              comp_type_self = "decrement")
    iter <- iter_create_increment(spec)
    for (i in 1:12) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(c(1L, 0L, i), iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})

test_that("increment iterator works when 'self' consists of orig-dest pair", {
    spec <- SpecIterIncrement(dim_self = c(4L, 4L, 1L),
                              dim_oth = c(4L, 1L),
                              map_dim = c(1L, 0L, 2L),
                              comp_type_self = "orig-dest",
                              indices_orig_self = 1L,
                              indices_dest_self = 2L)
    iter <- iter_create_increment(spec)
    ans <- cbind(1L,
                 rep(1:4, each = 4),
                 rep(1:4, times = 4))
    for (i in 1:16) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(ans[i, ], iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})

test_that("increment iterator works when 'self' consists of orig-dest pair plus one dimension", {
    spec <- SpecIterIncrement(dim_self = c(2L, 4L, 4L),
                              dim_oth = c(2L, 4L),
                              map_dim = c(1L, 2L, 0L),
                              comp_type_self = "orig-dest",
                              indices_orig_self = 2L,
                              indices_dest_self = 3L)
    iter <- iter_create_increment(spec)
    ans <- cbind(rep(1L, times = 32),
                 c(rep(1:2, times = 4),
                   rep(3:4, times = 4),
                   rep(5:6, times = 4),
                   rep(7:8, times = 4)),
                 rep(1:8, times = 4))
    for (i in 1:32) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(ans[i, ], iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})

test_that("increment iterator works when 'self' consists of two orig-dest pairs", {
    spec <- SpecIterIncrement(dim_self = c(3L, 3L, 4L, 4L),
                              dim_oth = c(3L, 4L),
                              map_dim = c(0L, 1L, 0L, 2L),
                              comp_type_self = "orig-dest",
                              indices_orig_self = c(2L, 4L),
                              indices_dest_self = c(1L, 3L))
    iter <- iter_create_increment(spec)
    ans <- cbind(rep(1L, times = 144),
                 rep(1:3, times = 48) + rep(c(0L, 3L, 6L, 9L), each = 9),
                 rep(1:3, each = 3) + rep(c(0L, 3L, 6L, 9L), each = 36))
    for (i in 1:144) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(ans[i, ], iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})

test_that("increment iterator works when 'self' is pool", {
    spec <- SpecIterIncrement(dim_self = 3:1,
                              dim_oth = c(3L, 1L),
                              map_dim = c(1L, 0L, 2L),
                              comp_type_self = "pool",
                              i_direction = 2L)
    iter <- iter_create_increment(spec)
    ans <- cbind(rep(1L, times = 6),
                 c(0L, 0L, 0L, 1L, 2L, 3L),
                 c(1L, 2L, 3L, 0L, 0L, 0L))
    for (i in 1:6) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(ans[i, ], iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})

test_that("increment iterator works when 'self' is pool, and have extra dimension", {
    spec <- SpecIterIncrement(dim_self = c(3L, 2L, 4L),
                              dim_oth = c(3L, 4L),
                              map_dim = c(1L, 0L, 2L),
                              comp_type_self = "pool",
                              i_direction = 2L)
    iter <- iter_create_increment(spec)
    ans <- cbind(rep(1L, times = 24),
                 c(rep(0L, 3), 1:3, rep(0L, 3), 4:6, rep(0L, 3), 7:9, rep(0L, 3), 10:12),
                 c(1:3, rep(0L, 3), 4:6, rep(0L, 3), 7:9, rep(0L, 3), 10:12, rep(0L, 3)))
    for (i in 1:24) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(ans[i, ], iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})


## With triangles -------------------------------------------------------------

test_that("increment iterator works when 'self' is increments and dimensions of 'self' and 'oth' identical - triangles", {
    spec <- SpecIterIncrement(dim_self = 4:2,
                              dim_oth = 4:3,
                              map_dim = c(1:2, 0L),
                              comp_type_self = "increment",
                              i_triangle_self = 3L)
    iter <- iter_create_increment(spec)
    ans <- cbind(rep(1:2, each = 12),
                 rep(1:12, times = 2),
                 0L)
    for (i in 1:24) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(ans[i, ], iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})

test_that("increment iterator works when 'self' is decrements and dimensions of 'self' and 'oth' identical - triangles", {
    spec <- SpecIterIncrement(dim_self = c(4, 2, 3),
                              dim_oth = 4:3,
                              map_dim = c(1, 0, 2),
                              comp_type_self = "decrement",
                              i_triangle_self = 2L)
    iter <- iter_create_increment(spec)
    ans <- cbind(rep(rep(1:2, each = 4), times = 3),
                 0L,
                 c(rep(1:4, times = 2),
                   rep(5:8, times = 2),
                   rep(9:12, times = 2)))
    for (i in 1:24) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(ans[i, ], iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})

test_that("increment iterator works when 'self' consists of orig-dest pair plus three dimensions - triangle", {
    spec <- SpecIterIncrement(dim_self = c(2L, 4L, 4L, 3L, 2L),
                              dim_oth = c(2L, 4L, 3L),
                              map_dim = c(1L, 2L, 0L, 3L, 0L),
                              comp_type_self = "orig-dest",
                              indices_orig_self = 2L,
                              indices_dest_self = 3L,
                              i_triangle = 5L)
    iter <- iter_create_increment(spec)
    ans <- cbind(rep(1:2, each = 96),
                 c(rep(1:2, times = 4),
                   rep(3:4, times = 4),
                   rep(5:6, times = 4),
                   rep(7:8, times = 4)) + rep(c(0L, 8L, 16L), each = 32),
                 rep(1:8, times = 4) + rep(c(0L, 8L, 16L), each = 32))
    for (i in 1:192) {
        expect_true(iter_has_next_increment(iter))
        expect_identical(ans[i, ], iter_next_increment(iter))
    }
    expect_false(iter_has_next_increment(iter))
})






