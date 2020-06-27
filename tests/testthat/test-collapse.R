
context("collapse")

test_that("collapse iterator works when 'self' and 'oth' are identical", {
    spec <- SpecIterCollapse(dim_self = 4:3,
                             dim_oth = 4:3,
                             map_dim = 1:2,
                             map_pos = list(1:4, 1:3))
    iter <- iter_create_collapse(spec)
    for (i in 1:12) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(i, iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'oth' is transpose of 'self'", {
    spec <- SpecIterCollapse(dim_self = 4:3,
                             dim_oth = 3:4,
                             map_dim = 2:1,
                             map_pos = list(1:4, 1:3))
    self <- matrix(1:12, 4, 3)
    oth <- t(self)
    ans <- match(self, oth)
    iter <- iter_create_collapse(spec)
    for (i in 1:12) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'oth' permutes positions on dimensions of 'self'", {
    spec <- SpecIterCollapse(dim_self = 4:3,
                             dim_oth = 4:3,
                             map_dim = 1:2,
                             map_pos = list(c(1, 4, 2, 3), c(3, 1, 2)))
    self <- matrix(1:12, 4, 3)
    oth <- self[c(1, 3, 4, 2), c(2, 3, 1)]
    ans <- match(self, oth)
    iter <- iter_create_collapse(spec)
    for (i in 1:12) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'oth' subsets positions on dimensions of 'self'", {
    spec <- SpecIterCollapse(dim_self = 4:3,
                             dim_oth = c(3, 1),
                             map_dim = 1:2,
                             map_pos = list(c(1, 2, 0, 3), c(0, 1, 0)))
    self <- matrix(1:12, 4, 3)
    oth <- self[c(1, 2, 4), 2, drop = FALSE]
    ans <- match(self, oth, nomatch = 0L)
    iter <- iter_create_collapse(spec)
    for (i in 1:12) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'oth' permutes and subsets 'self'", {
    spec <- SpecIterCollapse(dim_self = 4:2,
                             dim_oth = c(2, 3, 1),
                             map_dim = c(2, 3, 1),
                             map_pos = list(c(0, 1, 3, 2), c(0, 1, 0), 1:2))
    self <- array(1:24, 4:2)
    oth <- aperm(self, perm = c(3, 1, 2))
    oth <- oth[ , c(2, 4, 3), 2]
    ans <- match(self, oth, nomatch = 0L)
    iter <- iter_create_collapse(spec)
    for (i in 1:24) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'oth' collapses whole dimension of 'self'", {
    spec <- SpecIterCollapse(dim_self = 4:3,
                             dim_oth = 4,
                             map_dim = c(1, 0),
                             map_pos = list(1:4, c(0, 0, 0)))
    ans <- rep(1:4, times = 3)
    iter <- iter_create_collapse(spec)
    for (i in 1:12) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'oth' collapses som positions of 'self'", {
    spec <- SpecIterCollapse(dim_self = 4:3,
                             dim_oth = c(3, 2),
                             map_dim = 1:2,
                             map_pos = list(c(1:3, 3), c(1, 2, 1)))
    ans <- c(1:3, 3L,
             4:6, 6L,
             1:3, 3L)
    iter <- iter_create_collapse(spec)
    for (i in 1:12) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'self' collapsed to single value", {
    spec <- SpecIterCollapse(dim_self = 4:2,
                             dim_oth = c(1, 1, 1),
                             map_dim = 1:3,
                             map_pos = list(rep(1, 4), rep(1, 3), rep(1, 2)))
    iter <- iter_create_collapse(spec)
    for (i in 1:24) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(1L, iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when two dimensions collapsed from 'self'", {
    spec <- SpecIterCollapse(dim_self = 4:1,
                             dim_oth = 3:2,
                             map_dim = c(0, 1, 2, 0),
                             map_pos = list(rep(0, 4), 1:3, 1:2, 0))
    iter <- iter_create_collapse(spec)
    ans <- rep(1:6, each = 4)
    for (i in 1:24) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'self' and 'oth' both have single dimension", {
    spec <- SpecIterCollapse(dim_self = 4,
                             dim_oth = 2,
                             map_dim = 1,
                             map_pos = list(c(1, 0, 0, 2)))
    iter <- iter_create_collapse(spec)
    ans <- c(1L, 0L, 0L, 2L)
    for (i in 1:4) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when 'self' and 'oth' both have length 1", {
    spec <- SpecIterCollapse(dim_self = c(1, 1),
                             dim_oth = 1,
                             map_dim = c(1, 0),
                             map_pos = list(1, 0))
    iter <- iter_create_collapse(spec)
    expect_true(iter_has_next_collapse(iter))
    expect_identical(1L, iter_next_collapse(iter))
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works when two dimensions collapsing categories", {
    spec <- SpecIterCollapse(dim_self = c(3, 3),
                             dim_oth = c(2, 2),
                             map_dim = 1:2,
                             map_pos = list(c(1, 2, 2), c(1, 2, 2)))
    ans <- c(1, 2, 2,
             3, 4, 4,
             3, 4, 4)
    iter <- iter_create_collapse(spec)
    for (i in 1:9) {
        expect_true(iter_has_next_collapse(iter))
        expect_equal(ans[i], iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works 'oth' has an extra dimension at end", {
    spec <- SpecIterCollapse(dim_self = c(3, 3),
                             dim_oth = c(3, 3, 2),
                             map_dim = 1:2,
                             map_pos = list(1:3, 1:3))
    iter <- iter_create_collapse(spec)
    for (i in 1:9) {
        expect_true(iter_has_next_collapse(iter))
        expect_equal(c(i, i + 9L), iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works 'oth' has an extra dimension at beginning", {
    spec <- SpecIterCollapse(dim_self = c(3, 3),
                             dim_oth = c(2, 3, 3),
                             map_dim = 2:3,
                             map_pos = list(1:3, 1:3))
    iter <- iter_create_collapse(spec)
    for (i in seq(1, 17, 2)) {
        expect_true(iter_has_next_collapse(iter))
        expect_equal(c(i, i + 1L), iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works 'oth' has an extra dimension in the middle", {
    spec <- SpecIterCollapse(dim_self = c(3, 3),
                             dim_oth = c(3, 2, 3),
                             map_dim = c(1, 3),
                             map_pos = list(1:3, 1:3))
    iter <- iter_create_collapse(spec)
    ans_first <- c(1:3, 7:9, 13:15)
    for (i in 1:9) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(c(ans_first[i], ans_first[i] + 3L),
                         iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})

test_that("collapse iterator works 'oth' has an extra dimension in the middle and at the end", {
    spec <- SpecIterCollapse(dim_self = c(3, 3),
                             dim_oth = c(3, 2, 3, 2),
                             map_dim = c(1, 3),
                             map_pos = list(1:3, 1:3))
    iter <- iter_create_collapse(spec)
    ans_first <- c(1:3, 7:9, 13:15)
    for (i in 1:9) {
        expect_true(iter_has_next_collapse(iter))
        expect_identical(ans_first[i] + c(0L, 3L, 18L, 21L),
                         iter_next_collapse(iter))
    }
    expect_false(iter_has_next_collapse(iter))
})



