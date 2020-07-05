
context("account")

test_that("account iterator works on array with single dimension", {
    spec <- SpecIterAccount(dim = 4,
                            i_time = 1)
    iter <- iter_create_account(spec)
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 1L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(1L, 0L, 0L, 1L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(2L, 0L, 0L, 2L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(3L, 0L, 0L, 3L, 0L, 0L, 0L))
    expect_false(iter_has_next_account(iter))
})

test_that("account iterator works on array with two dimensions, time first, not including age", {
    spec <- SpecIterAccount(dim = 3:2,
                            i_time = 1)
    iter <- iter_create_account(spec)
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 1L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(1L, 0L, 0L, 1L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(2L, 0L, 0L, 2L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 2L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(4L, 0L, 0L, 3L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(5L, 0L, 0L, 4L, 0L, 0L, 0L))
    expect_false(iter_has_next_account(iter))
})

test_that("account iterator works on array with two dimensions, time second, not including age", {
    spec <- SpecIterAccount(dim = 3:2,
                            i_time = 2)
    iter <- iter_create_account(spec)
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 1L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 2L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 3L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(1L, 0L, 0L, 1L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(2L, 0L, 0L, 2L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(3L, 0L, 0L, 3L, 0L, 0L, 0L))
    expect_false(iter_has_next_account(iter))
})

test_that("account iterator works on array with three dimensions, time third, not including age", {
    spec <- SpecIterAccount(dim = c(2, 2, 2),
                            i_time = 3)
    iter <- iter_create_account(spec)
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 1L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 2L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 3L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 4L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(1L, 0L, 0L, 1L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(2L, 0L, 0L, 2L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(3L, 0L, 0L, 3L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(4L, 0L, 0L, 4L, 0L, 0L, 0L))
    expect_false(iter_has_next_account(iter))
})

test_that("account iterator works on array with two dimensions, time first, including age", {
    spec <- SpecIterAccount(dim = c(2, 2),
                            i_time = 1,
                            i_age = 2)
    iter <- iter_create_account(spec)
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 1L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 0L, 0L, 1L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 2L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(1L, 3L, 0L, 0L, 2L, 1L, 2L))
    expect_false(iter_has_next_account(iter))
})

test_that("account iterator works on array with two dimensions, time second, including age", {
    spec <- SpecIterAccount(dim = c(2, 2),
                            i_time = 2,
                            i_age = 1)
    iter <- iter_create_account(spec)
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 1L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 2L, 0L, 0L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(0L, 0L, 0L, 0L, 1L, 0L, 0L))
    expect_true(iter_has_next_account(iter))
    expect_identical(iter_next_account(iter), c(1L, 2L, 0L, 0L, 2L, 1L, 2L))
    expect_false(iter_has_next_account(iter))
})






