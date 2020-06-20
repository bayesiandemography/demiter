
context("helper-functions")

test_that("make_i_comp_type works with valid inputs", {
    make_i_comp_type <- demiter:::make_i_comp_type
    expect_identical(make_i_comp_type("increment"), 1L)
    expect_identical(make_i_comp_type("decrement"), 2L)
    expect_identical(make_i_comp_type("orig-dest"), 3L)
    expect_identical(make_i_comp_type("pool"), 4L)
    expect_error(make_i_comp_type("wrong"),
                 "invalid value for 'comp_type' : \"wrong\"")
})

test_that("make_offsets works with valid inputs", {
    make_offsets <- demiter:::make_offsets
    dim_oth <- c(4L, 2L, 3L)
    map_dim <- c(1L, 0L, 3L, 0L)
    ans_obtained <- make_offsets(dim_oth = dim_oth,
                                 map_dim = map_dim)
    ans_expected <- c(0L, 4L)
    expect_identical(ans_obtained, ans_expected)
    dim_oth <- c(4L, 2L, 3L)
    map_dim <- c(0L, 1L)
    ans_obtained <- make_offsets(dim_oth = dim_oth,
                                 map_dim = map_dim)
    ans_expected <- c(0L, 4L, 8L, 12L, 16L, 20L)
    expect_identical(ans_obtained, ans_expected)
    dim_oth <- c(4L, 2L, 3L)
    map_dim <- 1:3
    ans_obtained <- make_offsets(dim_oth = dim_oth,
                                 map_dim = map_dim)
    ans_expected <- 0L
    expect_identical(ans_obtained, ans_expected)
    dim_oth <- c(4L, 2L, 3L)
    map_dim <- 3L
    ans_obtained <- make_offsets(dim_oth = dim_oth,
                                 map_dim = map_dim)
    ans_expected <- 0:7
    expect_identical(ans_obtained, ans_expected)
    dim_oth <- c(4L, 2L, 3L)
    map_dim <- 2L
    ans_obtained <- make_offsets(dim_oth = dim_oth,
                                 map_dim = map_dim)
    ans_expected <- c(0:3, 8:11, 16:19)
    expect_identical(ans_obtained, ans_expected)
})

test_that("make_strides works with valid inputs", {
    make_strides <- demiter:::make_strides
    dim <- c(3, 4, 2)
    ans_obtained <- make_strides(dim)
    ans_expected <- c(1L, 3L, 12L)
    expect_identical(ans_obtained, ans_expected)
    dim <- c(1, 1, 2)
    ans_obtained <- make_strides(dim)
    ans_expected <- c(1L, 1L, 1L)
    expect_identical(ans_obtained, ans_expected)
    dim <- 10L
    ans_obtained <- make_strides(dim)
    ans_expected <- 1L
    expect_identical(ans_obtained, ans_expected)
    dim <- c(4L, 5L, 6L, 2L)
    ans_obtained <- make_strides(dim)
    ans_expected <- c(1L, 4L, 20L, 120L)
    expect_identical(ans_obtained, ans_expected)
})
