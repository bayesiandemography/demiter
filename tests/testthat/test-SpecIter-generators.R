
context("SpecIter-generators")

## SpecIterCohort -------------------------------------------------------------

test_that("SpecIterCohort creates valid SpecIterCohort object with no age", {
    ans <- SpecIterCohort(dim = 4:2,
                          i_time = 3,
                          offsets = 0L)
    expect_is(ans, "SpecIterCohort")
    expect_true(validObject(ans))
    expect_identical(ans@n_age, 0L)
    expect_identical(ans@stride_triangle, 0L)
    expect_identical(ans@stop_at_oldest, NA)
})

test_that("SpecIterCohort creates valid SpecIterCohort object with age but not triangle", {
    ans <- SpecIterCohort(dim = 4:2,
                          i_time = 3,
                          i_age = 1,
                          stop_at_oldest = FALSE,
                          offsets = c(0L, 24L))
    expect_is(ans, "SpecIterCohort")
    expect_true(validObject(ans))
    expect_identical(ans@n_age, 4L)
    expect_identical(ans@stride_triangle, 0L)
    expect_false(ans@stop_at_oldest)
})

test_that("SpecIterCohort creates valid SpecIterCohort object with age and triangle", {
    ans <- SpecIterCohort(dim = 4:2,
                          i_time = 1,
                          i_age = 2,
                          i_triangle = 3,
                          stop_at_oldest = TRUE,
                          offsets = c(0L, 24L))
    expect_is(ans, "SpecIterCohort")
    expect_true(validObject(ans))
    expect_identical(ans@n_time, 4L)
    expect_identical(ans@n_age, 3L)
    expect_identical(ans@stride_triangle, 12L)
    expect_true(ans@stop_at_oldest)
})


## SpecIterCollapse -----------------------------------------------------------

test_that("SpecIterCollapse creates valid SpecIterCollapse object when 'self' and 'oth' identical", {
    ans <- SpecIterCollapse(dim_self = 4:2,
                            dim_oth = 4:2,
                            map_dim = 1:3,
                            map_pos = list(1:4, 1:3, 1:2))
    expect_is(ans, "SpecIterCollapse")
    expect_true(validObject(ans))
})

test_that("SpecIterCollapse creates valid SpecIterCollapse object when dimensions permuted", {
    ans <- SpecIterCollapse(dim_self = 4:2,
                            dim_oth = 2:4,
                            map_dim = 3:1,
                            map_pos = list(1:4, 1:3, 1:2))
    expect_is(ans, "SpecIterCollapse")
    expect_true(validObject(ans))
})

test_that("SpecIterCollapse creates valid SpecIterCollapse object when dimension collapsed", {
    ans <- SpecIterCollapse(dim_self = 4:2,
                            dim_oth = 3:2,
                            map_dim = c(0, 1, 2),
                            map_pos = list(c(0, 0, 0, 0), 1:3, 1:2))
    expect_is(ans, "SpecIterCollapse")
    expect_true(validObject(ans))
})

test_that("SpecIterCollapse creates valid SpecIterCollapse object when dimension collapsed", {
    ans <- SpecIterCollapse(dim_self = 4:2,
                            dim_oth = 3:2,
                            map_dim = c(0, 1, 2),
                            map_pos = list(c(0, 0, 0, 0), 1:3, 1:2))
    expect_is(ans, "SpecIterCollapse")
    expect_true(validObject(ans))
})

test_that("SpecIterCollapse creates valid SpecIterCollapse object when dimension subsetted", {
    ans <- SpecIterCollapse(dim_self = 4:2,
                            dim_oth = c(3, 3, 2),
                            map_dim = 1:3,
                            map_pos = list(c(1, 0, 2, 3), 1:3, 1:2))
    expect_is(ans, "SpecIterCollapse")
    expect_true(validObject(ans))
})

test_that("SpecIterCollapse creates valid SpecIterCollapse object when dimensions subsetted and collapsed", {
    ans <- SpecIterCollapse(dim_self = 4:2,
                            dim_oth = c(3, 2),
                            map_dim = c(1, 0, 2),
                            map_pos = list(c(1, 0, 2, 3), c(0, 0, 0), 1:2))
    expect_is(ans, "SpecIterCollapse")
    expect_true(validObject(ans))
})

test_that("SpecIterCollapse creates valid SpecIterCollapse object when collapsed to one cell", {
    ans <- SpecIterCollapse(dim_self = 4:2,
                            dim_oth = 1,
                            map_dim = c(0, 0, 1),
                            map_pos = list(c(0, 0, 0, 0), c(0, 0, 0), c(1, 1)))
    expect_is(ans, "SpecIterCollapse")
    expect_true(validObject(ans))
})


## SpecIterIncrement ----------------------------------------------------------

test_that("SpecIterIncrement creates valid SpecIterIncrement object when 'self' is increment", {
    ans <- SpecIterIncrement(dim_self = 4:2,
                             dim_oth = 4:2,
                             map_dim = 1:3,
                             comp_type_self = "increment")
    expect_is(ans, "SpecIterIncrement")
    expect_true(validObject(ans))
    expect_identical(ans@i_comp_type_self, 1L)
})

test_that("SpecIterIncrement creates valid SpecIterIncrement object when 'self' is births", {
    ans <- SpecIterIncrement(dim_self = 4:2,
                             dim_oth = 3:2,
                             map_dim = 0:2,
                             comp_type_self = "increment")
    expect_is(ans, "SpecIterIncrement")
    expect_true(validObject(ans))
    expect_identical(ans@i_comp_type_self, 1L)
})

test_that("SpecIterIncrement creates valid SpecIterIncrement object when 'self' is decrement", {
    ans <- SpecIterIncrement(dim_self = 4:2,
                             dim_oth = 4:2,
                             map_dim = 1:3,
                             comp_type_self = "decrement")
    expect_is(ans, "SpecIterIncrement")
    expect_true(validObject(ans))
    expect_identical(ans@i_comp_type_self, 2L)
})

test_that("SpecIterIncrement creates valid SpecIterIncrement object when 'self' is orig-dest", {
    ans <- SpecIterIncrement(dim_self = c(2, 5, 5, 3),
                             dim_oth = c(2, 5, 3),
                             map_dim = c(1, 0, 2, 3),
                             comp_type_self = "orig-dest",
                             indices_orig_self = 2,
                             indices_dest_self = 3)
    expect_is(ans, "SpecIterIncrement")
    expect_true(validObject(ans))
    expect_identical(ans@i_comp_type_self, 3L)
})

test_that("SpecIterIncrement creates valid SpecIterIncrement object when 'self' is orig-dest", {
    ans <- SpecIterIncrement(dim_self = c(2, 5, 5, 3, 4, 4),
                             dim_oth = c(2, 5, 3, 4),
                             map_dim = c(1, 0, 2, 3, 0, 4),
                             comp_type_self = "orig-dest",
                             indices_orig_self = c(2, 5),
                             indices_dest_self = c(3, 6))
    expect_is(ans, "SpecIterIncrement")
    expect_true(validObject(ans))
    expect_identical(ans@i_comp_type_self, 3L)
})

test_that("SpecIterIncrement creates valid SpecIterIncrement object when 'self' is pool", {
    ans <- SpecIterIncrement(dim_self = 4:2,
                             dim_oth = 4:3,
                             map_dim = c(1, 2, 0),
                             comp_type_self = "pool",
                             i_direction = 3)
    expect_is(ans, "SpecIterIncrement")
    expect_true(validObject(ans))
    expect_identical(ans@i_comp_type_self, 4L)
})





