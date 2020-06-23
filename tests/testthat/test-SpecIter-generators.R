
context("SpecIter-generators")

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

