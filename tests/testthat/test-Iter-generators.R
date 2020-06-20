
context("Iter-generators")

test_that("IterCohort creates valid IterCohort object with no age", {
    ans <- IterCohort(dim = 4:2,
                      i_time = 3,
                      offsets = 0L)
    expect_is(ans, "IterCohort")
    expect_true(validObject(ans))
    expect_identical(ans@i_age, 0L)
    expect_identical(ans@i_triangle, 0L)
    expect_identical(ans@stop_at_oldest, NA)
})

test_that("IterCohort creates valid IterCohort object with age but not triangle", {
    ans <- IterCohort(dim = 4:2,
                      i_time = 3,
                      i_age = 1,
                      stop_at_oldest = FALSE,
                      offsets = c(0L, 24L))
    expect_is(ans, "IterCohort")
    expect_true(validObject(ans))
    expect_identical(ans@i_age, 1L)
    expect_identical(ans@i_triangle, 0L)
    expect_false(ans@stop_at_oldest)
})

test_that("IterCohort creates valid IterCohort object with age and triangle", {
    ans <- IterCohort(dim = 4:2,
                      i_time = 1,
                      i_age = 2,
                      i_triangle = 3,
                      stop_at_oldest = TRUE,
                      offsets = c(0L, 24L))
    expect_is(ans, "IterCohort")
    expect_true(validObject(ans))
    expect_identical(ans@i_time, 1L)
    expect_identical(ans@i_age, 2L)
    expect_identical(ans@i_triangle, 3L)
    expect_true(ans@stop_at_oldest)
})

