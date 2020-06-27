
context("cohort")

test_that("cohort iterator works on array with single dimension", {
    spec <- SpecIterCohort(dim = 4,
                           i_time = 1)
    iter <- iter_create_cohort(spec = spec,
                               i = 1)
    for (i in 1:4) {
        expect_true(iter_has_next_cohort(iter))
        expect_identical(i, iter_next_cohort(iter))
    }
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with multiple dimensions but no age", {
    spec <- SpecIterCohort(dim = 4:7,
                           i_time = 2)
    iter <- iter_create_cohort(spec = spec,
                               i = 27)
    for (i in 1:4) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(23 + i * 4, iter_next_cohort(iter))
    }
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with time dimension of length 1", {
    spec <- SpecIterCohort(dim = c(3, 10, 1),
                           i_time = 3)
    iter <- iter_create_cohort(spec = spec,
                               i = 12)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(12L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with just age and time, not reaching oldest age group", {
    spec <- SpecIterCohort(dim = c(10, 3),
                           i_time = 2,
                           i_age = 1,
                           stop_at_oldest = TRUE)
    iter <- iter_create_cohort(spec = spec,
                               i = 2)
    for (i in 0:2) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(2 + i * 11, iter_next_cohort(iter))
    }
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with just age and time, reaching oldest age group, stop_at_oldest is FALSE", {
    spec <- SpecIterCohort(dim = c(4, 10),
                           i_time = 2,
                           i_age = 1,
                           stop_at_oldest = TRUE)
    iter <- iter_create_cohort(spec = spec,
                               i = 14)
    for (i in 0:2) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(14 + i * 5, iter_next_cohort(iter))
    }
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with just age and time, reaching oldest age group, stop_at_oldest is FALSE", {
    spec <- SpecIterCohort(dim = c(4, 10),
                           i_time = 2,
                           i_age = 1,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 14)
    for (i in 0:2) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(14 + i * 5, iter_next_cohort(iter))
    }
    for (i in 0:3) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(28 + i * 4, iter_next_cohort(iter))
    }        
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with just age and time, reaching oldest age group, stop_at_oldest is FALSE, time is first dimension", {
    spec <- SpecIterCohort(dim = c(4, 10),
                           i_time = 1,
                           i_age = 2,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 33)
    for (i in 0:1) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(33 + i * 5, iter_next_cohort(iter))
    }
    for (i in 0:1) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(39 + i, iter_next_cohort(iter))
    }        
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, sex, and time, reaching oldest age group, stop_at_oldest is FALSE", {
    spec <- SpecIterCohort(dim = c(4, 2, 10),
                           i_time = 3,
                           i_age = 1,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 30)
    for (i in 0:2) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(30 + i * 9, iter_next_cohort(iter))
    }
    for (i in 0:3) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(56 + i * 8, iter_next_cohort(iter))
    }        
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, sex, and time, reaching oldest age group, stop_at_oldest is TRUE", {
    spec <- SpecIterCohort(dim = c(4, 2, 10),
                           i_time = 3,
                           i_age = 1,
                           stop_at_oldest = TRUE)
    iter <- iter_create_cohort(spec = spec,
                               i = 30)
    for (i in 0:2) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(30 + i * 9, iter_next_cohort(iter))
    }
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, sex, and time, starting at oldest age group, stop_at_oldest is FALSE", {
    spec <- SpecIterCohort(dim = c(4, 2, 10),
                           i_time = 3,
                           i_age = 1,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 16)
    for (i in 0:8) {
        expect_true(iter_has_next_cohort(iter))
        expect_equal(16 + 8 * i, iter_next_cohort(iter))
    }
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, sex, and time, starting at oldest age group, stop_at_oldest is TRUE", {
    spec <- SpecIterCohort(dim = c(4, 2, 10),
                           i_time = 3,
                           i_age = 1,
                           stop_at_oldest = TRUE)
    iter <- iter_create_cohort(spec = spec,
                               i = 16)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(16L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, triangle, and time, not reaching oldest age group", {
    spec <- SpecIterCohort(dim = c(10, 2, 3),
                           i_age = 1,
                           i_triangle = 2,
                           i_time = 3,
                           stop_at_oldest = TRUE)
    iter <- iter_create_cohort(spec = spec,
                               i = 3)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(3L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(33L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(24L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(54L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(45L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, triangle, and time, not reaching oldest age, start in final period, upper", {
    spec <- SpecIterCohort(dim = c(10, 2, 3),
                           i_age = 1,
                           i_triangle = 2,
                           i_time = 3,
                           stop_at_oldest = TRUE)
    iter <- iter_create_cohort(spec = spec,
                               i = 57)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(57L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(48L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, triangle, and time, not reaching oldest age, start in final period, lower", {
    spec <- SpecIterCohort(dim = c(10, 2, 3),
                           i_age = 1,
                           i_triangle = 2,
                           i_time = 3,
                           stop_at_oldest = TRUE)
    iter <- iter_create_cohort(spec = spec,
                               i = 43)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(43L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, triangle, and time, reaching oldest age, stop_at_oldest is TRUE", {
    spec <- SpecIterCohort(dim = c(3, 2, 10),
                           i_age = 1,
                           i_triangle = 2,
                           i_time = 3,
                           stop_at_oldest = TRUE)
    iter <- iter_create_cohort(spec = spec,
                               i = 17)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(17L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(15L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, triangle, and time, reaching oldest age, stop_at_oldest is FALSE", {
    spec <- SpecIterCohort(dim = c(3, 2, 10),
                           i_age = 1,
                           i_triangle = 2,
                           i_time = 3,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 17)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(17L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(15L, iter_next_cohort(iter))
    for (i in 0:6) {
        expect_true(iter_has_next_cohort(iter))
        expect_identical(24L + i * 6L, iter_next_cohort(iter))
    } 
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator works on array with age, sex, time, and triangle", {
    spec <- SpecIterCohort(dim = c(3, 2, 4, 2),
                           i_age = 1,
                           i_time = 3,
                           i_triangle = 4,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 14)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(14L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(44L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(21L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator stops with oldest upper triangle in final period", {
    spec <- SpecIterCohort(dim = c(3, 2, 4, 2),
                           i_age = 1,
                           i_time = 3,
                           i_triangle = 4,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 45)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(45L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator does not stop with oldest upper triangle in second-to-last period", {
    spec <- SpecIterCohort(dim = c(3, 2, 4, 2),
                           i_age = 1,
                           i_time = 3,
                           i_triangle = 4,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 39)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(39L, iter_next_cohort(iter))
    expect_true(iter_has_next_cohort(iter))
    expect_identical(45L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})

test_that("cohort iterator stops with lower triangle in final period", {
    spec <- SpecIterCohort(dim = c(3, 2, 4, 2),
                           i_age = 1,
                           i_time = 3,
                           i_triangle = 4,
                           stop_at_oldest = FALSE)
    iter <- iter_create_cohort(spec = spec,
                               i = 19)
    expect_true(iter_has_next_cohort(iter))
    expect_identical(19L, iter_next_cohort(iter))
    expect_false(iter_has_next_cohort(iter))
})













