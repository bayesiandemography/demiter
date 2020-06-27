
## Increment along a 'cohort' as time passes.
## A 'cohort' is defined, for the purposes of
## this iterator, as a group that shares
## a group of characteristics, including
## non-fixed characteristics (eg region)
## and possibly (though not necessarily)
## birthdate and hence, at any given time, age.

## 'self' has a time dimension.

## 'self' may have an age dimension.

## 'self' may have a triangle dimension, but only
## if it has an age dimension

## 'self' does not have a cohort dimension.

## Assume that if 'stop_at_oldest' is TRUE, then
## oldest age group must be open - if oldest age group
## is closed, the we have to stop at oldest.
## Converse does not hold though, ie we may want to
## stop at oldest, even if the oldest age group is open.

#' Cohort iterators
#'
#' A cohort iterator traverses an array with a time
#' dimension, identifying the cell or cells that
#' a cohort occupies at each time.
#'
#' A cohort is defined, for the purposes of the iterator,
#' as a group that shares one or more characteristics,
#' such as region and sex. The cohort can occupy multiple cells
#' at a given time, eg multiple regions or multiple iterations.
#' The array being traversed can include an age dimension,
#' but does not necessarily do so.
#' The composition of the cohort can change over time
#' due to events such as migration or death.
#'
#' The iterator does not need to start at the earliest
#' time contained in the array, and typically does not.
#' The initial position of the iterator within the array
#' is controlled by the \code{i} argument to function
#' \code{iter_create_cohort}.
#'
#' \code{iter_create_cohort} creates a new
#' iterator based on the information contained in
#' a \code{\link{SpecIterCohort}} object, and the value
#' \code{i}. If the cohort' occupies a single
#' cell at each time, then \code{i}
#' is the index for the first cell that the cohort occupies.
#' If the cohort occupies multiple cells at a time,
#' then \code{i} is the index for the cell that,
#' among all the first cells, has the lowest index.
#' For instance, if the cohort initially occupies
#' cells \code{21}, \code{23}, and \code{25}, then
#' \code{i} would be \code{21}.
#'
#' \code{iter_next_cohort} moves the iterator
#' forward and returns indices for all cells now
#' occupied by the cohort.
#'
#' \code{iter_has_next_cohort} returns \code{TRUE}
#' if the cohort has additional positions it
#' can occupy within the array, and \code{FALSE}
#' otherwise.
#'
#' If the array does not have an Lexis triangle dimension,
#' then the iterator advances along the time dimension with
#' each call to \code{iter_next_cohort}. If the array does have
#' a Lexis triangle dimension, then it advances along the
#' time dimension only when entering an "upper" Lexis triangle.
#'
#' If an array has an age dimension, the cohort's behaviour
#' when it reachs the oldest age group depends on the value for 
#' \code{stop_at_oldest} that was supplied when the
#' \code{\link{SpecIterCohort}} object was created.
#' If \code{stop_at_oldest} is \code{TRUE}, then the
#' cohort will stop at this point. If \code{stop_at_oldest}
#' is \code{FALSE}, then the cohort
#' will remain in the oldest age group, but will continue advancing
#' along the time dimension. 
#' 
#' @param spec An object of class \code{\link{SpecIterCohort}}.
#' @param i The index of a cell in the array.
#' @param iter A cohort iterator.
#'
#' @seealso \code{\link{SpecIterCohort}}, \code{\link{collapse}},
#' \code{\link{increment}}.
#'
#' @examples
#' ## No age, cohort occupies one cell
#' spec <- SpecIterCohort(dim = c(2, 3),
#'                        i_time = 2)
#' iter <- iter_create_cohort(spec = spec,
#'                            i = 2)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#' 
#' ## With age, cohort occupies one cell
#' spec <- SpecIterCohort(dim = c(3, 5),
#'                        i_age = 1,
#'                        i_time = 2,
#'                        stop_at_oldest = FALSE)
#' iter <- iter_create_cohort(spec = spec,
#'                            i = 5)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#'
#' ## No age, cohort occupies two cells
#' spec <- SpecIterCohort(dim = c(2, 3),
#'                       i_time = 2,
#'                       offset = c(0, 1))
#' iter <- iter_create_cohort(spec = spec,
#'                            i = 3)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#' iter_next_cohort(iter)
#' iter_has_next_cohort(iter)
#' @name cohort
NULL

## HAS_TESTS
#' @rdname cohort
#' @export
iter_create_cohort <- function(spec, i) {
    demcheck::err_is_class_obj(x = spec,
                               name = "spec",
                               class = "SpecIterCohort")
    i <- demcheck::err_tdy_positive_integer_scalar(x = i,
                                                   name = "i",
                                                   null_ok = FALSE)
    n_time <- spec@n_time
    n_age <- spec@n_age
    stride_time <- spec@stride_time
    stride_age <- spec@stride_age
    stride_triangle <- spec@stride_triangle
    pos_time <- (((i - 1L) %/% stride_time) %% n_time) + 1L
    if (stride_age > 0L)
        pos_age <- (((i - 1L) %/% stride_age) %% n_age) + 1L
    else
        pos_age <- 0L
    if (stride_triangle > 0L)
        pos_triangle <- (((i - 1L) %/% stride_triangle) %% 2L) + 1L
    else
        pos_triangle <- 0L
    ans <- new.env(size = 14L)
    ans$i <- i
    ans$pos_time <- pos_time
    ans$pos_age <- pos_age
    ans$pos_triangle <- pos_triangle
    ans$n_time <- n_time
    ans$n_age <- n_age
    ans$stride_time <- stride_time
    ans$stride_age <- stride_age
    ans$stride_triangle <- stride_triangle
    ans$stop_at_oldest <- spec@stop_at_oldest
    ans$offsets <- spec@offsets
    ans$n_offsets <- spec@n_offsets
    ans$has_next <- TRUE
    ans$is_first <- TRUE
    ans
}

## HAS_TESTS
#' @rdname cohort
#' @export
iter_next_cohort <- function(iter) {
    i <- iter$i
    pos_time <- iter$pos_time
    pos_age <- iter$pos_age
    pos_triangle <- iter$pos_triangle
    n_time <- iter$n_time
    n_age <- iter$n_age
    stride_time <- iter$stride_time
    stride_age <- iter$stride_age
    stride_triangle <- iter$stride_triangle
    stop_at_oldest <- iter$stop_at_oldest
    offsets <- iter$offsets
    is_first <- iter$is_first
    has_age <- stride_age > 0L
    has_triangle <- stride_triangle > 0L
    is_oldest <- has_age && (pos_age == n_age)
    is_lower <- has_triangle && pos_triangle == 1L
    ## Step 1: Except when 'first' is TRUE, update position
    if (is_first) {
        iter$is_first <- FALSE
    }
    else {
        if (has_age) {
            if (has_triangle) {
                if (is_oldest) {
                    ## Case 1: Has age, has triangles, in oldest age group.
                    ## If already in the oldest age group, and still going,
                    ## then 'stop_at_oldest' must be FALSE
                    pos_time <- pos_time + 1L
                    i <- i + stride_time
                    ## if currently in lower triangle, advance
                    ## to upper triangle; if currently in upper
                    ## triangle, stay in upper triangle
                    if (is_lower) {
                        pos_triangle <- 2L
                        i <- i + stride_triangle
                        is_lower <- FALSE
                    }
                }
                else {
                    ## Case 2: Has age, has triangles, not in oldest age group.
                    if (is_lower) {
                        pos_time <- pos_time + 1L
                        i <- i + stride_time
                        pos_triangle <- 2L
                        i <- i + stride_triangle
                        is_lower <- FALSE
                    }
                    else {
                        pos_age <- pos_age + 1L
                        i <- i + stride_age
                        is_oldest <- pos_age == n_age
                        pos_triangle <- 1L
                        i <- i - stride_triangle
                        is_lower <- TRUE
                    }
                }
            }
            else {
                if (is_oldest) {
                    ## Case 3: Has age, no triangles, in oldest age group
                    ## If already in the oldest age group, and still going,
                    ## then 'stop_at_oldest' must be FALSE
                    pos_time <- pos_time + 1L
                    i <- i + stride_time
                }
                else {
                    ## Case 4: Has age, no triangles, not in oldest age group
                    pos_time <- pos_time + 1L
                    i <- i + stride_time
                    pos_age <- pos_age + 1L
                    i <- i + stride_age
                    is_oldest <- pos_age == n_age
                }
            }
        }
        else {
            ## Case 5: No age
            pos_time <- pos_time + 1L
            i <- i + stride_time
        }
        iter$i <- i
        iter$pos_time <- pos_time
        iter$pos_age <- pos_age
        iter$pos_triangle <- pos_triangle
    }
    ## Step 2: Derive value for 'has_next'
    is_last <- pos_time == n_time
    if (has_age && stop_at_oldest && is_oldest)
        has_next <- FALSE
    else if (has_triangle && !is_lower && !is_oldest)
        has_next <- TRUE
    else
        has_next <- !is_last
    iter$has_next <- has_next
    ## Return result
    i_vec <- i + offsets
    i_vec
}
        
## HAS_TESTS
#' @rdname cohort
#' @export
iter_has_next_cohort <- function(iter) {
    iter$has_next
}
