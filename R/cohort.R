
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


iter_create_cohort <- function(spec, i) {
    ## check inputs
    demcheck::err_is_class_obj(x = spec,
                               name = "spec",
                               class = "IterCohort")
    methods::validObject(spec)
    i <- demcheck::err_tdy_integer_scalar(x = i,
                                          name = "i",
                                          null_ok = FALSE)
    demcheck::err_positive_scalar(x = i,
                                  name = "i")
    demcheck::err_le_scalar(x1 = i,
                            x2 = as.integer(prod(spec@dim_self)),
                            name1 = "i",
                            name2 = "prod(dim_self)")
    ## extract data from 'spec'
    i_time <- spec@i_time_self
    i_age <- spec@i_age_self
    i_triangle <- spec@i_triangle_self
    strides <- spec@strides_self
    stop_at_oldest <- spec@stop_at_oldest
    offsets <- spec@offsets
    ## start creating iterator
    ans <- new.env(size = 13L)
    ## i
    ans$i <- i
    ## time
    stride_time <- strides[[i_time]]
    n_time <- dim[[i_time]]
    pos_time <- (((i - 1L) %/% stride_time) %% n_time) + 1L
    ans$stride_time <- stride_time
    ans$n_time <- n_time
    ans$pos_time <- pos_time
    ## age
    has_age <- i_age > 0L
    ans$has_age <- has_age
    if (has_age) {
        stride_age <- strides[[i_age]]
        n_age <- dim[[i_age]]
        pos_age <- (((i - 1L) %/% stride_age) %% n_age) + 1L
        ans$stride_age <- stride_age
        ans$n_age <- n_age
        ans$pos_age <- pos_age        
        ans$stop_at_oldest <- spec@stop_at_oldest
    }
    ## triangle
    has_triangle <- i_triangle > 0L
    ans$has_triangle <- has_triangle
    if (has_triangle) {
        stride_triangle <- strides[[i_triangle]]
        pos_triangle <- (((i - 1L) %/% stride_triangle) %% 2L) + 1L
        ans$stride_triangle <- stride_triangle
        ans$pos_triangle <- pos_triangle
    }
    ## offsets
    ans$offsets <- spec@offsets
    ## return
    ans
}


iter_next_cohort <- function(iter) {
    i <- iter$i
    n_time <- iter$n_time
    stride_time <- iter$stride_time
    pos_time <- iter$pos_time
    has_age <- iter$has_age
    offsets <- iter$offsets
    ## ------------------------------------------------------------------------
    ## Step 1: Update position
    ## ------------------------------------------------------------------------
    if (has_age) {
        n_age <- iter$n_age
        stride_age <- iter$stride_age
        pos_age <- iter$pos_age
        stop_at_oldest <- iter$stop_at_oldest
        is_oldest  <- pos_age == n_age
        has_triangle <- iter$has_triangle
        if (has_triangle) {
            stride_triangle <- iter$stride_triangle
            pos_triangle <- iter$pos_triangle
            is_lower <- pos_triangle == 1L
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
                }
            }
            else {
                ## Case 2: Has age, has triangles, not in oldest age group.
                if (is_lower) {
                    pos_time <- pos_time + 1L
                    i <- i + stride_time
                    pos_triangle <- 2L
                    i <- i + stride_triangle
                }
                else {
                    pos_age <- pos_age + 1L
                    i <- i + stride_age
                    pos_triangle <- 1L
                    i <- i - stride_triangle
                }
            }
            ans$pos_triangle <- pos_triangle
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
            }
        }
        ans$pos_age <- pos_age
    }
    else {
        ## Case 5: No age
        pos_time <- pos_time + 1L
        i <- i + stride_time
    }
    iter$pos_time <- pos_time
    ## ------------------------------------------------------------------------
    ## Step 2: Update iterator and return vector
    ## ------------------------------------------------------------------------
    is_last <- pos_time == n_time
    if (has_age && stop_at_oldest && is_oldest)
        has_next <- FALSE
    else if (has_triangle && !is_lower && !is_oldest)
        has_next <- TRUE
    else
        has_next <- !is_last
    iter$has_next <- has_next
    i_vec <- i + offsets
    i_vec
}
        
iter_has_next_cohort <- function(iter) {
    iter$has_next
}


iter_nval_cohort <- function(iter) {
    iter$n_offsets
}
