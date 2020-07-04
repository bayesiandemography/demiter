
#' Account iterators
#'
#' An account iterator traverses an array of population counts,
#' returning the indices of arrays that contribute to each cell,
#' as specified by the demographic accounting equations.
#'
#' The array \code{self} of population counts must have a time
#' dimension with a length of at least two. If the array has an
#' age dimension, that dimension must also have a length of at least
#' two.
#' 
#' The arrays contributing to each cell are as follows:
#' \describe{
#'   \item{\code{self}}{The population array \code{self} itself.
#'     Except in the first period, values of \code{self} at
#'     time \code{t} depend on values of \code{self} at time \code{t-1}.
#'   }
#'   \item{\code{initial}}{An array of initial population counts.
#'     \code{initial} has the same dimensions as \code{self},
#'     except that it does not have a time dimension.
#'   }
#'   \item{\code{births}}{An array of birth counts. Any dimensions with
#'     dimtype \code{"age"} or \code{"parent"} have been collapsed
#'     away. The time dimension is one element shorter than
#'     the time dimension of \code{self}.
#'   }
#'   \item{\code{no_triangle}}{An array of increments
#'     with no age or triangle dimensions. \code{no_triangle}
#'     only occurs when \code{self} has no age dimension. It has
#'     the same dimensions as \code{self}, except that its
#'     time dimension is one element shorter.
#'   }
#'   \item{\code{lower_triangle}}{An array of increments
#'     formed from lower Lexis triangles. \code{lower_triangle}
#'     only occurs when \code{self} has an age dimension. It has
#'     the same dimensions as \code{self}, except that its
#'     time dimension is one element shorter.
#'   }
#'   \item{\code{upper_triangle}}{An array of increments
#'     formed from upper Lexis triangles. \code{upper_triangle}
#'     only occurs when \code{self} has an age dimension. It has
#'     the same dimensions as \code{self}, except that its
#'     time dimension is one element shorter.
#'   }
#' }
#'
#' If \code{self} has an age dimension, and if the current cell
#' belongs to the oldest age group, but not to the first time
#' point, then the cell receives contributions from two
#' cells in \code{self} and two cells in \code{upper_triangle}.
#'
#' Function \code{iter_next_account} returns an integer vector
#' of length 8. The vector contains indices to cells in the following
#' arrays:
#' \enumerate{
#'   \item \code{self}
#'   \item \code{self} (oldest age group only)
#'   \item \code{initial} (first time point only)
#'   \item \code{births}
#'   \item \code{no_triangle}
#'   \item \code{lower_triangle}
#'   \item \code{upper_triangle}
#'   \item \code{upper_triangle} (oldest age group only)
#' }
#' A zero in the integer vector implies that the current cell in
#' \code{self} does not receive any contribution from the associated
#' array. For any given cell, most entries in the vector are zero.
#'
#' Arrays \code{no_triangle}, \code{lower_triangle} and \code{upper_triangle}
#' are typically assembled using a \code{\link{increment}} iterator.
#'
#' In some applications, the demographic account may not have
#' births, or may not have increments or decrements, so
#' arrays \code{births}, \code{no_triangle}, \code{lower_triangle},
#' or \code{upper_triangle} may not exist. The account iterator
#' does not know or care about the existence of the arrays: it
#' returns indices anyway. It is the responsibility of the calling
#' function to decide what to do with these indices.
#'
#' @param spec An object of class \code{\link{SpecIterAccount}}.
#' @param iter An account iterator.
#'
#' @seealso \code{\link{SpecIterAccount}}, \code{\link{cohort}},
#' \code{\link{collapse}}, \code{\link{increment}}
#'
#' @examples
#' spec <- SpecIterAccount(dim = c(3, 3, 2, 4),
#'                         i_time = 4,
#'                         i_age = 2)
#' iter <- iter_create_account(spec)
#' for (i in 1:9) {
#'     print(iter_has_next_account(iter))
#'     print(iter_next_account(iter))
#' }
#' iter_has_next_account(iter)
#' @name account
NULL

#' @rdname account
#' @export
iter_create_account <- function(spec) {
    ans <- new.env(size = 12L)
    ans$pos_self <- spec@pos_self
    ans$dim_self <- spec@dim_self
    ans$n_dim_self <- spec@n_dim_self
    ans$i_time_self <- spec@i_time_self
    ans$i_age_self <- spec@i_age_self
    ans$n_age_self <- spec@n_age_self
    ans$strides_self <- spec@strides_self
    ans$strides_initial <- spec@strides_initial
    ans$strides_births <- spec@strides_births
    ans$strides_lower_upper <- spec@strides_lower_upper
    ans$has_next <- TRUE
    ans$is_first <- TRUE
    ans
}

#' @rdname account
#' @export
iter_next_account <- function(iter) {
    pos_self <- iter$pos_self
    dim_self <- iter$dim_self
    n_dim_self <- iter$n_dim_self
    i_time_self <- iter$i_time_self
    i_age_self <- iter$i_age_self
    n_age_self <- iter$n_age_self
    strides_self <- iter$strides_self
    strides_initial <- iter$strides_initial
    strides_births <- iter$strides_births
    strides_lower_upper <- iter$strides_lower_upper
    is_first <- iter$is_first
    i_self_1 <- 0L
    i_self_2 <- 0L
    i_initial <- 0L
    i_births <- 0L
    i_no_triangle <- 0L
    i_lower_triangle <- 0L
    i_upper_triangle_1 <- 0L
    i_upper_triangle_2 <- 0L
    ## Step 1: Update position in 'self'
    if (is_first) {
        iter$is_first <- FALSE
    }
    else {
        for (i_dim_self in seq_len(n_dim_self)) {
            val_pos_self <- pos_self[[i_dim_self]]
            val_dim_self <- dim_self[[i_dim_self]]
            if (val_pos_self < val_dim_self) {
                pos_self[[i_dim_self]] <- val_pos_self + 1L
                break
            }
            else
                pos_self[[i_dim_self]] <- 1L
        }
    }
    ## Step 2: Calculate indices 'self_1', 'self_2', 'initial', 'births',
    ## 'no_triangle', 'lower_triangle', 'upper_triangle_1', 'upper_triangle_2'
    pos_time <- pos_self[[i_time_self]]
    is_first_time_point <- pos_time == 1L
    if (is_first_time_point) {
        ## initial population - remove time dimension
        i_initial <- 1L
        for (i_dim_self in seq_len(n_dim_self)) {
            is_dim_time <- i_dim_self == i_time_self
            if (!is_dim_time) {
                val_pos_initial <- pos_self[[i_dim_self]]
                passed_dim_time <- i_dim_self > i_time_self
                i_dim_initial <- i_dim_self - passed_dim_time
                stride_initial  <- strides_initial[[i_dim_initial]]
                i_initial <- i_initial + (val_pos_initial - 1L) * stride_initial
            }
        }
    }
    else {
        has_age <- i_age_self > 0L
        if (has_age) {
            pos_age <- pos_self[[i_age_self]]
            is_youngest <- pos_age == 1L
            is_oldest <- pos_age == n_age_self
            ## previous population value for cohort - subtract 1 from age and time
            i_self_1 <- 1L
            for (i_dim_self in seq_len(n_dim_self)) {
                val_pos_self <- pos_self[[i_dim_self]]
                is_dim_time_or_age <- (i_dim_self == i_time_self) || (i_dim_self == i_age_self)
                val_pos_self_1 <- val_pos_self - is_dim_time_or_age
                stride_self_1 <- strides_self[[i_dim_self]]
                i_self_1 <- i_self_1 + (val_pos_self_1 - 1L) * stride_self_1
            }
            ## increments in lower triangle - subtract 1 from time
            i_lower_triangle <- 1L
            for (i_dim_self in seq_len(n_dim_self)) {
                val_pos_self <- pos_self[[i_dim_self]]
                is_dim_time <- i_dim_self == i_time_self
                val_pos_lower_triangle <- val_pos_self - is_dim_time
                stride_lower_upper <- strides_lower_upper[[i_dim_self]]
                i_lower_triangle <- (i_lower_triangle
                    + (val_pos_lower_triangle - 1L) * stride_lower_upper)
            }
            if (is_youngest) {
                ## births - ignore age and subtract 1 from time
                i_births <- 1L
                for (i_dim_self in seq_len(n_dim_self)) {
                    is_dim_age <- i_dim_self == i_age_self
                    if (!is_dim_age) {
                        val_pos_self <- pos_self[[i_dim_self]]
                        is_dim_time <- i_dim_self == i_time_self
                        val_pos_births <- val_pos_self - is_dim_time
                        passed_dim_age <- i_dim_self > i_age_self
                        i_dim_births <- i_dim_self - passed_dim_age
                        stride_births <- strides_births[[i_dim_births]]
                        i_births <- i_births  + (val_pos_births - 1L) * stride_births
                    }
                }
            }
            else {
                ## increments in upper triangle - subtract 1 from age and time
                stride_age_lower_upper <- strides_lower_upper[[i_age_self]]
                i_upper_triangle <- i_lower_triangle - stride_age_lower_upper
            }
            if (is_oldest) {
                ## previous population value for oldest age group - subtract 1 from time
                i_self_2 <- 1L
                for (i_dim_self in seq_len(n_dim_self)) {
                    val_pos_self <- pos_self[[i_dim_self]]
                    is_dim_time <- i_dim_self == i_time_self
                    val_pos_self_2 <- val_pos_self - is_dim_time
                    stride_self_2 <- strides_self[[i_dim_self]]
                    i_self_2 <- i_self_2 + (val_pos_self_2 - 1L) * stride_self_2
                }
                ## increments from upper triangle - same index as lower triangle
                i_upper_2 <- i_lower_triangle
            }
        }
        else {
            ## with no age, 'births' and 'no_triangle' have same dimensions,
            ## and both have same index as previous value of 'self'
            i_self_1 <- 1L
            i_births <- 1L
            for (i in seq_len(n_dim_self)) {
                val_pos_self <- pos_self[[i_dim_self]]
                is_dim_time <- i_dim_self == i_time_self
                val_pos_self_1 <- val_pos_self - is_dim_time
                stride_self_1 <- strides_self[[i_dim_self]]
                stride_births <- strides_births[[i_dim_self]]
                i_self_1 <- i_self_1 + (val_pos_self_1 - 1L) * stride_self_1
                i_births <- i_births + (val_pos_self_1 - 1L) * stride_births
            }
            i_no_triangle <- i_births
        }
    }
    ## Step 3: Collect and return
    i_vec <- c(i_self_1,
               i_self_2,
               i_initial,
               i_births,
               i_no_triangle,
               i_lower_triangle,
               i_upper_triangle_1,
               i_upper_triangle_2)
    has_next <- any(pos_self < dim_self)
    iter$pos_self <- pos_self
    iter$has_next <- has_next
    i_vec
}

#' @rdname account
#' @export
iter_has_next_account <- function(iter) {
    iter$has_next
}
