
## ## Assume that any 'age-of-mother' or 'parent' dimensions of
## ## births have been collapsed away.

## ## Assume that dimensions identical when found in both.

## #' Account iterators
## #'
## #' An increment iterator traverses array \code{self}
## #' giving the indices for associated arrays of (net) increments.
## #'
## #' Array \code{self} describes events, such as births,
## #' deaths, or migration, that affect the size and structure
## #' of a population. Array \code{self} must have a dimension with
## #' dimtype \code{"time"}, consisting of intervals of equal lengths.
## #' If \code{self} has a dimension with dimtype \code{"age"}, then
## #' that dimenions must consist of intervals with equal lengths,
## #' except for the last interval, which must be open. In addition,
## #' if \code{self} has an age dimension, then \code{self}
## #' must also have a dimension with dimtype \code{"triangle"}.
## #'
## #' If \code{self} does not have age and triangle dimensions
## #' then there is a single \code{oth} array. The \code{oth} array
## #' describes the effect of the component on population size
## #' at the end of each period.
## #' If \code{self} does have age and triangle dimensions, then there
## #' are two \code{oth} arrays. The first \code{oth} array describes the
## #' effect of the component on population at the end of each period,
## #' which is calculated from lower Lexis triangles. The second \code{oth}
## #' array records the effect of the component on accession to the
## #' next age group during each period, which is calculated from upper
## #' Lexis triangles.
## #'
## #' If \code{self} and \code{oth} share a dimension, then the two
## #' versions of the dimension must match exactly, in that they
## #' have exactly the same categories in exactly the same order.
## #' The one partial exception to this rule is time. If \code{oth}
## #' is population at the end of the period, then the time dimension
## #' for \code{oth} will consist of points, while the
## #' time dimension of \code{self} consists of intervals.
## #'
## #' If \code{self} is births, and has a dimension with dimtype
## #' \code{"age"}, then age refers to the parent,
## #' not to the child. However, births increase the number of children,
## #' not the number of people of child-bearing age. 
## #' The age dimension in \code{self} therefore does not
## #' correspond to the age dimension in the associated array
## #' of increments.
## #'
## #' The array \code{self} can belong to one of four types: increment, decrement,
## #' origin-destination, and pool. Arrays of births or immigrations
## #' are examples of increments. If \code{self} is an increment array,
## #' the value in each cell of \code{self} is added to the value of the
## #' associated cell in  \code{oth}. Arrays of deaths or emigrations
## #' are examples of decrements. If \code{self} is a decrement array,
## #' the value in each cell of \code{self} is subtracted from the
## #' value of the associated cell in \code{oth}.
## #' Origin-destination arrays and pool arrays are different ways of
## #' representing movements between regions or other statuses. The
## #' value in each cell of \code{self} is subtracted from one
## #' cell in \code{oth} and added to another cell.
## #'
## #' Function \code{iter_next_cohort} returns an integer vector of length
## #' three. The first element of this vector takes a value of \code{1}
## #' if \code{oth} represents population at the end of the period,
## #' and a value of \code{2} if \code{oth} represents accession
## #' during the period. The second element of the integer vector
## #' gives the index of the cell in \code{oth} whose value should
## #' be increased by the value of the cell in \code{self}. If
## #' contributes to increasing. If \code{self} is a decrement
## #' array, then the second element is \code{0}. The third element of the
## #' integer vector gives the index of the cell in \code{oth} whose value
## #' should be decreased by the value of the cell in \code{self}. If
## #' \code{self} is an increment array, then the third element is \code{0}.
## #'
## #' @param spec An object of class \code{\link{SpecIterIncrement}}.
## #' @param iter An increment iterator.
## #'
## #' @seealso \code{\link{SpecIterIncrement}}, \code{\link{cohort}},
## #' \code{\link{increment}}
## #'
## #' @examples
## #' spec <- SpecIterAccount(dim_self = c(3, 3),
## #'                           dim_oth = 3,
## #'                           map_dim = c(1, 0),
## #'                           comp_type_self = "orig-dest",
## #'                           indices_orig_self = 1,
## #'                           indices_dest_self = 1)
## #' iter <- iter_create_increment(spec)
## #' for (i in 1:9) {
## #'     print(iter_has_next_increment(iter))
## #'     print(iter_next_increment(iter))
## #' }
## #' iter_has_next_increment(iter)
## #' @name account
## NULL

## #' @rdname account
## #' @export
## iter_create_account <- function(spec) {
##     ans <- new.env(size = 11L)
##     ans$pos_self <- spec@pos_self
##     ans$dim_self <- spec@dim_self
##     ans$n_dim_self <- spec@n_dim_self
##     ans$i_time_self <- spec@i_time_self
##     ans$i_age_self <- spec@i_age_self
##     ans$strides_self <- spec@strides_self
##     ans$strides_init <- spec@strides_init
##     ans$strides_births <- spec@strides_births
##     ans$strides_lower_upper <- spec@strides_lower_upper
##     ans$has_next <- TRUE
##     ans$is_first <- TRUE
##     ans
## }

## #' @rdname account
## #' @export
## iter_next_account <- function(iter) {
##     pos_self <- iter$pos_self
##     dim_self <- iter$dim_self
##     n_dim_self <- iter$n_dim_self
##     i_time_self <- iter$i_time_self
##     i_age_self <- iter$i_afe_self
##     strides_self <- iter$strides_self
##     strides_init <- iter$strides_init
##     strides_births <- iter$strides_births
##     strides_lower_upper <- iter$strides_lower_upper
##     is_first <- iter$is_first
##     i_self_1 <- 0L
##     i_self_2 <- 0L
##     i_popn_init <- 0L
##     i_births <- 0L
##     i_incr_lower <- 0L
##     i_incr_upper_1 <- 0L
##     i_incr_upper_2 <- 0L
##     ## Step 1: Update position in 'self'
##     if (is_first) {
##         iter$is_first <- FALSE
##     }
##     else {
##         for (i_dim_self in seq_len(n_dim_self)) {
##             val_dim_self <- dim_self[[i_dim_self]]
##             val_pos_self <- pos_self[[i_dim_self]]
##             if (val_pos_self < val_dim_self) {
##                 pos_self[[i_dim_self]] <- val_pos_self + 1L
##                 break
##             }
##             else
##                 pos_self[[i_dim_self]] <- 1L
##         }
##     }
##     ## Step 2: Calculate indices 'self1', 'self2', 'init',
##     ## 'births', 'lower', 'upper1', 'upper2'
##     is_first_time <- pos_time == 1L
##     if (is_first_time) {
##         i_init <- 1L
##         for (i_dim_self in seq_len(n_dim_self)) {
##             is_dim_time <- i_dim_self == i_time_self
##             if (!is_dim_time) {
##                 val_pos_init <- pos_self[[i_dim_self]]
##                 passed_dim_time <- i_dim_self > i_tim_self
##                 i_dim_init <- i_dim_self - passed_dim_time
##                 stride_init  <- strides_init[[i_dim_init]]
##                 i_init <- i_init + (val_pos_init - 1L) * stride_init
##             }
##         }
##     }
##     else {
##         has_age <- i_age_self > 0L
##         if (has_age) {
##             pos_age <- pos_self[[i_age_self]]
##             is_youngest <- pos_age == 1L
##             is_oldest <- pos_age == n_age
##             i_self1 <- 1L
##             for (i_dim_self in seq_len(n_dim_self)) {
##                 val_pos_self <- pos_self[[i_dim_self]]
##                 is_dim_time_or_age <- (i_dim_self == i_time_self) || (i_dim_self == i_age_self)
##                 val_pos_self1 <- val_pos_self - is_dim_time_or_age
##                 stride_self1 <- strides_self[[i_dim_self]]
##                 i_self1 <- i_self1 + (val_pos_self1 - 1L) * stride_self1
##             }
##             i_lower <- 1L
##             for (i_dim_self in seq_len(n_dim_self)) {
##                 val_pos_self <- pos_self[[i_dim_self]]
##                 is_dim_time <- i_dim_self == i_time_self
##                 val_pos_lower <- val_pos_self - is_dim_time
##                 stride_lower <- strides_lower_upper[[i_dim_lower]]
##                 i_lower <- i_lower + (val_pos_self1 - 1L) * stride_lower_upper
##             }
##             if (is_youngest) {
##                 i_births <- 1L
##                 for (i_dim_self in seq_len(n_dim_self)) {
##                     is_dim_age <- i_dim_self == i_age_self
##                     if (!is_dim_age) {
##                         val_pos_self <- pos_self[[i_dim_self]]
##                         is_dim_time <- i_dim_self == i_time_self
##                         is_dim_age <- i_dim_self == i_age_self
##                         val_pos_births <- val_pos_self - is_dim_time
##                         i_dim_births <- i_dim_self - is_dim_age
##                         stride_births <- strides_births[[i_dim_births]]
##                         i_births <- i_births  + (val_pos_births - 1L) * stride_births
##                     }
##                 }
##             }
##             if (is_oldest) {
##                 i_self2 <- 1L
##                 for (i_dim_self in seq_len(n_dim_self)) {
##                     val_pos_self <- pos_self[[i_dim_self]]
##                     is_dim_time <- i_dim_self == i_time_self
##                     val_pos_self2 <- val_pos_self - is_dim_time
##                     stride_self2 <- strides_self[[i_dim_self]]
##                     i_self2 <- i_self2 + (val_pos_self2 - 1L) * stride_self2
##                 }
##                 i_upper2 <- i_lower
##             }
##         }
##         else {
##             i_self1 <- 1L
##             i_births <- 1L
##             for (i in seq_len(n_dim_self)) {
##                 val_pos_self <- pos_self[[i_dim_self]]
##                 is_dim_time <- i_dim_self == i_time_self
##                 val_pos_self1_births <- val_pos_self - is_dim_time
##                 stride_self1 <- strides_self[[i_dim_self]]
##                 stride_births <- strides_births[[i_dim_self]]
##                 i_self1 <- i_self1 + (val_pos_self1_births - 1L) * stride_self1
##                 i_births1 <- i_births1 + (val_pos_self1_births - 1L) * stride_births1
##             }
##         }
##     }
##     ## collect and return
##     i <- c(i_self1, i_self2, i_init, i_births, i_lower, i_upper1, i_upper2)
##     has_next <- any(pos_self < dim_self)
##     iter$pos_self <- pos_self
##     iter$has_next <- has_next
##     i
## }

## #' @rdname account
## #' @export
## iter_has_next_account <- function(iter) {
##     iter$has_next
## }
