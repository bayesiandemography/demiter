
## If 'self' has one or more origin-destination
## pairs of dimensions, then the origin destination
## in each pair maps on to 'oth', and the 'destination'
## dimension does not.

## Assume 'self' and 'oth' both have regular age-time strides.

## Assume 'self' and 'oth' do not have cohort dimensions.

## If 'self' is births, any 'age' refers to the mother,
## rather than the child, and does not count as
## an 'age' dimension - or a shared dimension at all
## by the iterator.

## 'self' has a triangle dimension iff it
## has an age dimension. 'oth' never has a triangle dimension.

## every cell in 'self' maps on to a cell in (one of the) 'oth'

## Last age group is open. (Age of mother in births does not count.)

#' Increment iterators
#'
#' An increment iterator traverses array \code{self}
#' giving the indices for associated arrays of (net) increments.
#'
#' Array \code{self} describes events, such as births,
#' deaths, or migration, that affect the size and structure
#' of a population. Array \code{self} must have a dimension with
#' dimtype \code{"time"}, consisting of intervals of equal lengths.
#' If \code{self} has a dimension with dimtype \code{"age"}, then
#' that dimenions must consist of intervals with equal lengths,
#' except for the last interval, which must be open. In addition,
#' if \code{self} has an age dimension, then \code{self}
#' must also have a dimension with dimtype \code{"triangle"}.
#'
#' If \code{self} does not have age and triangle dimensions
#' then there is a single \code{oth} array. The \code{oth} array
#' describes the effect of the component on population size
#' at the end of each period.
#' If \code{self} does have age and triangle dimensions, then there
#' are two \code{oth} arrays. The first \code{oth} array describes the
#' effect of the component on population at the end of each period,
#' which is calculated from lower Lexis triangles. The second \code{oth}
#' array records the effect of the component on accession to the
#' next age group during each period, which is calculated from upper
#' Lexis triangles.
#'
#' If \code{self} and \code{oth} share a dimension, then the two
#' versions of the dimension must match exactly, in that they
#' have exactly the same categories in exactly the same order.
#' The one partial exception to this rule is time. If \code{oth}
#' is population at the end of the period, then the time dimension
#' for \code{oth} will consist of points, while the
#' time dimension of \code{self} consists of intervals.
#'
#' If \code{self} is births, and has a dimension with dimtype
#' \code{"age"}, then age refers to the parent,
#' not to the child. However, births increase the number of children,
#' not the number of people of child-bearing age. 
#' The age dimension in \code{self} therefore does not
#' correspond to the age dimension in the associated array
#' of increments.
#'
#' The array \code{self} can belong to one of four types: increment, decrement,
#' origin-destination, and pool. Arrays of births or immigrations
#' are examples of increments. If \code{self} is an increment array,
#' the value in each cell of \code{self} is added to the value of the
#' associated cell in  \code{oth}. Arrays of deaths or emigrations
#' are examples of decrements. If \code{self} is a decrement array,
#' the value in each cell of \code{self} is subtracted from the
#' value of the associated cell in \code{oth}.
#' Origin-destination arrays and pool arrays are different ways of
#' representing movements between regions or other statuses. The
#' value in each cell of \code{self} is subtracted from one
#' cell in \code{oth} and added to another cell.
#'
#' Function \code{iter_next_cohort} returns an integer vector of length
#' three. The first element of this vector takes a value of \code{1}
#' if \code{oth} represents population at the end of the period,
#' and a value of \code{2} if \code{oth} represents accession
#' during the period. The second element of the integer vector
#' gives the index of the cell in \code{oth} whose value should
#' be increased by the value of the cell in \code{self}. If
#' contributes to increasing. If \code{self} is a decrement
#' array, then the second element is \code{0}. The third element of the
#' integer vector gives the index of the cell in \code{oth} whose value
#' should be decreased by the value of the cell in \code{self}. If
#' \code{self} is an increment array, then the third element is \code{0}.
#'
#' @param spec An object of class \code{\link{SpecIterIncrement}}.
#' @param iter An increment iterator.
#'
#' @seealso \code{\link{SpecIterIncrement}}, \code{\link{cohort}},
#' \code{\link{increment}}
#'
#' @examples
#' spec <- SpecIterIncrement(dim_self = c(3, 3),
#'                           dim_oth = 3,
#'                           map_dim = c(1, 0),
#'                           comp_type_self = "orig-dest",
#'                           indices_orig_self = 1,
#'                           indices_dest_self = 1)
#' iter <- iter_create_increment(spec)
#' for (i in 1:9) {
#'     print(iter_has_next_increment(iter))
#'     print(iter_next_increment(iter))
#' }
#' iter_has_next_increment(iter)
#' @name increment
NULL

#' @rdname increment
#' @export
iter_create_increment <- function(spec) {
    ans <- new.env(size = 16L)
    ans$pos_self <- spec@pos_self
    ans$pos_oth <- spec@pos_oth
    ans$dim_self <- spec@dim_self
    ans$n_dim_self <- spec@n_dim_self
    ans$n_dim_oth <- spec@n_dim_oth
    ans$map_dim <- spec@map_dim
    ans$strides_self <- spec@strides_self
    ans$strides_oth <- spec@strides_oth
    ans$i_triangle_self <- spec@i_triangle_self
    ans$i_comp_type_self <- spec@i_comp_type_self
    ans$indices_orig_self <- spec@indices_orig_self
    ans$indices_dest_self <- spec@indices_dest_self
    ans$n_orig_dest_self <- spec@n_orig_dest_self
    ans$i_direction_self <- spec@i_direction_self
    ans$has_next <- TRUE
    ans$is_first <- TRUE
    ans
}

#' @rdname increment
#' @export
iter_next_increment <- function(iter) {
    pos_self <- iter$pos_self
    pos_oth <- iter$pos_oth
    dim_self <- iter$dim_self
    n_dim_self <- iter$n_dim_self
    n_dim_oth <- iter$n_dim_oth
    map_dim <- iter$map_dim
    strides_self <- iter$strides_self
    strides_oth <- iter$strides_oth
    i_triangle_self <- iter$i_triangle_self
    i_comp_type_self <- iter$i_comp_type_self
    indices_orig_self <- iter$indices_orig_self
    indices_dest_self <- iter$indices_dest_self
    n_orig_dest_self <- iter$n_orig_dest_self
    i_direction_self <- iter$i_direction_self
    is_first <- iter$is_first
    incremented_self <- FALSE
    ## Step 1: Update position in 'self' and 'oth'
    if (is_first) {
        iter$is_first <- FALSE
    }
    else {
        for (i_dim_self in seq_len(n_dim_self)) {
            val_dim_self <- dim_self[[i_dim_self]]
            val_pos_self <- pos_self[[i_dim_self]]
            if (val_pos_self < val_dim_self) {
                val_pos_self  <- val_pos_self + 1L
                incremented_self <- TRUE
            }
            else
                val_pos_self <- 1L
            pos_self[[i_dim_self]] <- val_pos_self
            i_dim_oth <- map_dim[[i_dim_self]]
            has_dim_oth <- i_dim_oth > 0L
            if (has_dim_oth)
                pos_oth[[i_dim_oth]] <- val_pos_self ## dimensions identical
            if (incremented_self)
                break
        }
    }
    ## Step 2: Generate vector of length 3 containing indices of array,
    ## increment and decrement
    has_triangle <- i_triangle_self > 0L
    i_oth <- c(1L, 0L, 0L) ## 1 in first position signifies popn at end of period
    if (has_triangle) {
        is_upper <- pos_self[[i_triangle_self]] == 2L
        if (is_upper)
            i_oth[[1L]] <- 2L ## 2 in first position signifies attrition during period
    }
    i_oth_base <- 1L
    for (i_dim_oth in seq_len(n_dim_oth)) {
        val_strides_oth <- strides_oth[[i_dim_oth]]
        val_pos_oth <- pos_oth[[i_dim_oth]]
        i_oth_base <- i_oth_base + (val_pos_oth - 1L) * val_strides_oth
    }
    if (i_comp_type_self == 1L) # increment
        i_oth[[2L]] <- i_oth_base
    else if (i_comp_type_self == 2L) # decrement
        i_oth[[3]] <- i_oth_base
    else if (i_comp_type_self == 3L) { # orig-dest
        offset_dest <- 0L
        for (i_orig_dest in seq_len(n_orig_dest_self)) {
            i_orig_self <- indices_orig_self[[i_orig_dest]]
            i_dest_self <- indices_dest_self[[i_orig_dest]]
            stride_orig <- strides_self[[i_orig_self]]
            stride_dest <- strides_self[[i_dest_self]]
            pos_orig <- pos_self[[i_orig_self]]
            pos_dest <- pos_self[[i_dest_self]]
            offset_dest <- (offset_dest
                - (pos_orig - 1L) * stride_orig
                + (pos_dest - 1L) * stride_dest)
        }
        i_oth[[2L]] <- i_oth_base + offset_dest
        i_oth[[3L]] <- i_oth_base
    }
    else if (i_comp_type_self == 4L) { # pool
        is_in <- pos_self[[i_direction_self]] == 1L
        if (is_in)
            i_oth[[2L]] <- i_oth_base
        else
            i_oth[[3L]] <- i_oth_base
    }
    else
        stop(gettextf("invalid value for '%s' : %d",
                      "i_comp_type_self",
                      i_comp_type_self))
    ## Step 3: Update iterator and return vector
    has_next <- any(pos_self < dim_self)
    iter$pos_self <- pos_self
    iter$pos_oth <- pos_oth
    iter$has_next <- has_next
    i_oth
}

#' @rdname increment
#' @export
iter_has_next_increment <- function(iter) {
    iter$has_next
}
