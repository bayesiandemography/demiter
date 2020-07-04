
#' Increment iterators
#'
#' An increment iterator traverses array \code{self}
#' giving the indices for an associated array, or pair of arrays,
#' of (net) increments. Every cell in \code{self} maps on
#' to a single cell in the associated array(s). Every cell
#' in the associated array(s) maps on to one or more cells
#' in \code{self}.
#' 
#' Array \code{self} describes events, such as births,
#' deaths, or migration, that affect the size and structure
#' of a population. Array \code{self} must have a time dimension.
#' It may have an age dimension. If it does, it must also have a
#' triangle dimension. It may not have have cohort dimension.
#'
#' Array \code{self} must have positive length. None of
#' its dimensions can have length zero.
#' 
#' If \code{self} has pairs of origin-destination dimensions
#' or parent-child dimensions, then the destination dimensions
#' or child dimensions are mapped into \code{oth}. The origin
#' diemnsions or parent dimensions are dropped. The rule
#' that parent dimensions are dropped extends to the age
#' dimension when \code{self} is births. Age in this case
#' refers to the parent, and so is not included in \code{oth}.
#'
#' If \code{self} does not have age and triangle dimensions,
#' then there is a single \code{oth} array holding net increments.
#' If \code{self} does have age and triangle dimensions,
#' then there are two \code{oth} arrays. The first \code{oth} array
#' holds increments derived from lower Lexis triangles, and the second
#' \code{oth} array holds increments derived from upper Lexis triangles.
#'
#' If \code{self} and \code{oth} share a dimension, then the two
#' versions of the dimension must match exactly, in that they
#' have exactly the same categories in exactly the same order.
#' The one partial exception to this rule for time. If \code{oth}
#' is population at the end of the period, then the time dimension
#' for \code{oth} will consist of points, while the
#' time dimension of \code{self} consists of intervals.
#'
#' The array \code{self} can belong to one of four types:
#' \code{"increment"}, \code{"decrement"},\code{"orig-dest"},
#' and \code{"pool"}. Arrays of births or immigrations
#' are examples of increments. If \code{self} is an increment array, then
#' the value in each cell of \code{self} is added to the value of the
#' associated cell in \code{oth}. Arrays of deaths or emigrations
#' are examples of decrements. If \code{self} is a decrement array,
#' then the value in each cell of \code{self} is subtracted from the
#' value of the associated cell in \code{oth}.
#' Orig-dest and pool arrays are different ways of
#' representing movements between regions or other statuses. The
#' value in each cell of \code{self} is subtracted from one
#' cell in \code{oth} and added to another cell in \code{oth}.
#' The cell that is subtracted from and the cell that is added to
#' can be the same, so that the net increment is zero.
#'
#' Function \code{iter_next_cohort} returns an integer vector of length
#' three.
#'
#' The first element of the integer vector identifies the array
#' \code{oth} of increments that the current cell in\code{self}
#' contributes to. A value of \code{1} indicates that \code{self}
#' does not have age or triangle dimensions,
#' and that the cell contributes to an array of
#' 'no-triangle' increments. A value of \code{2} or \code{3}
#' indicates that \code{self} does have age and triangle dimensions.
#' A value of \code{2} indicates that the current cell of \code{self}
#' belongs to a lower Lexis triangle, and contributes to
#' an array of 'lower-triangle' increments. A value of \code{3}
#' indicates that the current cell of \code{self}
#' belongs to an upper Lexis triangle, and contributes to
#' an array of 'upper-triangle' increments.
#'
#' The second element of the integer vector returned by
#' \code{iter_next_cohort}
#' gives the index of the cell in \code{oth}
#' that is incremented. If no cell in \code{oth} is
#' incremented, then the value is \code{0}.
#'
#' The third element of the integer vector returned by
#' \code{iter_next_cohort} gives the index of the cell
#' \code{oth} that is decremented. If no cell in \code{oth}
#' is decremented, then the value is \code{0}.
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
#'                           map_dim = c(0, 1),
#'                           comp_type_self = "orig-dest",
#'                           indices_orig_self = 1,
#'                           indices_dest_self = 2)
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
    ans <- new.env(size = 15L)
    ans$pos_self <- spec@pos_self
    ans$pos_oth <- spec@pos_oth
    ans$dim_self <- spec@dim_self
    ans$n_dim_self <- spec@n_dim_self
    ans$n_dim_oth <- spec@n_dim_oth
    ans$map_dim <- spec@map_dim
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
        incremented_self <- FALSE
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
    i_oth <- c(0L, 0L, 0L)
    has_triangle <- i_triangle_self > 0L
    if (has_triangle) {
        is_lower <- pos_self[[i_triangle_self]] == 1L
        if (is_lower)
            i_oth[[1L]] <- 2L
        else
            i_oth[[1L]] <- 3L
    }
    else
        i_oth[[1L]] <- 1L
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
        offset_orig <- 0L
        for (i_orig_dest in seq_len(n_orig_dest_self)) {
            i_orig_self <- indices_orig_self[[i_orig_dest]]
            i_dest_self <- indices_dest_self[[i_orig_dest]]
            pos_orig <- pos_self[[i_orig_self]]
            pos_dest <- pos_self[[i_dest_self]]
            i_orig_dest_oth <- map_dim[[i_dest_self]]
            stride_orig_dest_oth <- strides_oth[[i_orig_dest_oth]]
            offset_orig <- offset_orig + (pos_orig - pos_dest) * stride_orig_dest_oth
        }
        i_oth[[2L]] <- i_oth_base
        i_oth[[3L]] <- i_oth_base + offset_orig
    }
    else if (i_comp_type_self == 4L) { # pool
        is_out <- pos_self[[i_direction_self]] == 1L
        if (is_out)
            i_oth[[3L]] <- i_oth_base
        else
            i_oth[[2L]] <- i_oth_base
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
