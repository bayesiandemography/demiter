
#' Collapse iterators
#'
#' A collapse iterator traverses array \code{self},
#' returning the indices for the associated cells
#' in array \code{oth}.
#'
#' Arrays \code{self} and \code{oth} must both have at least
#' one dimension, and none of their dimensions can have length 0.
#'
#' Arrays \code{self} and \code{oth} are related as follows:
#' \itemize{
#' \item \code{self} can have dimensions not in \code{oth},
#'   and \code{oth} can have dimensions not in \code{self},
#'   but \code{self} and \code{oth} must share at least
#'   one dimension.
#' \item A position on the \code{self} version of a shared
#'   dimension can map to zero or one positions on the \code{oth}
#'   version of the dimension.
#' \item A position on the \code{oth} version of a shared dimension
#'   must map to at least one position on the \code{self} version
#'   of the dimension, and can map to several.
#' }
#'
#' If \code{oth} has extra dimensions that \code{self} does not,
#' then indices along these extra dimensions are included in the
#' return value from the iterator. If \code{oth} does not
#' have extra dimensions, then the return value has length 1.
#' 
#' \code{iter_create_collapse} creates a new iterator,
#' based on the information on arrays \code{self} and
#' \code{oth} contained in a \code{\link{SpecIterCollapse}}
#' object.
#'
#' \code{iter_next_collapse} moves the iterator to
#' the next cell of \code{self}, and returns the
#' indices of the associated cell(s) in \code{oth}.
#'
#' \code{iter_has_next_collapse} returns \code{FALSE}
#' if the iterator has reached the final cell of
#' \code{self}, and \code{TRUE} otherwise.
#'
#' @param spec An object of class \code{\link{SpecIterCollapse}}.
#' @param iter A collapse iterator.
#'
#' @seealso \code{\link{SpecIterCollapse}}, \code{\link{cohort}},
#' \code{\link{increment}}
#'
#' @examples
#' spec <- SpecIterCollapse(dim_self = c(3, 2),
#'                          dim_oth = 2,
#'                          map_dim = c(1, 0),
#'                          map_pos = list(c(1, 2, 2), c(0, 0)))
#' iter <- iter_create_collapse(spec)
#' for (i in 1:6) {
#'     print(iter_has_next_collapse(iter))
#'     print(iter_next_collapse(iter))
#' }
#' iter_has_next_collapse(iter)
#' @name collapse
NULL

#' @rdname collapse
#' @export
iter_create_collapse <- function(spec) {
    ans <- new.env(size = 11L)
    ans$pos_self <- spec@pos_self
    ans$pos_oth <- spec@pos_oth
    ans$dim_self <- spec@dim_self
    ans$n_dim_self <- spec@n_dim_self
    ans$n_dim_oth <- spec@n_dim_oth
    ans$map_dim <- spec@map_dim
    ans$map_pos <- spec@map_pos
    ans$strides_oth <- spec@strides_oth
    ans$offsets <- spec@offsets
    ans$n_offsets <- spec@n_offsets
    ans$has_next <- TRUE
    ans$is_first <- TRUE
    ans
}

#' @rdname collapse
#' @export
iter_next_collapse <- function(iter) {
    pos_self <- iter$pos_self
    pos_oth <- iter$pos_oth
    n_dim_self <- iter$n_dim_self
    n_dim_oth <- iter$n_dim_oth
    dim_self <- iter$dim_self
    map_dim <- iter$map_dim
    map_pos <- iter$map_pos
    strides_oth <- iter$strides_oth
    n_offsets <- iter$n_offsets
    offsets <- iter$offsets
    is_first <- iter$is_first
    if (is_first) {
        maps_into_oth <- all(pos_oth > 0L)
        iter$is_first <- FALSE
    }
    else {
        incremented_self <- FALSE
        maps_into_oth <- TRUE
        for (i_dim_self in seq_len(n_dim_self)) {
            val_dim_self <- dim_self[[i_dim_self]]
            val_map_pos <- map_pos[[i_dim_self]]
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
            if (has_dim_oth) {
                val_pos_oth <- val_map_pos[[val_pos_self]]
                if (val_pos_oth > 0L)
                    pos_oth[[i_dim_oth]] <- val_pos_oth
                else
                    maps_into_oth <- FALSE
            }
            if (incremented_self || !maps_into_oth)
                break
        }
    }
    if (maps_into_oth) {
        i_oth_first <- 1L
        for (i_dim_oth in seq_len(n_dim_oth)) {
            val_strides_oth <- strides_oth[[i_dim_oth]]
            val_pos_oth <- pos_oth[[i_dim_oth]]
            i_oth_first <- i_oth_first + val_strides_oth * (val_pos_oth - 1L)
        }
        i_oth <- i_oth_first + offsets
    }
    else
        i_oth <- rep.int(0L, times = n_offsets)
    has_next <- any(pos_self < dim_self)
    iter$pos_self <- pos_self
    iter_pos_oth <- pos_oth
    iter$has_next <- has_next
    i_oth
}


#' @rdname collapse
#' @export
iter_has_next_collapse <- function(iter) {
    iter$has_next
}

