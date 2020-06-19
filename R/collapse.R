
## 'self' can have dimensions not in 'oth', and 'oth'
## can have dimensions not in 'self, but 'self' and 'oth'
## must share at least one dimension

## With shared dimensions, not all positions on a dimension of 'self'
## have to map on to a position in the dimension of 'oth', and
## multiple positions on the dimension of 'self' can map on to
## the same position on the dimension of 'oth'. Every position
## on the dimension of 'oth' must map on to one or more positions
## on the dimension of 'self.


## 'self' and 'oth' must both have positive length


iter_create_collapse <- function(spec) {
    ans <- new.env(size = 10L) ## CHECK THIS
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
    ans
}

iter_next_collapse <- function(iter) {
    pos_self <- iter$pos_self
    pos_oth <- iter$pos_oth
    n_dim_self <- iter$n_dim_self
    n_dim_oth <- iter@n_dim_oth
    dim_self <- iter$dim_self
    map_dim <- iter$map_dim
    map_pos <- iter$map_pos
    strides_oth <- iter$strides_oth
    n_offsets <- iter$n_offsets
    offsets <- iter$offsets
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
        val_pos_oth <- val_map_pos[[val_pos_self]]
        if (val_pos_oth == 0L)
            maps_into_oth <- FALSE
        pos_self[[i_dim_self]] <- val_pos_self
        i_dim_oth <- map_dim[[i_dim_self]]
        pos_oth[[i_dim_oth]] <- val_pos_oth
        if (incremented_self || !maps_into_oth)
            break
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
    has_next <- (i_dim_self < n_dim_self) || (val_pos_self < val_dim_self)
    iter$pos_self <- pos_self
    iter_pos_oth <- pos_oth
    iter$has_next <- has_next
    i_oth
}

iter_has_next_collapse <- function(iter) {
    iter$has_next
}


iter_nval_collapse <- function(iter) {
    iter$n_offsets
}
