

make_i_comp_type <- function(comp_type) {
    if (comp_type == "increment")
        1L
    else if (comp_type == "decrement")
        2L
    else if (comp_type == "orig-dest")
        3L
    else if (comp_type == "pool")
        4L
    else
        stop(gettextf("invalid value for '%s' : \"%s\"",
                      "comp_type", comp_type))
}


make_offsets <- function(dim_oth, map_dim) {
    n_dim_oth <- length(dim_oth)
    stride <- 1L
    ans <- rep(list(NULL), times = n_dim_oth)
    for (i_dim_oth in seq_len(n_dim_oth)) {
        is_in_map_dim <- match(i_dim_oth, map_dim, nomatch = 0L) > 0L
        if (!is_in_map_dim) {
            val_dim_oth <- dim_oth[[i_dim_oth]]
            ans[[i_dim_oth]] <- seq.int(from = 0L,
                                        by = stride,
                                        length.out = val_dim_oth)
        }
        stride <- val_dim_oth * stride
    }
    ans <- ans[!is.null(ans)]
    ans <- Reduce(f = outer, x = ans)
    ans <- as.integer(ans)
    ans
}

make_strides <- function(dim) {
    n_dim <- length(dim)
    if (n_dim == 0L)
        ans <- integer()
    else {
        ans <- cumprod(dim[-n_dim])
        ans <- as.integer(ans)
        ans <- c(1L, ans)
    }
    ans
}
