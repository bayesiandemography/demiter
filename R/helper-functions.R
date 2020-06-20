
## HAS_TESTS
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


## HAS_TESTS
make_offsets <- function(dim_oth, map_dim) {
    demcheck::err_positive_dim(x = dim_oth,
                               name = "dim_oth")
    demcheck::err_map_dim(x = map_dim,
                          name = "map_dim")
    n_dim_oth <- length(dim_oth)
    stride <- 1L
    ans <- rep(list(NULL), times = n_dim_oth)
    for (i_dim_oth in seq_len(n_dim_oth)) {
        val_dim_oth <- dim_oth[[i_dim_oth]]
        no_assoc_dim_in_self <- !(i_dim_oth %in% map_dim)
        if (no_assoc_dim_in_self) {
            ans[[i_dim_oth]] <- seq.int(from = 0L,
                                        by = stride,
                                        length.out = val_dim_oth)
        }
        stride <- val_dim_oth * stride
    }
    ans <- Filter(Negate(is.null), ans)
    if (identical(length(ans), 0L))
        0L
    else {
        add_outer <- function(e1, e2) outer(e1, e2, "+")
        ans <- Reduce(f = add_outer, x = ans)
        as.integer(ans)
    }
}


## HAS_TESTS
make_strides <- function(dim) {
    demcheck::err_positive_length(dim)
    dim <- demcheck::err_tdy_positive_integer_vector(x = dim,
                                                     name = "dim")
    n_dim <- length(dim)
    ans <- cumprod(dim[-n_dim])
    ans <- as.integer(ans)
    ans <- c(1L, ans)
    ans
}
