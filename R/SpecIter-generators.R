
#' Create an object of class "SpecIterAccount"
#'
#' Create an object of class \code{SpecIterAccount} that
#' contains the information needed to construct
#' account iterators for an array of population counts.
#'
#' @param dim An integer vector. The dimensions of the array.
#' @param i_time The index for the time dimension of the array.
#' @param i_age The index for the age dimension of the array,
#' or \code{NULL} if the array does not have an age dimension.
#'
#' @return An object of class \code{SpecIterAccount}.
#'
#' @seealso To create a account iterator from an object of class
#' \code{SpecIterAccount}, use function
#' \code{\link[=account]{iter_create_account}}.
#'
#' @examples
#' x <- SpecIterAccount(dim = c(4, 2, 3),
#'                      i_time = 3,
#'                      i_age = 1)
#' class(x)
#' @export
SpecIterAccount <- function(dim, i_time, i_age = NULL) {
    ## 'dim_self'
    demcheck::err_positive_length(x = dim,
                                  name = "dim")
    dim_self <- demcheck::err_tdy_positive_integer_vector(x = dim,
                                                          name = "dim")
    ## 'i_time_self'
    i_time_self <- demcheck::err_tdy_positive_integer_scalar(x = i_time,
                                                             name = "i_time",
                                                             null_ok = FALSE)
    ## 'i_age_self'
    if (is.null(i_age))
        i_age_self <- 0L
    else
        i_age_self <- demcheck::err_tdy_positive_integer_scalar(x = i_age,
                                                                name = "i_age",
                                                                null_ok = TRUE)
    ## 'dim_self', 'i_time_self'
    demcheck::err_le_scalar(x1 = i_time_self,
                            x2 = length(dim_self),
                            name1 = "i_time",
                            name2 = "length(dim)")
    n_time <- dim_self[[i_time_self]]
    demcheck::err_dim_min_length(length_actual = n_time,
                                 length_min = 2L,
                                 name = "time")
    ## 'dim_self', 'i_age_self'
    if (i_age_self > 0L)
        demcheck::err_le_scalar(x1 = i_age_self,
                                x2 = length(dim_self),
                                name1 = "i_age",
                                name2 = "length(dim)")
    ## 'i_time_self', 'i_age_self'
    demcheck::err_indices_distinct(indices = list(i_time_self, i_age_self),
                                   names = c("i_time", "i_age"),
                                   exclude_zero = TRUE)
    ## 'n_age_self'
    if (i_age_self > 0L) {
        n_age_self <- dim_self[[i_age_self]]
        demcheck::err_dim_min_length(length_actual = n_age_self,
                                     length_min = 2L,
                                     name = "age")
    }
    else
        n_age_self <- 0L
    ## 'n_dim_self'
    n_dim_self <- length(dim_self)
    ## pos_self
    pos_self <- rep(1L, times = n_dim_self)
    ## 'stride_self'
    strides_self <- make_strides(dim_self)
    ## 'strides_initial'
    dim_initial <- replace(dim_self,
                           list = i_time_self,
                           values = 1L)
    strides_initial <- make_strides(dim_initial)
    ## 'strides_increments'
    dim_increments <- replace(dim_self,
                              list = i_time_self,
                              values = n_time - 1L)
    strides_increments <- make_strides(dim_increments)
    methods::new("SpecIterAccount",
                 pos_self = pos_self,
                 dim_self = dim_self,
                 n_dim_self = n_dim_self,
                 i_time_self = i_time_self,
                 i_age_self = i_age_self,
                 n_age_self = n_age_self,
                 strides_self = strides_self,
                 strides_initial = strides_initial,
                 strides_increments = strides_increments)
}

    
#' Create an object of class "SpecIterCohort"
#'
#' Create an object of class \code{SpecIterCohort} that
#' contains the information needed to construct iterators
#' to traverse cohorts within arrays with a particular
#' set of dimensions
#'
#' @param dim An integer vector. The dimensions of the array.
#' @param i_time The index for the time dimension of the array.
#' @param i_age The index for the age dimension of the array.
#' Equal to \code{NULL} if the array does not have an age dimension.
#' @param i_triangle The index of any dimension of array
#' that has dimtype \code{"triangle"}. Equal to \code{NULL} if array
#' does not have a triangle dimension.
#' @param stop_at_oldest Whether function \code{\link{iter_next_cohort}}
#' should return \code{FALSE} once the iterator reaches the oldest age group.
#' @param offsets A vector of one or more integers specifying
#' the offsets used to identify cohorts that are traversed
#' in parallel. Equals \code{0} of only one cohort is being traversed.
#'
#' @return An object of class \code{SpecIterCohort}.
#'
#' @seealso To create a cohort iterator from an object of class
#' \code{SpecIterCohort}, use function
#' \code{\link[=cohort]{iter_create_cohort}}.
#'
#' @examples
#' x <- SpecIterCohort(dim = c(4, 2, 3),
#'                     i_time = 3,
#'                     i_age = 1,
#'                     i_triangle = 2,
#'                     stop_at_oldest = FALSE,
#'                     offset = 0)
#' class(x)
#' @export
SpecIterCohort <- function(dim, i_time, i_age = NULL, i_triangle = NULL,
                           stop_at_oldest = NULL, offsets = 0L) {
    ## 'dim'
    demcheck::err_positive_length(x = dim,
                                  name = "dim")
    dim <- demcheck::err_tdy_positive_integer_vector(x = dim,
                                                     name = "dim")
    ## 'i_time'
    i_time <- demcheck::err_tdy_positive_integer_scalar(x = i_time,
                                                        name = "i_time",
                                                        null_ok = FALSE)
    ## 'i_age'
    i_age <- demcheck::err_tdy_positive_integer_scalar(x = i_age,
                                                       name = "i_age",
                                                       null_ok = TRUE)
    ## 'i_triangle'
    demcheck::chk_null_if_null(x1 = i_triangle,
                               x2 = i_age,
                               name1 = "i_triangle",
                               name2 = "i_age")
    if (!is.null(i_triangle)) {
        i_triangle <- demcheck::err_tdy_positive_integer_scalar(x = i_triangle,
                                                                name = "i_triangle",
                                                                null_ok = FALSE)
        demcheck::err_le_scalar(x1 = i_triangle,
                                x2 = length(dim),
                                name1 = "i_triangle",
                                name2 = "length(dim)")
        demcheck::err_not_equal_integer_scalar(x1 = i_triangle,
                                               x2 = i_time,
                                               name1 = "i_triangle",
                                               name2 = "i_time")
        demcheck::err_not_equal_integer_scalar(x1 = i_triangle,
                                               x2 = i_age,
                                               name1 = "i_triangle",
                                               name2 = "i_age")
    }
    ## stop_at_oldest
    demcheck::err_null_ifonlyif_null(x1 = stop_at_oldest,
                                     x2 = i_age,
                                     name1 = "stop_at_oldest",
                                     name2 = "i_age")
    if (!is.null(stop_at_oldest))
        demcheck::err_is_logical_flag(x = stop_at_oldest,
                                      name = "stop_at_oldest")
    ## 'offsets'
    offsets <- demcheck::err_tdy_non_negative_integer_vector(x = offsets,
                                                             name = "offsets")
    demcheck::err_positive_length(x = offsets,
                                  name = "offsets")
    demcheck::err_strictly_increasing(x = offsets,
                                      name = "offsets")
    ## 'dim', 'i_time'
    demcheck::err_le_scalar(x1 = i_time,
                            x2 = length(dim),
                            name1 = "i_time",
                            name2 = "length(dim)")
    ## 'dim', 'i_age'
    if (!is.null(i_age)) {
        demcheck::err_le_scalar(x1 = i_age,
                                x2 = length(dim),
                                name1 = "i_age",
                                name2 = "length(dim)")
    }
    ## 'dim', 'i_triangle'
    if (!is.null(i_triangle)) {
        demcheck::err_le_scalar(x1 = i_time,
                                x2 = length(dim),
                                name1 = "i_time",
                                name2 = "length(dim)")
    }
    ## 'i_age', 'i_triangle'
    val <- demcheck::chk_null_if_null(x1 = i_triangle,
                                      x2 = i_age,
                                      name1 = "i_triangle",
                                      name2 = "i_age")
    if (!isTRUE(val))
        return(val)
    ## 'i_age', 'stop_at_oldest'
    val <- demcheck::chk_null_ifonlyif_null(x1 = stop_at_oldest,
                                            x2 = i_age,
                                            name1 = "stop_at_oldest",
                                            name2 = "i_age")
    ## 'i_time', 'i_age', 'i_triangle'
    ## (NULLs are zapped)
    val <- demcheck::chk_indices_distinct(indices = list(i_time,
                                                         i_age,
                                                         i_triangle),
                                          names = c("i_time",
                                                    "i_age",
                                                    "i_triangle"),
                                          exclude_zero = FALSE)
    if (!isTRUE(val))
        return(val)
    ## convert NULLs to 0s or NA
    if (is.null(i_age))
        i_age <- 0L
    if (is.null(i_triangle))
        i_triangle <- 0L
    if (is.null(stop_at_oldest))
        stop_at_oldest <- NA
    ## 'n'
    n_time <- dim[[i_time]]
    if (i_age > 0L)
        n_age <- dim[[i_age]]
    else
        n_age <- 0L
    ## 'strides'
    strides <- make_strides(dim)
    stride_time <- strides[[i_time]]
    if (i_age > 0L)
        stride_age <- strides[[i_age]]
    else
        stride_age  <- 0L
    if (i_triangle > 0L)
        stride_triangle <- strides[[i_triangle]]
    else
        stride_triangle  <- 0L
    ## 'n_offsets'
    n_offsets <- length(offsets)
    ## return value
    methods::new("SpecIterCohort",
                 n_time = n_time,
                 n_age = n_age,
                 stride_time = stride_time,
                 stride_age = stride_age,
                 stride_triangle = stride_triangle,
                 stop_at_oldest = stop_at_oldest,
                 offsets = offsets,
                 n_offsets = n_offsets)
}


#' Create an object of class "SpecIterCollapse"
#'
#' Create an object of class \code{SpecIterCollpse} that
#' contains the information needed to create an iterator that
#' maps cells in array \code{self} into cells in array \code{oth}.
#'
#' @param dim_self An integer vector. The dimensions of array \code{self}.
#' @param dim_oth An integer vector. The dimensions of array \code{oth}.
#' @param map_dim An integer vector mapping the dimensions of \code{self}
#' on to those of \code{oth}. If a dimension from \code{self} is not
#' found in \code{oth}, the associated element of \code{map_dim} is \code{0}.
#' @param map_pos A list of integers vectors. Each integer vector
#' maps positions along a dimension of \code{self} on to a position
#' along the matching dimension of \code{oth}.
#'
#' @return An object of class \code{SpecIterCollapse}.
#'
#' @seealso To create a collapse iterator from an object of class
#' \code{SpecIterCollapse}, use function
#' \code{\link[=collapse]{create_iter_collapse}}.
#'
#' @examples
#' x <- SpecIterCollapse(dim_self = c(4L, 2L, 3L),
#'                       dim_oth = c(4L, 3L),
#'                       map_dim = c(1L, 0L, 2L),
#'                       map_pos = list(1:4, c(0L, 0L), 1:3))
#' class(x)
#' @export
SpecIterCollapse <- function(dim_self, dim_oth, map_dim, map_pos) {
    ## 'dim_self'
    dim_self <- demcheck::err_tdy_positive_integer_vector(x = dim_self,
                                                          name = "dim_self")
    ## 'dim_oth'
    dim_oth <- demcheck::err_tdy_positive_integer_vector(x = dim_oth,
                                                         name = "dim_oth")
    ## 'map_dim'
    map_dim <- demcheck::err_tdy_map_dim(map_dim = map_dim,
                                         n_dim_self = length(dim_self),
                                         n_dim_oth = length(dim_oth))
    ## 'map_pos'
    map_pos <- demcheck::err_tdy_map_pos(map_pos = map_pos,
                                         dim_self = dim_self,
                                         dim_oth = dim_oth,
                                         map_dim = map_dim)
    ## derived quantities
    n_dim_self <- length(dim_self)
    n_dim_oth <- length(dim_oth)
    pos_self <- rep(1L, times = n_dim_self)
    pos_oth <- rep(1L, times = n_dim_oth)
    for (i_dim_self in seq_len(n_dim_self)) {
        val_map_dim <- map_dim[[i_dim_self]]
        maps_into_oth <- val_map_dim > 0L
        if (maps_into_oth) {
            val_map_pos <- map_pos[[i_dim_self]]
            pos_oth[[val_map_dim]] <- val_map_pos[[1L]]
        }
    }
    strides_oth <- make_strides(dim_oth)
    offsets <- make_offsets(dim_oth = dim_oth,
                            map_dim = map_dim)
    n_offsets <- length(offsets)
    ## return value
    methods::new("SpecIterCollapse",
                 pos_self = pos_self,
                 pos_oth = pos_oth,
                 dim_self = dim_self,
                 n_dim_self = n_dim_self,
                 n_dim_oth = n_dim_oth,
                 map_dim = map_dim,
                 map_pos = map_pos,
                 strides_oth = strides_oth,
                 offsets = offsets,
                 n_offsets = n_offsets)
}


#' Create an object of class "SpecIterIncrement"
#'
#' Create an object of class \code{SpecIterCollpse} that
#' contains the information needed to create an iterator
#' that maps cells in array \code{self} to cells
#' in arrays of increments or decrements.
#'
#' @inheritParams SpecIterCollapse
#' @param comp_type_self The type of component: \code{"increment"},
#' \code{"decrement"}, \code{"orig-dest"}, or \code{"pool"}.
#' @param i_triangle_self The index of any dimension of array \code{self}
#' that has dimtype \code{"triangle"}. Equal to \code{NULL} if \code{self}
#' does not have a triangle dimension.
#' @param indices_orig_self An integer vector containing
#' the indices of any dimensions of \code{self} with dimtype
#' \code{"origin"}.
#' @param indices_dest_self An integer vector containing
#' the indices of any dimensions of \code{self} with dimtype
#' \code{"destination"}.
#' @param i_direction_self The index of any dimension of array \code{self}
#' that has dimtype \code{"direction"}. Equal to \code{NULL} if \code{self}
#' does not have a direction dimension.
#'
#' @return An object of class \code{SpecIterIncrement}.
#'
#' @seealso To create an increment iterator from an object of class
#' \code{SpecIterIncrement}, use function
#' \code{\link[=increment]{create_iter_increment}}.
#'
#' @examples
#' x <- SpecIterIncrement(dim_self = c(4, 2, 3),
#'                        dim_oth = c(4, 3),
#'                        map_dim = c(1, 0, 2),
#'                        comp_type_self = "increment",
#'                        i_triangle_self = 2)
#' class(x)
#' @export
SpecIterIncrement <- function(dim_self,
                              dim_oth,
                              map_dim,
                              comp_type_self,
                              i_triangle_self = NULL,
                              indices_orig_self = NULL,
                              indices_dest_self = NULL,
                              i_direction_self = NULL) {
    ## 'dim_self'
    demcheck::err_positive_length(dim_self,
                                  name = "dim_self")
    dim_self <- demcheck::err_tdy_positive_integer_vector(x = dim_self,
                                                          name = "dim_self")
    ## 'dim_oth'
    demcheck::err_positive_length(dim_oth,
                                  name = "dim_oth")
    dim_oth <- demcheck::err_tdy_positive_integer_vector(x = dim_oth,
                                                         name = "dim_oth")
    ## 'map_dim'
    map_dim <- demcheck::err_tdy_map_dim(map_dim = map_dim,
                                         n_dim_self = length(dim_self),
                                         n_dim_oth = length(dim_oth))
    ## 'camp_type_self'
    demcheck::err_member_comp_type(x = comp_type_self,
                                   name = "comp_type_self")
    ## 'i_triangle_self'
    if (!is.null(i_triangle_self)) {
        i_triangle_self <- demcheck::err_tdy_positive_integer_scalar(x = i_triangle_self,
                                                                     name = "i_triangle_self",
                                                                     null_ok = FALSE)
        demcheck::err_le_scalar(x1 = i_triangle_self,
                                x2 = length(dim_self),
                                name1 = "i_triangle_self",
                                name2 = "length(dim_self)")
        demcheck::err_omitted(index = i_triangle_self,
                              map_dim = map_dim,
                              name_index = "i_triangle_self",
                              name_dim = "triangle")
    }
    ## 'indices_orig_self'
    if (!is.null(indices_orig_self)) {
        demcheck::err_positive_length(indices_orig_self,
                                      name = "indices_orig_self")
        indices_orig_self <- demcheck::err_tdy_positive_integer_vector(x = indices_orig_self,
                                                                       name = "indices_orig_self")
        demcheck::err_le_vector(x1 = indices_orig_self,
                                x2 = length(dim_self),
                                name1 = "indices_orig_self",
                                name2 = "length(dim_self)")
        demcheck::err_omitted(index = indices_orig_self,
                              map_dim = map_dim,
                              name_index = "indices_orig_self",
                              name_dim = "origin")
    }
    ## 'indices_dest_self'
    demcheck::err_null_ifonlyif_null(x1 = indices_dest_self,
                                     x2 = indices_orig_self,
                                     name1 = "indices_dest_self",
                                     name2 = "indices_orig_self")
    if (!is.null(indices_dest_self)) {
        demcheck::err_length_same(x1 = indices_dest_self,
                                  x2 = indices_orig_self,
                                  name1 = "indices_dest_self",
                                  name2 = "indices_orig_self")
        indices_dest_self <- demcheck::err_tdy_positive_integer_vector(x = indices_dest_self,
                                                                       name = "indices_dest_self")
        demcheck::err_le_vector(x1 = indices_dest_self,
                                x2 = length(dim_self),
                                name1 = "indices_dest_self",
                                name2 = "length(dim_self)")
        demcheck::err_not_omitted(index = indices_dest_self,
                                  map_dim = map_dim,
                                  name_index = "indices_dest_self",
                                  name_dim = "destination")
    }
    ## 'i_direction_self'
    if (!is.null(i_direction_self)) {
        i_direction_self <- demcheck::err_tdy_positive_integer_scalar(x = i_direction_self,
                                                                      name = "i_direction_self",
                                                                      null_ok = FALSE)
        demcheck::err_le_scalar(x1 = i_direction_self,
                                x2 = length(dim_self),
                                name1 = "i_direction_self",
                                name2 = "length(dim_self)")
        demcheck::err_omitted(index = i_direction_self,
                              map_dim = map_dim,
                              name_index = "i_direction_self",
                              name_dim = "direction")
    }
    ## derived values
    n_dim_self <- length(dim_self)
    n_dim_oth <- length(dim_oth)
    pos_self <- rep.int(1L, times = n_dim_self)
    pos_oth <- rep.int(1L, times = n_dim_oth)
    strides_oth <- make_strides(dim_oth)
    i_comp_type_self <- make_i_comp_type(comp_type_self)
    if (is.null(i_triangle_self))
        i_triangle_self <- 0L
    if (is.null(indices_orig_self)) {
        indices_orig_self <- 0L
        indices_dest_self <- 0L
    }
    n_orig_dest_self <- length(indices_orig_self)
    if (is.null(i_direction_self))
        i_direction_self <- 0L
    ## 'i_comp_type_self', 'indices_orig_self', and 'i_direction_self'
    demcheck::err_comp_type_indices(i_comp_type_self = i_comp_type_self,
                                    indices_orig_self = indices_orig_self,
                                    i_direction_self = i_direction_self)
    methods::new("SpecIterIncrement",
                 pos_self = pos_self,
                 pos_oth = pos_oth,
                 dim_self = dim_self,
                 n_dim_self = n_dim_self,
                 n_dim_oth = n_dim_oth,
                 map_dim = map_dim,
                 strides_oth = strides_oth,
                 i_comp_type_self = i_comp_type_self,
                 i_triangle_self = i_triangle_self,
                 indices_orig_self = indices_orig_self,
                 indices_dest_self = indices_dest_self,
                 n_orig_dest_self = n_orig_dest_self,
                 i_direction_self = i_direction_self)
}
