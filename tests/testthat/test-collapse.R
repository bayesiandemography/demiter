
context("collapse")

spec <- SpecIterCollapse(dim_self = c(4, 2),
                         dim_oth = 3,
                         map_dim = c(1, 0),
                         map_pos = list(c(1, 2, 3, 3), c(0, 0)))
iter <- iter_create_collapse(spec)
iter_has_next_collapse(iter)
iter_next_collapse(iter)

