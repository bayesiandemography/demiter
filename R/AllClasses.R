
setClass("SpecIter",
         contains = "VIRTUAL")

setClass("SpecIterCohort",
         contains = "SpecIter",
         slots = c(n_time = "integer",
                   n_age = "integer",
                   stride_time = "integer",
                   stride_age = "integer",
                   stride_triangle = "integer",
                   stop_at_oldest = "logical",
                   offsets = "integer",
                   n_offsets = "integer"),
         validity = function(object) {
             n_time <- object@n_time
             n_age <- object@n_age
             stride_time <- object@stride_time
             stride_age <- object@stride_age
             stride_triangle <- object@stride_triangle
             stop_at_oldest <- object@stop_at_oldest
             offsets <- object@offsets
             n_offsets <- object@n_offsets
             ## 'n_time'
             val <- demcheck::chk_positive_scalar(x = n_time,
                                                  name = "n_time")
             if (!isTRUE(val))
                 return(val)
             ##' n_age'
             val <- demcheck::chk_non_negative_scalar(x = n_age,
                                                      name = "n_age")
             if (!isTRUE(val))
                 return(val)
             ## 'stride_time'
             val <- demcheck::chk_positive_scalar(x = stride_time,
                                                  name = "stride_time")
             if (!isTRUE(val))
                 return(val)
             ## 'stride_age'
             val <- demcheck::chk_non_negative_scalar(x = stride_age,
                                                      name = "stride_age")
             if (!isTRUE(val))
                 return(val)
             ## 'stride_triangle'
             val <- demcheck::chk_non_negative_scalar(x = stride_triangle,
                                                      name = "stride_triangle")
             if (!isTRUE(val))
                 return(val)
             ## 'stop_at_oldest'
             val <- demcheck::chk_length_1(x = stop_at_oldest,
                                           name = "stop_at_oldest")
             if (!isTRUE(val))
                 return(val)
             ## 'offsets'
             val <- demcheck::chk_non_negative_vector(x = offsets,
                                                      name = "offsets")
             if (!isTRUE(val))
                 return(val)
             val <- demcheck::chk_strictly_increasing(x = offsets,
                                                      name = "offsets")
             if (!isTRUE(val))
                 return(val)
             ## 'n_offsets'
             val <- demcheck::chk_positive_scalar(x = n_offsets,
                                                  name = "n_offsets")
             if (!isTRUE(val))
                 return(val)
             ## 'stride_age' and 'stop_at_oldest'
             if (stride_age == 0L)
                 val <- demcheck::chk_is_na_scalar(x = stop_at_oldest,
                                                   name = "stop_at_oldest")
             else
                 val <- demcheck::chk_not_na_scalar(x = stop_at_oldest,
                                                    name = "stop_at_oldest")
             if (!isTRUE(val))
                 return(val)
             ## 'offsets' and 'n_offsets'
             val <- demcheck::chk_length_equals(x1 = offsets,
                                                x2 = n_offsets,
                                                name1 = "offsets",
                                                name2 = "n_offsets")
             if (!isTRUE(val))
                 return(val)
             TRUE    
         })



setClass("SpecIterCollapse",
         contains = "SpecIter",
         slots = c(pos_self = "integer",
                   pos_oth = "integer",
                   dim_self = "integer",
                   n_dim_self = "integer",
                   n_dim_oth = "integer",
                   map_dim = "integer",
                   map_pos = "list",
                   strides_oth = "integer",
                   offsets = "integer",
                   n_offsets = "integer"),
         validity = function(object) {
             pos_self <- object@pos_self
             pos_oth <- object@pos_oth
             dim_self <- object@dim_self
             n_dim_self <- object@n_dim_self
             n_dim_oth <- object@n_dim_oth
             map_dim <- object@map_dim
             map_pos <- object@map_pos
             strides_oth <- object@strides_oth
             offsets <- object@offsets
             n_offsets <- object@n_offsets
             ## 'pos_self'
             val <- demcheck::chk_pos_initial(x = pos_self,
                                              name = "pos_self",
                                              zero_ok = FALSE)
             if (!isTRUE(val))
                 return(val)
             ## 'pos_oth'
             val <- demcheck::chk_non_negative_vector(x = pos_oth,
                                                      name = "pos_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'dim_self'
             val <- demcheck::chk_positive_dim(x = dim_self,
                                               name = "dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_dim_self'
             val <- demcheck::chk_positive_scalar(x = n_dim_self,
                                                  name = "n_dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_dim_oth'
             val <- demcheck::chk_positive_scalar(x = n_dim_oth,
                                                  name = "n_dim_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'map_dim'
             val <- demcheck::chk_map_dim(x = map_dim,
                                          name = "map_dim")
             if (!isTRUE(val))
                 return(val)
             ## 'map_pos'
             val <- demcheck::chk_map_pos(x = map_pos,
                                          name = "map_pos")
             if (!isTRUE(val))
                 return(val)
             ## 'strides_oth'
             val <- demcheck::chk_positive_vector(x = strides_oth,
                                                  name = "strides_oth")
             if (!isTRUE(val))
                 return(val)
             val <- demcheck::chk_increasing(x = strides_oth,
                                             name = "strides_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'offsets'
             val <- demcheck::chk_non_negative_vector(x = offsets,
                                                      name = "offsets")
             if (!isTRUE(val))
                 return(val)
             val <- demcheck::chk_strictly_increasing(x = offsets,
                                                      name = "offsets")
             if (!isTRUE(val))
                 return(val)
             ## 'n_offsets'
             val <- demcheck::chk_positive_scalar(x = n_offsets,
                                                  name = "n_offsets")
             if (!isTRUE(val))
                 return(val)
             ## 'dim_self' and 'pos_self'
             val <- demcheck::chk_length_same(x1 = pos_self,
                                              x2 = dim_self,
                                              name1 = "pos_self",
                                              name2 = "dim_self")
             if (!isTRUE(val))
                 return(val)
             val <- demcheck::chk_le_vector(x1 = pos_self,
                                            x2 = dim_self,
                                            name1 = "pos_self",
                                            name2 = "dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_dim_self' and 'dim_self'
             val <- demcheck::chk_length_equals(x1 = dim_self,
                                                x2 = n_dim_self,
                                                name1 = "dim_self",
                                                name2 = "n_dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_dim_oth' and 'pos_oth'
             val <- demcheck::chk_length_equals(x1 = pos_oth,
                                                x2 = n_dim_oth,
                                                name1 = "pos_oth",
                                                name2 = "n_dim_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'map_dim' and 'dim_self'
             val <- demcheck::chk_length_same(x1 = map_dim,
                                              x2 = dim_self,
                                              name1 = "map_dim",
                                              name2 = "dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'map_dim' and 'n_dim_oth'
             val <- demcheck::chk_all_x1_in_x2(x1 = map_dim,
                                               x2 = seq_len(n_dim_oth),
                                               name1 = "map_dim",
                                               name2 = "seq_len(n_dim_oth)",
                                               exclude_zero = TRUE)
             if (!isTRUE(val))
                 return(val)
             ## 'map_pos' and 'dim_self'
             val <- demcheck::chk_length_same(x1 = map_pos,
                                              x2 = dim_self,
                                              name1 = "map_pos",
                                              name2 = "dim_self")
             if (!isTRUE(val))
                 return(val)
             val <- demcheck::chk_lengths_elements_equal_vec(x1 = map_pos,
                                                             x2 = dim_self,
                                                             name1 = "map_pos",
                                                             name2 = "dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'map_pos' and 'map_dim'
             for (i in seq_along(map_pos)) {
                 val_map_pos <- map_pos[[i]]
                 val_map_dim <- map_dim[[i]]
                 if (val_map_dim > 0L) {
                     if (all(val_map_pos == 0L)) {
                         return(gettextf("element %d of '%s' equals %d but element %d of '%s' has no non-zero values",
                                         i, "map_dim", val_map_dim, i, "map_pos"))
                     }
                 }
                 else {
                     if (any(val_map_pos != 0L)) {
                         return(gettextf("element %d of '%s' equals %d but element %d of '%s' has non-zero values",
                                         i, "map_dim", val_map_dim, i, "map_pos"))
                     }
                 }
             }
             ## 'strides_oth' and 'n_dim_oth'
             val <- demcheck::chk_length_equals(x1 = strides_oth,
                                                x2 = n_dim_oth,
                                                name1 = "strides_oth",
                                                name2 = "n_dim_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'n_offsets' and 'offsets'
             val <- demcheck::chk_length_equals(x1 = offsets,
                                                x2 = n_offsets,
                                                name1 = "offsets",
                                                name2 = "n_offsets")
             if (!isTRUE(val))
                 return(val)
             TRUE    
         })


setClass("SpecIterIncrement",
         contains = "SpecIter",
         slots = c(pos_self = "integer",
                   pos_oth = "integer",
                   dim_self = "integer",
                   n_dim_self = "integer",
                   n_dim_oth = "integer",
                   map_dim = "integer",
                   strides_oth = "integer",
                   i_comp_type_self = "integer",
                   i_triangle_self = "integer",
                   indices_orig_self = "integer",
                   indices_dest_self = "integer",
                   n_orig_dest_self = "integer",
                   i_direction_self = "integer"),
         validity = function(object) {
             pos_self <- object@pos_self
             pos_oth <- object@pos_oth
             dim_self <- object@dim_self
             n_dim_self <- object@n_dim_self
             n_dim_oth <- object@n_dim_oth
             map_dim <- object@map_dim
             strides_oth <- object@strides_oth
             i_comp_type_self <- object@i_comp_type_self
             i_triangle_self = object@i_triangle_self
             indices_orig_self = object@indices_orig_self
             indices_dest_self = object@indices_dest_self
             n_orig_dest_self = object@n_orig_dest_self
             i_direction_self = object@i_direction_self
             ## 'pos_self'
             val <- demcheck::chk_pos_initial(x = pos_self,
                                              name = "pos_self",
                                              zero_ok = FALSE)
             if (!isTRUE(val))
                 return(val)
             ## 'pos_oth'
             val <- demcheck::chk_pos_initial(x = pos_oth,
                                              name = "pos_oth",
                                              zero_ok = FALSE)
             ## 'dim_self'
             val <- demcheck::chk_positive_dim(x = dim_self,
                                               name = "dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_dim_self'
             val <- demcheck::chk_positive_scalar(x = n_dim_self,
                                                  name = "n_dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_dim_oth'
             val <- demcheck::chk_positive_scalar(x = n_dim_oth,
                                                  name = "n_dim_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'map_dim'
             val <- demcheck::chk_map_dim(x = map_dim,
                                          name = "map_dim")
             if (!isTRUE(val))
                 return(val)
             ## 'strides_oth'
             val <- demcheck::chk_positive_vector(x = strides_oth,
                                                  name = "strides_oth")
             if (!isTRUE(val))
                 return(val)
             val <- demcheck::chk_strictly_increasing(x = strides_oth,
                                                      name = "strides_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'i_comp_type_self'
             val <- demcheck::chk_positive_scalar(x = i_comp_type_self,
                                                  name = "i_comp_type_self")
             if (!isTRUE(val))
                 return(val)
             ## 'i_triangle_self'
             val <- demcheck::chk_non_negative_scalar(x = i_triangle_self,
                                                      name = "i_triangle_self")
             if (!isTRUE(val))
                 return(val)
             ## 'indices_orig_self'
             val <- demcheck::chk_non_negative_vector(x = indices_orig_self,
                                                      name = "indices_orig_self")
             if (!isTRUE(val))
                 return(val)
             ## 'indices_dest_self'
             val <- demcheck::chk_non_negative_vector(x = indices_dest_self,
                                                      name = "indices_dest_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_orig_dest_self'
             val <- demcheck::chk_non_negative_scalar(x = n_orig_dest_self,
                                                      name = "n_orig_dest_self")
             if (!isTRUE(val))
                 return(val)
             ## 'i_direction_self'
             val <- demcheck::chk_non_negative_scalar(x = i_direction_self,
                                                      name = "i_direction_self")
             if (!isTRUE(val))
                 return(val)
             ## 'dim_self' and 'pos_self'
             val <- demcheck::chk_length_same(x1 = pos_self,
                                              x2 = dim_self,
                                              name1 = "pos_self",
                                              name2 = "dim_self")
             if (!isTRUE(val))
                 return(val)
             val <- demcheck::chk_le_vector(x1 = pos_self,
                                            x2 = dim_self,
                                            name1 = "pos_self",
                                            name2 = "dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_dim_self' and 'dim_self'
             val <- demcheck::chk_length_equals(x1 = dim_self,
                                                x2 = n_dim_self,
                                                name1 = "dim_self",
                                                name2 = "n_dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'n_dim_oth' and 'pos_oth'
             val <- demcheck::chk_length_equals(x1 = pos_oth,
                                                x2 = n_dim_oth,
                                                name1 = "pos_oth",
                                                name2 = "n_dim_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'map_dim' and 'dim_self'
             val <- demcheck::chk_length_same(x1 = map_dim,
                                              x2 = dim_self,
                                              name1 = "map_dim",
                                              name2 = "dim_self")
             if (!isTRUE(val))
                 return(val)
             ## 'map_dim' and 'n_dim_self'
             val <- demcheck::chk_all_x1_in_x2(x1 = map_dim,
                                               x2 = seq_len(n_dim_self),
                                               name1 = "map_dim",
                                               name2 = "seq_len(n_dim_self)",
                                               exclude_zero = TRUE)
             if (!isTRUE(val))
                 return(val)
             ## 'strides_oth' and 'n_dim_oth'
             val <- demcheck::chk_length_equals(x1 = strides_oth,
                                                x2 = n_dim_oth,
                                                name1 = "strides_oth",
                                                name2 = "n_dim_oth")
             if (!isTRUE(val))
                 return(val)
             ## 'indices_orig_self' and 'indices_dest_self'
             val <- demcheck::chk_length_same(x1 = indices_orig_self,
                                              x2 = indices_dest_self,
                                              name1 = "indices_orig_self",
                                              name2 = "indices_dest_self")
             if (!isTRUE(val))
                 return(val)
             ## 'indices_orig_self' and 'n_dim_oth'
             val <- demcheck::chk_length_equals(x1 = indices_orig_self,
                                                x2 = n_orig_dest_self,
                                                name1 = "indices_orig_self",
                                                name2 = "n_orig_dest")
             if (!isTRUE(val))
                 return(val)
             ## 'i_triangle_self', 'indices_orig_self', 'indices_dest_self', 'i_direction_self'
             val <- demcheck::chk_indices_distinct(indices = list(i_triangle_self,
                                                                  indices_orig_self,
                                                                  indices_dest_self,
                                                                  i_direction_self),
                                                   names = c("i_triangle_self",
                                                             "indices_orig_self",
                                                             "indices_dest_self",
                                                             "i_direction_self"),
                                                   exclude_zero = TRUE)
             ## 'i_comp_type_self', 'indices_orig_self', and 'i_direction_self'
             val <- demcheck::chk_comp_type_indices(i_comp_type_self = i_comp_type_self,
                                                    indices_orig_self = indices_orig_self,
                                                    i_direction_self = i_direction_self)
             if (!isTRUE(val))
                 return(val)
             TRUE    
         })
