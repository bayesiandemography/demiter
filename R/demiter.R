
#' demiter: Iterators for demographic arrays
#'
#' Iterators for demographic arrays, plus
#' classes and functions for workign with
#' these iterators. End users would not
#' normally use this package directly.
#'
#' The iterators themselves are built from environments.
#' They break with R's usual semantics in that they
#' are modified in place by the 'next' functions.
#' Calling a 'next' function on an interator that
#' has been exhausted throws an error.
#'
#' All iterators have a has_next function, which
#' returns FALSE if the iterator has been exhausted.
#'
#' Iterators are created from objects with class IterSpec,
#' by a call to an 'create_iterator' function. 
#'
#' \pkg{demiter} does not use formal classes and
#' methods for iterators, but instead uses
#' suffixes on all functions.
#' 
#'
#' @docType package
#' @name demiter
NULL

