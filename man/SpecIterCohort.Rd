% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpecIter-generators.R
\name{SpecIterCohort}
\alias{SpecIterCohort}
\title{Create an object of class "SpecIterCohort"}
\usage{
SpecIterCohort(
  dim,
  i_time,
  i_age = NULL,
  i_triangle = NULL,
  stop_at_oldest = NULL,
  offsets = 0L
)
}
\arguments{
\item{dim}{An integer vector. The dimensions of the array.}

\item{i_time}{The index for the time dimension of the array.}

\item{i_age}{The index for the age dimension of the array.
Equal to \code{NULL} if the array does not have an age dimension.}

\item{i_triangle}{The index of any dimension of array
that has dimtype \code{"triangle"}. Equal to \code{NULL} if array
does not have a triangle dimension.}

\item{stop_at_oldest}{Whether function \code{\link{iter_next_cohort}}
should return \code{FALSE} once the iterator reaches the oldest age group.}

\item{offsets}{A vector of one or more integers specifying
the offsets used to identify cohorts that are traversed
in parallel. Equals \code{0} of only one cohort is being traversed.}
}
\value{
An object of class \code{SpecIterCohort}.
}
\description{
Create an object of class \code{SpecIterCohort} that
contains the information needed to construct iterators
to traverse cohorts within arrays with a particular
set of dimensions
}
\examples{
x <- SpecIterCohort(dim = c(4, 2, 3),
                    i_time = 3,
                    i_age = 1,
                    i_triangle = 2,
                    stop_at_oldest = FALSE,
                    offset = 0)
class(x)
}
\seealso{
To create a cohort iterator from an object of class
\code{SpecIterCohort}, use function
\code{\link[=cohort]{iter_create_cohort}}.
}
