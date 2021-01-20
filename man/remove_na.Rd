\name{remove_na}
\alias{remove_na}
\title{
  Remove individuals or variables having too many missing values in a
  dataframe.
}
\description{
  Return a new dataframe keeping only the variables with enough
  information. The user may define a threshold for what is
  \dQuote{enough information} (either by a raw number or a proportion).
}
\usage{
remove_na(data, which = c("ind", "var"), prop_min = NULL, n_min = NULL)
}
\arguments{
  \item{data}{
    a dataframe.
  }
  \item{which}{
    either \code{ind} or \code{var}, to remove individuals or variables
    respectively.
  }
  \item{prop_min}{
    a proportion between 0 and 1. An individual or a variable must have
    at least a proportion \code{prop_min} of nonmissing values to be
    kept.
  }
  \item{n_min}{
    an integer. An individual or a variable must have at least
    \code{n_min} nonmissing values to be kept.
  }
}
\value{
  A dataframe purged from the cases or variables having too many missing values.
}
\author{
  Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}
}
\note{
 If both \code{n_min} and \code{prop_min} are supplied, only
 \code{n_min} will be used.
}
