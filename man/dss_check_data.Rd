\name{dss_check_data}
\alias{dss_check_data}
\title{
  Test whether your data file has the required format for use in \code{rdss}.
}
\description{
  This is the mandatory first step when using \code{rdss}. This
  function performs several checks for possible formatting mistakes, and
  returns a dataframe with \dQuote{normalized} reformatted contents.
}
\usage{
dss_check_data(dtf, sex, females, males,
               tbd, rm_empty_rows = FALSE,
               mode = "console")
}
\arguments{
  \item{dtf}{
    previously imported dataframe. Warning: at this stage, individual
    IDs \bold{must} be indicated as a character vector in the first
    column of the dataframe, and not directly as custom row names (see
    Notes below, see also the package vignette).
  }
  \item{sex}{
    character string; name of the column filled with the sex of
    individuals in the dataframe \code{dtf}.
  }
  \item{females}{
    character string; abbreviation used for female individuals in the
    sex column.
  }
  \item{males}{
    character string; abbreviation used for male individuals in the
    sex column.
  }
  \item{tbd}{
    character string; abbreviation used for target individuals in the
    sex column.
  }
  \item{rm_empty_rows}{
    boolean. Should individuals with no value at all be removed from the
    dataframe?
  }
  \item{mode}{
    for internal use in the shiny app only; final users in R scripts
    should stick with the default value, \code{console}.
  }
}
\details{
  This functions performs a series a six checks on the dataframe
  \code{dtf}, and displays explicit and useful error messages when
  formatting mistakes are found (duplicates in row names, typos in the
  \code{Sex} column, etc.).

  Also, it returns a dataframe whose the contents are
  \dQuote{standardized}:
  \itemize{
    \item the sex column is automatically renamed as \code{Sex}
    \item the sex factor is then releveled: females now match the level
  \code{F}, males now match the level \code{M}, target individuals now
  match the level \code{TBD}. This will facilitate and standardize the
  presentation of classification results for all users.
  }
}
\value{
  A dataframe with same contents as \code{dtf}, but whose sex factor has
  possibly be renamed and releveled (see Details).
}
\author{
  Frédéric Santos
}
\note{
  Please note that the input dataframe \code{dtf} must \code{not} have
  row names, i.e. must not have been imported using the argument
  \code{row.names = 1} from \code{read.csv()}, for instance. Instead,
  its first column must be a character vector filled with individual
  IDs. This character vector will be transformed as row names (after
  several checks) by this function. See the package vignette for
  additional details.
}
