\name{dss_impute_missing}
\alias{dss_impute_missing}
\title{
  A wrapper for missing data imputation prior to sex estimation.
}
\description{
  This function allows for the imputation of missing values of the
  reference dataset, before performing secondary sex estimation. Two
  modern and efficient algorithms of missing imputation are proposed.
}
\usage{
dss_impute_missing(dtf, method = "missMDA")
}
\arguments{
  \item{dtf}{
    dataframe of reference individuals, typically as returned by
    \code{\link{dss_check_data}}.
  }
  \item{method}{
    character string, either "missMDA" or "missForest". Uses the
    corresponding R package to perform missing data imputation.
  }
}
\details{
  If \code{method = "missMDA"}, the function
  \code{\link[missMDA]{imputePCA}} from package `missMDA` is used for
    missing data imputation. If \code{method = "missForest"}, the
    function \code{\link[missForest]{missForest}} from package
    `missForest` is used. See their respective documentation for more
    theoretical and practical details.
}
\value{
  a dataframe, which is the imputed version of the input dataframe
  \code{dtf}.
}
\author{
  Frédéric Santos.
}
\seealso{
  \code{\link[missMDA]{imputePCA}}, \code{\link[missForest]{missForest}}
}
