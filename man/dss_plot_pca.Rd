\name{dss_plot_pca}
\alias{dss_plot_pca}
\title{
  A wrapper for plotting a principal component analysis with confidence
  ellipses
}
\description{
  Two PCA plots (variables and individuals) are displayed for the
  reference dataset. On the individuals plot, confidence ellipses for
  both sexes can be added. The target individual is particularly
  highlighted.
}
\usage{
dss_plot_pca(ref, imputed_ref, target,
             ellipses = c("none", "classical", "robust"),
             labels = FALSE)
}
\arguments{
  \item{ref}{
    dataframe of reference individuals, typically returned by
    \code{\link{dss_check_data}}.
}
  \item{imputed_ref}{
    imputed dataframe of reference individuals, typically returned by
    \code{\link{dss_impute_missing}}.
  }
  \item{target}{
    1-row dataframe, target individual.
  }
  \item{ellipses}{
    character string; type of confidence ellipses to be displayed.
}
  \item{labels}{
    boolean: should all individuals IDs be displayed on the plot?
  }
}
\details{
  The PCA is computed after standardization of variables (all variables
  are scaled to unit variance upstream).
}
\value{
  No value is returned.
}
\author{
  Frédéric Santos.
}
\note{
   This function is only a wrapper with quite limited facilities for
   customizing the plot. It is still possible to compute and plot the
   PCA directly with \code{\link[FactoMineR]{PCA}} for more options.
}
\seealso{
   \code{\link[FactoMineR]{PCA}}
}
