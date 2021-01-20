\name{dss_plot_md_pattern}
\alias{dss_plot_md_pattern}
\title{
  Visualization of missing data pattern in the reference dataframe
}
\description{
  This function is a wrapper for two external functions offering
  facilities for visualizing missing data patterns:
  \code{visdat::vis_miss()} and \code{mice::md.pattern()}.
}
\usage{
dss_plot_md_pattern(ref, target, type = "map", rotate = TRUE)
}
\arguments{
  \item{ref}{
    dataframe. Reference dataset composed of individual of known sex,
    typically returned by \link{dss_check_data}.
  }
  \item{target}{
    1-row dataframe. Target individual.
  }
  \item{type}{
    character string; either \code{"map"} or \code{"pattern"}. Type of
    visualization for missing data pattern. \code{map} uses
    \code{\link[visdat]{vis_miss}}, whereas \code{pattern} uses
    \code{\link[mice]{md.pattern}}.
  }
  \item{rotate}{
    boolean. Should variable names be rotated on the plot to improve
    readability?
  }
}
\value{
  No value is returned; this function only produces a graphical output.
}
\author{
  Frédéric Santos.
}
\note{
  \code{ref} must have at least two numeric variables (missing values
  \dQuote{patterns} have no meanings for one single variable).
}
\seealso{
  \code{\link[visdat]{vis_miss}}, \code{\link[mice]{md.pattern}}.
}
