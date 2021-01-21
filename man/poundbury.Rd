\name{poundbury}
\alias{poundbury}
\docType{data}
\title{
  An example dataset for secondary sex estimation.
}
\description{
  This dataset has been presented in Santos (2021), and has been
  extracted from the Goldman Data Set online (Auerbach, n. d.). It
  illustrates an example of convenient data formatting for performing
  sex estimation with rdss.
}
\usage{data("poundbury")}
\format{
  A data frame with 48 observations on the following 9 variables.
  \describe{
    \item{\code{ID}}{a factor}
    \item{\code{Sex}}{a factor with levels \code{F} \code{M} \code{TBD}}
    \item{\code{LHML}}{a numeric vector}
    \item{\code{LHMLD}}{a numeric vector}
    \item{\code{LHHD}}{a numeric vector}
    \item{\code{LRML}}{a numeric vector}
    \item{\code{LFML}}{a numeric vector}
    \item{\code{LTML}}{a numeric vector}
    \item{\code{LTMLD}}{a numeric vector}
  }
}
\source{
  The corresponding CSV spreadsheet is available online at:
  \url{https://gitlab.com/f-santos/rdss/-/tree/master/inst}
}
\references{
  Auerbach, B. (n. d.) The Goldman Osteometric Data
  Set. \url{http://web.utk.edu/~auerbach/GOLD.htm}
  
  Santos, F. (2021) rdss: an R package to facilitate the use of Murail
  et al.'s (1999) approach of sex estimation in past
  populations. \emph{International Journal of Osteoarchaeology}. doi:
  10.1002/oa.2957
}
\examples{
data(poundbury)
}
\keyword{datasets}
