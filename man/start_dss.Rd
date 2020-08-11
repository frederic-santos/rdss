\name{start_dss}
\alias{start_dss}
\title{
  An R-Shiny application for making sex estimation easier
}

\description{
  Launches the graphical user interface (GUI)
}

\usage{
start_dss()
}

\note{
  The R console is not available when the GUI is active. To exit the
  GUI, type Echap (on MS Windows systems) or Ctrl+C (on Linux systems)
  in the R console.

  On 14-inch (or smaller) screens, for convenience, it may be necessary
  to decrease the zoom level of your web browser and/or to turn on
  fullscreen mode.
}

\details{
  The GUI of rdss is completely autonomous: reading the data file and
specifying the parameters of the analysis are done through the
interface. Once the dataset is loaded, the output reacts dynamically to
any change in the analysis settings.
}

\value{
  The function returns no value by itself, but all results can be
  individually downloaded through the graphical interface.
}

\references{
  Murail, P., Bruzek, J. and Braga, J. (1999) A new approach
  to sexual diagnosis in past populations. Practical adjustments from
  Van Vark's procedure. \emph{International Journal of
  Osteoarchaeology}, \bold{9}, 39--53. doi:
  10.1002/(SICI)1099-1212(199901/02)9:1<39::AID-OA458>3.0.CO;2-V
}

\author{
  Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}
}

\examples{
## Launch the GUI:
\dontrun{ start_dss() }
}
