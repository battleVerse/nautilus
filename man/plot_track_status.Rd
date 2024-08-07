% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_track_status.R
\name{plot_track_status}
\alias{plot_track_status}
\title{Visualize when targets were tracked}
\usage{
plot_track_status(
  scenario,
  scalePoints = 1,
  textSize = 3,
  showFalseTracks = TRUE,
  hideLegend = FALSE
)
}
\arguments{
\item{scenario}{MUST contain targetTruth and assignmentData (from target_assignment())}

\item{scalePoints}{a scale to set the size of all of points in the figure. scalePoints affects all of the plotted objects. This is useful in figures with more or fewer data points}

\item{textSize}{size of label text}

\item{showFalseTracks}{plot false tracks as semi-tranparent black dots (default = TRUE)}

\item{hideLegend}{hides the legend to make the graph easier to read (default=FALSE)}
}
\value{
ggplot object
}
\description{
Plot truth data, color coded to show whether target was tracked
}
\examples{
plot_track_status(scenarioMaker::example1_scenario \%>\% target_assignment("point",cutoff=100))
}
