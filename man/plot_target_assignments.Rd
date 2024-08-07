% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_target_assignments.R
\name{plot_target_assignments}
\alias{plot_target_assignments}
\title{Plot target assignments}
\usage{
plot_target_assignments(
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
Figure that shows which sensor points were assigned to which targets.  Thin lines connect the points the sensor point to the interpolated true position at that time.  It also indicates which points were determined to be false tracks.  This figure is very useful to show the differences between assignment methods but can be difficult to read when there are many targets and tracks in one figure. The figure plots all of the targets as well as all of the tracks that were not determined to be false tracks. False track points are shown as black dots.
The first point (in time) of each target is labeled with the name of the target.  The last recorded sensor point for each track is labeled with the number.
}
\examples{
plot_target_assignments(scenarioMaker::example1_scenario \%>\% target_assignment("point",cutoff=100))
}
