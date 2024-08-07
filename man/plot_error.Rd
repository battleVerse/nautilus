% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_error.R
\name{plot_error}
\alias{plot_error}
\title{Plot [location error / bearing error / range error] by [range / time]}
\usage{
plot_error(
  scenario,
  rangeCutoff,
  xTerm = "rangeToShip",
  yTerm = "locationError",
  colorTerm = "tgtAssigned",
  doFacet = FALSE,
  plotFalseTracks = FALSE
)
}
\arguments{
\item{scenario}{MUST contain assignmentData (from target_assignment())}

\item{rangeCutoff}{the position for a horizontal cutoff line. This should be equal to the cutoff you used in your analysis.  Will only draw a single line, so if the cutoff specified has 2 or 3 terms it may be less useful}

\item{xTerm}{(string) the variable plotted on the x-axis. Currently there are two options:
\itemize{
 \item{"rangeToShip"}
 \item{"time"}}}

\item{yTerm}{(string) the variable plotted on the y-axis. Currently there are three options:
\itemize{
 \item{"locationError"}
 \item{"bearingError"}
 \item{"downrangeError"}}}

\item{colorTerm}{(string) the variable to be split by color. Currently there are two options:
\itemize{
 \item{"tgtAssigned"}
 \item{"trackNum"}}}

\item{doFacet}{(default FALSE) boolean.  Determines whether the graph will also facet by colorTerm}

\item{plotFalseTracks}{(default FALSE) boolead. Determines whether graph will show fast track points or hide them}
}
\value{
ggplot object
}
\description{
This figure plots one of several choices for system error (location error, range error, bearing error) against time or range.  Can be split by target assigned or track number.
}
\examples{
myScenario=scenarioMaker::example1_scenario \%>\%
 target_assignment("point",cutoff=100)

plot_error(scenario=myScenario,
 rangeCutoff=2000,
 xTerm="rangeToShip",
 yTerm="locationError",
 colorTerm="tgtAssigned")
}
