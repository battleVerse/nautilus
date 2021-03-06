% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_overall_coverage.R
\name{plot_overall_coverage}
\alias{plot_overall_coverage}
\title{Plot overall target coverage}
\usage{
plot_overall_coverage(scenario)
}
\arguments{
\item{scenario}{MUST contain assignmentData (from target_assignment()) and target truth data}
}
\value{
ggplot object
}
\description{
This figure shows the overall coverage for each target over the course of the test.
}
\examples{
plot_overall_coverage(scenarioMaker::example1_scenario \%>\% target_assignment("point",cutoff=100))
}
