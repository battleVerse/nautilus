% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_coverage_by_target.R
\name{plot_coverage_by_target}
\alias{plot_coverage_by_target}
\title{Plot track coverage by target}
\usage{
plot_coverage_by_target(scenario, labelOffset = 0.1, textSize = 3)
}
\arguments{
\item{scenario}{MUST contain assignmentData (from target_assignment())}

\item{labelOffset}{(default = 0.1) amount to offset label names from first or last point}

\item{textSize}{size of label text}
}
\value{
ggplot object
}
\description{
This figure shows which targets were tracked by which radar track over time.
}
\examples{
plot_coverage_by_target(scenarioMaker::example1_scenario \%>\% target_assignment("point",cutoff=100))
}
