#' @title Plot tracks per target
#'
#' @description This figure shows the number of tracks per target at each time that the sensor gave a reading.
#'
#' @param scenario must contain assignmentData, the output of target_assignment()
#'
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_tracks_per_target(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))


# a few functions to help with plotting

#user facing

plot_tracks_per_target= function(scenario){

    #user facing
    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }
    assignmentData=scenario$assignmentData

    plotData=get_tracks_per_target(assignmentData)

    myPlot=ggplot(plotData,aes(x = time, y = numTracksOnTarget, colour = tgtAssigned)) +
        geom_point() +
        geom_line() +
        labs(x = 'Time', y = 'Number of tracks per target', title = 'Tracks per target') +
        scale_y_continuous(breaks=seq(round(max(plotData$numTracksOnTarget),0)))+
        facet_grid( tgtAssigned ~ .)

        return(myPlot)

}


