#' @title Plot track coverage by target
#'
#' @description This figure shows which targets were tracked by which radar track over time.
#'
#' @param scenario MUST contain assignmentData (from target_assignment())
#' @param labelOffset (default = 0.1) amount to offset label names from first or last point
#' @param textSize size of label text
#'
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_coverage_by_target(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))



# a few functions to help with plotting

#user facing


plot_coverage_by_target = function(scenario, labelOffset=.1, textSize = 3){



    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    assignmentData=scenario$assignmentData


    coverageData=assignmentData %>%
        filter(isFalseTrack == FALSE) %>%
        mutate(time=as.POSIXct(time,tz="UTC",origin="1970-01-01")) %>%
        group_by(segmentNumber, tgtAssigned, trackNum) %>%
        summarise(maxtime = max(time), mintime = min(time)) %>%
        ungroup() %>%
        group_by(tgtAssigned) %>%
        mutate(segPerTgt = 1:length(segmentNumber))


    myPlot=ggplot(coverageData) +
        geom_crossbar(aes(x = segPerTgt, y = mintime, ymin = mintime, ymax = maxtime,
                          fill = trackNum)) +
        # Facets and orders based on alphabetical order
        facet_wrap(~tgtAssigned) +
        # Add track number labels to each segment (in case too many colors to label each track)
        geom_label(aes(x = segPerTgt, y = (mintime - labelOffset), label = trackNum), size = textSize,alpha=.7) +
        labs(x = '', y = 'Time', title = 'Target coverage', fill = 'Sensor track number') +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

    return(myPlot)
}
