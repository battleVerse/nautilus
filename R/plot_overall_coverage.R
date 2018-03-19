#' @title Plot overall target coverage
#'
#' @description This figure shows the overall coverage for each target over the course of the test.
#'
#' @param scenario MUST contain assignmentData (from target_assignment()) and target truth data
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_overall_coverage(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))



# a few functions to help with plotting

#user facing

plot_overall_coverage = function(scenario){


    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    if (!is.data.frame(scenario$targetTruth)){ #if targetTruth isn't present
        stop("This function is missing targetTruth data, though assignmentData exists. This shouldn't be possible.")
    }

    assignmentData=scenario$assignmentData
    truthData=scenario$targetTruth

        coverageData= assignmentData %>%
                filter(isFalseTrack == FALSE) %>%
                mutate(time=as.POSIXct(time,tz="UTC",origin="1970-01-01")) %>%
                group_by(segmentNumber, tgtAssigned, trackNum) %>%
                summarise(maxtime = max(time), mintime = min(time)) %>%
                ungroup() %>%
                group_by(tgtAssigned) %>%
                mutate(segPerTgt = 1:length(segmentNumber))

        myPlot=ggplot(coverageData) +
                geom_crossbar(aes(x = tgtAssigned, y = mintime, ymin = mintime,
                                  ymax = maxtime), fill = 'black') +
                labs(x = 'Target', y = 'Time', title = 'Target coverage compared to truth data')

        if (!(is.null(truthData))) {
                truthMinMax <- truthData %>%
                        mutate(time=as.POSIXct(time,tz="UTC",origin="1970-01-01")) %>%
                        group_by(truthID) %>%
                        summarise(maxtime = max(time), mintime = min(time))

                myPlot=myPlot+geom_crossbar(data = truthMinMax, aes(x = truthID, y = mintime, ymin = mintime,
                                                                    ymax = maxtime, group = truthID),fill="white", width = 0.2)

        }

        return(myPlot)

}
