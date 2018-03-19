#' @title Summarize amount of time each target was tracked
#'
#' @description Table showing when targets were detected
#'
#' @param scenario must contain assignmentData, the output of target_assignment()
#'
#' @return Data frame containing a summary of how much time each target was tracked.  The data frame contains the collowing variables:
#'
#' \itemize{
#'  \item{"tgt" - Target name/identifier}
#'  \item{"Total Time Tracked" - The total amount of time that the target had at least one valid track}
#'  \item{"# of Unique Tracks" - The number of segments that at one point were assigned to the target. If a track is assigned to target A, then jumps to target B, then returns to target A, then that would count as two unique tracks on target A.}
#'  \item{"# of Tracked Sections" - Total number of sections covering the track. If target A is covered by track 1, and then track 2 covers target A before track 1 ends then that will count as only one tracked section.}
#'  \item{"Time Detected" - the first time that a target is detected}
#'  \item{"Tracking Ended" - the last time that a target is detected}}
#'
#' @export
#'
#' @examples
#' summarize_time_tracked(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))


summarize_time_tracked=function(scenario){
    #user facing
    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }
    assignmentData=scenario$assignmentData
    ### Figure out when each track segment started and stopped
    trackStartStop=assignmentData %>%
        filter(isFalseTrack == FALSE) %>%
        mutate(time=as.POSIXct(time,tz="UTC",origin="1970-01-01")) %>%
        group_by(segmentNumber, tgtAssigned, trackNum) %>%
        summarise(stopTime = max(time), startTime = min(time)) %>%
        ungroup() %>%
        group_by(tgtAssigned) %>%
        mutate(segPerTgt = 1:length(segmentNumber))

    tempOverlapList=list()
    i=1
    for (target in unique(trackStartStop$tgtAssigned)){ #go through each target
        # pull out the data for this target
        thisTarget=filter(trackStartStop,tgtAssigned==target)
        # find any overlaps resulting from multiple tracks simultaneously tracking the same target
        overlaps=nautilus:::find_overlaps(startList=unlist(thisTarget$startTime),stopList=unlist(thisTarget$stopTime))
        tempOverlapList[[i]]=data.frame(tgt=target,startTime=overlaps[[1]],stopTime=overlaps[[2]],totalSegments=max(thisTarget$segPerTgt))
        i=i+1
    }

    # combine the different targets, calculate new metric timeTracked
    finalOverlaps=do.call(rbind,tempOverlapList) %>% mutate(timeTracked=stopTime-startTime)

    result=ungroup(finalOverlaps)%>%
        group_by(tgt) %>%
        summarize("Total Time Tracked"=round(sum(timeTracked),digits=2),
                  "# of Unique Tracks"=first(totalSegments),
                  "# of Tracked Sections"=n(),
                  "Time Detected"=min(startTime),
                  "Tracking Ended"=max(stopTime))

    return(result)

}


