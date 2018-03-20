
#' @title Get number of tracks assigned to each target over time
#'
#' @description Takes assignmentData, and figures out how many tracks were assigned to each target over time. It does this by looking for when track segments start and stop. NOTE! false tracks will NOT interrupt a track, so if a target is tracked at time t=1 and t=10 (with false tracks in between), this will assume it was tracked the whole time
#'
#' @param assignmentData the output of target_assignment()
#' @param truthData the input truth data (default=NULL). If you don't pass this, the return is at sensor times. If you DO pass this, return is at truth data times.

#'
#' @return dataframe containing lat, lon, alt, heading, time, truthID, and numTracksOnTarget
#'
#' @importFrom data.table setDT
#'
#' @keywords internal
#'




get_tracks_per_target= function(assignmentData,truthData=NULL){
    #internal function, approved for non-scenario input

    allTimes <- expand.grid(time = unique(filter(assignmentData, isFalseTrack == FALSE)$time),
                            tgtAssigned = unique(filter(assignmentData, isFalseTrack == FALSE)$tgtAssigned))

    trackData = assignmentData %>%
        ungroup() %>%
        filter(isFalseTrack == FALSE) %>%
        group_by(segmentNumber, tgtAssigned, trackNum) %>%
        # get start and stop times for each segment
        summarise(stopTime = max(time), startTime = min(time)) %>%
        ungroup() %>%
        tidyr::gather('trackEvent', 'time', c(stopTime, startTime)) %>%
        # put in chronological order (with startTime before stopTime in case there is a single point)
        arrange(time, trackEvent) %>%
        group_by(tgtAssigned) %>%
        #go through the list - every time a new track shows up, +1, every time one goes away, -1
        mutate(numTracksOnTarget=cumsum(ifelse(trackEvent=="startTime",1,-1))) %>%
        # recombine with data (each time there was a measurement x number of tgts) to fill out the graph
        right_join(allTimes) %>%
        arrange(tgtAssigned, time) %>%
        # fill in NAs with value above
        tidyr::fill(numTracksOnTarget, .direction = 'down') %>%
        # Any that are left are times before tgt was detected.  These count as 0
        tidyr::replace_na(replace = list(numTracksOnTarget = 0))

    ### now we have the # of tracks on each target at the sensorData times ###

    if (is.data.frame(truthData)){ #if we have truthData, then give the answer in terms of truth data times
        tempList=list()
        i=1

        for (target in (unique(truthData$truthID))) { #go through each target

            thisTargetTruth=filter(truthData,truthID==target) #pull out data for this particular target
            thisTargetAssignments=filter(trackData,tgtAssigned==target) #pull out the assignments for this target

            #use a fancy data.table call that will go through each truth time and get the nearest earlier numTracksOnTarget from the sensor-time-based calculation
            data.table::setDT(thisTargetTruth)[, numTracksOnTarget := data.table::setDT(thisTargetAssignments)[thisTargetTruth, numTracksOnTarget, on = "time", roll = Inf]]

            tempList[[i]]=thisTargetTruth
            i=i+1

        }

        coverageData=do.call(rbind,tempList) %>%
        tidyr::replace_na(replace = list(numTracksOnTarget = 0)) #replace NAs with 0s

        return(coverageData)

    } else{ ### if we DIDN'T give truth data, just return it at the sensor data times
        return(trackData)
    }
}
