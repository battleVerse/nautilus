#' @title Second-pass method of target assignment
#'
#' @description There are several reasons why one may want to run target_assignment() twice.  For example, for large data sets with a large number of false tracks (clutter), running window methods may be very slow.  It may be possible to speed up computation by first running a fast method of target_assignment() ()point or whole track) with a large cutoff value, then returning to run the slower window method on the reduced data set.  As another example, a user may want to ensure that they keep all points within a track (as the method = 'wholeTrack' would do), however they also want a track to be able to switch targets (as the other methods can).  In this case, the user might want to run the whole track method first with a medium-sized cutoff to remove all of the false tracks, and then return and use one of the other methods with a large cutoff to ensure that none of the remaining points are thrown out while still allowing them to switch targets.
#'
#' This function can be used to easily run \code{\link{target_assignment}} for a second pass after it has already been run once.  The first pass should be designed to remove as much of the false tracks as possible without interfering with data that the user would want to keep.  It will then run only those points NOT determined to be false tracks through the second pass.  The points eliminated in the first pass will be included in the final data frame so that no data points are lost during this process (unless the user specifies otherwise, perhaps to reduce the overall size of the data for plotting).
#'
#' The first pass should properly be a faster method to properly take advantage of the two-pass technique, therefore we recommend using method='point' or method='wholeTrack' for the first pass before using this function.
#'
#'
#'
#'
#'
#' @param scenario scenario must contain the output of target_track_assignment() (the result of the first pass)
#'
#' @param method string specifying which subfunction to call. Options include:
#' \itemize{
#'  \item{"point": point method of target assignment. Calls target_assignment.point().  Requires additional parameters "cutoff".  See \code{\link{target_assignment.point}} for additional details}
#'  \item{"wholeTrack": whole track method of target assignment. Calls target_assignment.track().  Requires additional parameters "cutoff".  See \code{\link{target_assignment.track}} for additional details}
#'  \item{"windowSquare": square window method of target assignment. Calls target_assignment.window(), requires additional parameters "cutoff" and "windowSize".  optional parameter "parallel" will parallelize computation across multiple cores.  default for this is FALSE.  See \code{\link{target_assignment.window}} for additional details}
#'  \item{"windowGauss": Gaussian window method of target assignment. Calls target_assignment.gauss().  Requires additional parameters "cutoff" and "windowSize".  optional parameter "parallel" will parallelize computation across multiple cores.  default for this is FALSE.  See \code{\link{target_assignment.gauss}} for additional details}
#'  \item{"user": user-specified method of target assignment. Calls target_assignment.user().  Requires additional parameters "userAssignedVector".  See \code{\link{target_assignment.user}} for additional details}
#' }
#'
#' @param excludeFirstPass (Default = FALSE) determines whether to include the false tracks from the first pass in the final data frame.
#'
#' @param ... additional commands to be passed to each subfunction ( target.assignment.XXXXX() ). For example, all of the subfunctions require a cutoff parameter and the tho window methods (method = "windowSquare" or "windowGauss") each require the window size (windowSize) to be specified.  See \code{\link{target_assignment}} for addiitonal details.
#'
#' @return A data frame with the same number of observations as the sensor data that was originally passed to target_track_distance(). Each sensor point in the original data set has been assigned to a target using the method specified by the user, and the output data provides the following summary statistics:
#' \itemize{
#'  \item{lonError: difference in longitude between the sensor point and assigned target at the time of the sensor point}
#'  \item{latError: difference in latitude between the sensor point and assigned target at the time of the sensor point}
#'  \item{altError: difference in altitude between the sensor point and assigned target at the time of the sensor point}
#'  \item{bearingToTarget: bearing (azimuth) to target from ownship at the time of the sensor point}
#'  \item{trackNum: the track number associated with this sensor data point}
#'  \item{tgtAssigned: the ID of the target assigned to this sensor point}
#'  \item{locationError: distance between sensor point and target at the given time}
#'  \item{pointIndex: index referring to which of the original sensor data points this target-track pair refers. Necessary for comparisons in target_assignment()}
#'  \item{time: the time that this sensor point was recorded}
#'  \item{bearingError: difference in bearing between the sensor point and target at the time of the sensor point}
#'  \item{downrangeError: difference in range to ownship between the sensor point and target}
#'  \item{lon: longitude of the sensor point}
#'  \item{lat: latitude of the sensor point}
#'  \item{alt: altitude of the sensor point}
#'  \item{rangeToShip: range from target to ownship at the time of the sensor data point}
#'  \item{targetAspect: target aspect (as seen from ownship) at the time of the sensor data point}
#'  \item{meanLocationError: (only in square and gauss window methods) mean distance between sensor point and target for all of the points included in the window}
#'  \item{isFalseTrack: boolean indicating whether a point is outside the cutoff and therefore considered a false track}
#'  \item{tgtXtrack: factor expressing the truthID.trackNum interaction}
#'  \item{segmentNumber: an integer counting the number of times during which a single track is assigned to a particular target}
#'  }
#'
#' @import scenarioMaker
#'
#' @export
#'
#' @examples
#'

#'
#' # Example 1: speeding up the gaussian window method by
#' # eliminating some false tracks in the first pass
#'
#'     target_assignment(scenario=scenarioMaker::example2_scenario,
#'      method = 'point', cutoff = 300) %>%
#'     target_assignment_secondpass(method = 'windowGauss',
#'      cutoff = 100, windowSize = 30)
#'
#'
#' # Example 2: Removing all whole tracks that are false tracks
#' # and keeping all of the data points for tracks that are not
#' # false tracks.  In the second pass, a large cutoff prevents
#' # any further points from being removed while allowing for
#' # tracks to switch targets
#'
#'     target_assignment(scenario=scenarioMaker::example2_scenario,
#'      method = 'wholeTrack', cutoff = 200) %>%
#'     target_assignment_secondpass(method = 'windowGauss',
#'      cutoff = 5000, windowSize = 30)



# function that takes truth and radar data and assigns targets to tracks
# NOTE that each of the methods requires slightly different calls for the cutoffs.

target_assignment_secondpass = function(scenario, method, excludeFirstPass = FALSE, ...) {

    ### Make sure a first pass was run ###
    if (any(is.null(scenario$assignmentData))){
        stop("'assignmentData' does not exist in your input scenario. Before you can run a second pass, you must first run a regular target assignment with 'target_assignment'.")
    }

    ### Pull out the good (non-false) tracks from the previous assignment ###
    newSensorData <- scenario$assignmentData %>%
        filter(isFalseTrack == FALSE) %>%
        select(time, trackNum, lon, lat, alt)

    if (nrow(newSensorData)==0){ #is it empty?
        warning("target_assignment_secondpass is going to fail - it looks like the first pass marked EVERY track as a false track.\n")
    }

    ### Recalculate targetTrackDistance for the new (smaller) set of tracks ###
    newTargetTrackDistance=scenarioMaker::target_track_distance(truthData = scenario$targetTruth,
                                                                sensorData = newSensorData,
                                                                ownShipData = scenario$ownShipTruth)


    tmpScenario=scenario #make a temporary copy of the scenario
    tmpScenario$targetTrackDistance=newTargetTrackDistance #We don't keep this in 'scenario', because overwriting newTargetTrackDistance would screw up future target_assignment calls (they would all use the subset of 'good' tracks determined by the 1st pass)

    ### Run assignment using the updated targetTrackDistance ###
    tmpScenario <- target_assignment(scenario=tmpScenario, method,...)




    # if we exclude the first pass, we overwrite the old assignmentData with the new assignmentData (dropping all the 'false' tracks from the first pass)
    if (excludeFirstPass) {
        scenario$assignmentData=tmpScenario$assignmentData
    } else{
        firstPassFalseTracks=filter(scenario$assignmentData, isFalseTrack == TRUE)

        ### cast things to character to avoid warnings ###
        firstPassFalseTracks= firstPassFalseTracks %>%
            mutate(trackNum = as.character(trackNum),
                   tgtAssigned= as.character(tgtAssigned),
                   tgtXtrack = as.character(tgtXtrack),
                   segmentNumber = as.character(tgtXtrack))

        tmpScenario$assignmentData= tmpScenario$assignmentData %>%
            mutate(trackNum = as.character(trackNum),
                   tgtAssigned= as.character(tgtAssigned),
                   tgtXtrack = as.character(tgtXtrack),
                   segmentNumber = as.character(tgtXtrack))


        ### combine data frames and put things back into factors ###
        combinedassignmentData=bind_rows(firstPassFalseTracks,tmpScenario$assignmentData) %>%
            mutate(trackNum = as.factor(trackNum),
                   tgtAssigned= as.factor(tgtAssigned),
                   tgtXtrack = as.factor(tgtXtrack),
                   segmentNumber = as.factor(tgtXtrack))



        scenario$assignmentData=combinedassignmentData
    }

    ### Create a record of the parameters used ###
    dots=list(...)
    scenario$assignmentParameters$secondPassMethod=method

    if (tolower(method) != "user"){ #all methods but "user" have a cutoff
        scenario$assignmentParameters$secondPassCutoff=dots$cutoff
    } else {
        scenario$assignmentParameters$secondPassCutoff=NA
    }

    if (tolower(method) %in% c("windowsquare","windowgauss")){ #window methods
        scenario$assignmentParameters$secondPassWindow=dots$window
    } else {
        scenario$assignmentParameters$secondPassWindow=NA
    }


    scenario$assignmentParameters$excludeFirstPass=excludeFirstPass


    ### Return
    return(scenario)

}



