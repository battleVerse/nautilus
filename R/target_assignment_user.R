#' @title Target assignment (whole track method) - DO NOT DIRECTLY CALL THIS FUNCTION
#'
#' @description This function IS NOT intended to be called by the user.  The user is supposed to call target_assignment(method = 'user') to access this function.
#'
#' This function assigns every sensor point to a single target using the whole track method, which assigns every point in an entire track to a single target. target.assignment.track() takes the output of target_track_distance() which had previously calculated the location error (difference between sensor point and target) for each sensor point compared to each target. This function takes the mean location error for each track by grouping by track number and target and calculating the mean location error. It then assigns a single track to the closest target (minimum of locationError) while the rest are discarded.
#'
#' A sensor point is then determined to be a false track (isFalseTrack = TRUE) if one of several possible cutoffs is exceeded. See get_isFalseTrack() for details.
#'
#' This function is not intended to be used as a stand-alone function, but rather as a part of the call to target_assignment().
#'
#' @param assignmentData the output of target_assignment()
#' @param userAssignedVector a vector of values with the names of targets each point will be assigned to. Must be the same length as the number of observations in assignmentData
#'
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
#'  \item{meanlocation: (only in square and gauss window methods) mean distance between sensor point and target for all of the points included in the window}
#'  \item{isFalseTrack: boolean indicating whether a point is outside the cutoff and therefore considered a false track}
#'  \item{tgtXtrack: factor expressing the truthID.trackNum interaction}
#'  \item{segmentNumber: an integer counting the number of times during which a single track is assigned to a particular target}
#'  }
#'
#'
#' @examples
#' numPoints=dim(scenarioMaker::example1_scenario$sensorData)[1]
# target_assignment(scenarioMaker::example1_scenario,method='user', userAssignedVector = rep('A', times = numPoints))

# function that takes truth and radar data and assigns targets to tracks
# NOTE that each of the methods requires slightly different calls for the cutoffs.



target_assignment.user <- function(assignmentData, userAssignedVector){
        ###############################################################################
        ### Description: The user can provide their own track assignment if they have
        ### their own method for doing this.
        ###
        ### Input: a list of target names. If the first element is "A", then the row
        ### of pointIndex=1 with truthID="A" will be kept and the others will be dropped
        ###############################################################################

        # user needs to specify a numeric vector the same length as the initial radar data
        #  (i.e., unique(assignmentData$pointIndex))


        userAssignment <- data.frame(pointIndex = unique(assignmentData$pointIndex),
                                     truthID = userAssignedVector)

        target_assign <- inner_join(assignmentData, userAssignment)

        # rename columns
        target_assign= ungroup(target_assign) %>% rename(tgtAssigned=truthID)

        return(target_assign)
}





