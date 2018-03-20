#' @title Target assignment (point method) - DO NOT DIRECTLY CALL THIS FUNCTION
#'
#' @description This function IS NOT intended to be called by the user.  The user is supposed to call target_assignment(method = "point") to access this function.
#'
#' This function assigns every sensor point to a single target using the point method, which treats every point individually and assigns it to the closest target. The input is grouped by pointIndex and then only the closest target (minimum of locationError) is kept while the rest are discarded.
#'
#' A target is then determined to be a false track (isFalseTrack = TRUE) if one of several possible cutoffs is exceeded. See \code{\link{get_isFalseTrack}} for details.
#'
#' This function is not intended to be used as a stand-alone function, but rather as a part of the call to \code{\link{target_assignment}}. This function is called by target_assignment() when the user specifies method = "point".
#'
#' @param targetTrackDistance the output of target_track_distance()
#' @param cutoff numeric vector of length 1, 2, or 3 specifying the conditions under which a point is determined to be a false track. The different lengths imply different cutoff conditions:
#' \itemize{
#'  \item{One number (vector length 1): points are false tracks if the location error (difference between sensor point and closest target) is greater than this value in meters}
#'  \item{Two numbers (vector length 2): first number is cutoff in meters of the 2D error (sqrt(locationError^2 - altError^2)). Second number is cutoff in meters of the absolute value of the altitude error (sensor altitude reading - true target altitude)}
#'  \item{Three numbers (vector length 3): first number is range error in meters, second number is bearing error in degrees, third number is altitude error in meters}}
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
#'  \item{meanLocationError: (only in square and gauss window methods) mean distance between sensor point and target for all of the points included in the window}
#'  \item{isFalseTrack: boolean indicating whether a point is outside the cutoff and therefore considered a false track}
#'  \item{tgtXtrack: factor expressing the truthID.trackNum interaction}
#'  \item{segmentNumber: an integer counting the number of times during which a single track is assigned to a particular target}
#'  }
#'


# function that takes truth and radar data and assigns targets to tracks
# NOTE that each of the methods requires slightly different calls for the cutoffs.


target_assignment.point <- function(targetTrackDistance, cutoff){
        ##############################################################################################################
        ### Description: Simplest system, this method assigns each sensor point to the truth target to which it is closest
        ### (we're trying each target on for size and finding the closest)
        ### IMPORTANT: After this step, we filter out sensor points that are further than the cutoff distance
        ### marking them as isFalseTrack = TRUE. Basically, despite sensor point Y being closest to target X, it is
        ### still so far away that we regard it as clutter
        ###
        ### Input: 'cutoff' (meters) is the maximum allowed distance between a sensor point and a truth target
        ### that is considered a valid assignment (if it is greater that cutoff then the point will be marked as clutter)
        ##############################################################################################################

        targetAssign = stats::na.omit(targetTrackDistance) %>%
                # group by the radar point (i.e. if there are 4 truth targets, then each radar point will be replicated 4 times in pointIndex)
                group_by(pointIndex) %>%
                # find the smallest error for each set of pointIndex (e.g. if there are 4 targets, go through each individual radar point find which of the 4 has the smallest error and keep only that one)
                slice(which.min(locationError))

        # add the isFalseTrack column, default is false
        targetAssign <- get_isFalseTrack(targetAssign, cutoff)

        # rename columns
        targetAssign= ungroup(targetAssign) %>% rename(tgtAssigned=truthID)

        return(targetAssign)
}

