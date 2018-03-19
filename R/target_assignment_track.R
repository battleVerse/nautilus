#' @title Target assignment (whole track method) - DO NOT DIRECTLY CALL THIS FUNCTION
#'
#' @description This function IS NOT intended to be called by the user.  The user is supposed to call target_assignment(method = 'track') to access this function.
#'
#' This function assigns every sensor point to a single target using the whole track method, which assigns every point in an entire track to a single target. target.assignment.track() takes the output of \code{\link{target_track_distance}} which had previously calculated the location error (difference between sensor point and target) for each sensor point compared to each target. This function takes the mean location error for each track by grouping by track number and target and calculating the mean location error. It then assigns a single track to the closest target (minimum of locationError) while the rest are discarded.
#'
#' A sensor point is then determined to be a false track (isFalseTrack = TRUE) if one of several possible cutoffs is exceeded. See \code{\link{get_isFalseTrack}} for details.
#'
#' This function is not intended to be used as a stand-alone function, but rather as a part of the call to \code{\link{target_assignment}}.
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
#'  \item{meanlocation: (only in square and gauss window methods) mean distance between sensor point and target for all of the points included in the window}
#'  \item{isFalseTrack: boolean indicating whether a point is outside the cutoff and therefore considered a false track}
#'  \item{tgtXtrack: factor expressing the truthID.trackNum interaction}
#'  \item{segmentNumber: an integer counting the number of times during which a single track is assigned to a particular target}
#'  }
#'



# function that takes truth and radar data and assigns targets to tracks
# NOTE that each of the methods requires slightly different calls for the cutoffs.


target_assignment.track <- function(targetTrackDistance, cutoff){
        ###########################################################################################
        ### Description: This method assigns an entire track to the target to which it is closest
        ### over the course of the entire track. Not particularly recommended, but this method
        ### may be desirable in certain cases.
        ### IMPORTANT: This method will mark entire tracks as isFalseTrack=TRUE or FALSE, and CANNOT mark
        ### just portions of a track as clutter. So, this is all or nothing!
        ###
        ### Input: 'cutoff' (meters) is the maximum allowed AVERAGE distance between an entire sensor
        ### track and a truth target that is considered a valid assignment (if it is greater than the
        ### cutoff then the ENTIRE TRACK will be marked as clutter)
        ###########################################################################################

        # find the mean RMS error for each truth-track pair and which target is closest to which track
        closestTargets <- targetTrackDistance %>%
                group_by(trackNum, truthID) %>%
                # get the average location error for each ENTIRE track against each truth target
                summarise(meanlocation = mean(locationError,na.rm=TRUE),
                          locationError = mean(locationError,na.rm=TRUE),  # for use in get_isFalseTrack()
                          downrangeError = mean(abs(downrangeError),na.rm=TRUE),
                          bearingError = mean(abs(bearingError),na.rm=TRUE),
                          altError = mean(abs(altError),na.rm=TRUE)) %>%
                ungroup() %>%
                group_by(trackNum) %>%
                # pull out the target with the smallest location error for each track/target pairing
                slice(which.min(meanlocation)) %>%
                ungroup()

        # add the isFalseTrack column, default is false
        closestTargets <- get_isFalseTrack(closestTargets, cutoff)

        # rename and extract stuff we want
        closestTargets=select(closestTargets,c(trackNum, closestID = truthID, isFalseTrack))

        ### Now we have a dataframe (closestTargets) with 2 columns: trackNum and closestID, and we can assign each track to a true target

        ### Step 1: left_join onto targetTrackDistance by trackNum. Adds a column with the closest track ID, which we can compare to the truthID in the row
        target_assign <- left_join(targetTrackDistance, closestTargets, by="trackNum")
        ### Step 2: only keep a row if its truthID matches the closestID, which we added in the previous step based on the trackNum
        target_assign <- target_assign[as.character(target_assign$truthID) ==
                                               as.character(target_assign$closestID) , ] %>%
                select(-closestID) # drop extra label so the output matches target_assignment.point

        # rename columns
        target_assign= ungroup(target_assign) %>% rename(tgtAssigned=truthID)

        return(target_assign)
}

