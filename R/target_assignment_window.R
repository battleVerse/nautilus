#' @title Target assignment (square window method) - DO NOT DIRECTLY CALL THIS FUNCTION
#'
#' @description This function IS NOT intended to be called by the user.  The user is supposed to call target_assignment(method = 'windowSquare') to access this function.
#'
#' This function assigns every sensor point to a single target using the square window method, which averages the errors of nearby points. For each point, this function keeps all points belonging to the same track within the given time window. For example, for the point in track 3 at time t = 60 and windowsize = 20, this function averages the location error, downrange error, bearing error, and altitude error for all points belonging to track 3 that were recorded between time 50 and time 70. It then assigns each point to the closest target (minimum of locationError) while the rest are discarded.
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
#' @param windowSize size of the time window to be averaged over (in same units as original sensorData input, likely POSIX). At time t, all points between t - windowSize/2 and t + windowSize/2 are kept.
#' @param parallel when TRUE, will run target assignment on multiple cores
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
#' @importFrom foreach %dopar%
#'




target_assignment.window <- function(targetTrackDistance, cutoff, windowSize, parallel=FALSE){
    ###########################################################################################
    ### Description: This function is similar to the 'point' method described above, but
    ### instead of simply assigning each sensor point to the closest track, it includes the
    ### sensor points to either side (within a user-defined windowSize) to determine an
    ### average distance, and assigns it based on that. This is supposed to help smooth out
    ### the track assignment and make it more reasonable.
    ###
    ### Input: 'cutoff' (meters) is the maximum allowed distance between a sensor point (and the
    ### other points within the windowSize) and a truth target. If the distance > cutoff,
    ### then the point will be marked as clutter.
    ### 'windowSize' tells the function how much time on either side of the main point
    ### to include in the average. The units are the same as what the user provided (probably seconds)
    ###
    ### NOTE! The closest target is calculated BEFORE clutter is calculated, so a point
    ### marked as clutter will still have been included in calculating closest target


    # find the mean RMS error for each truth-track pair and which target is closest to which track


    pointIDList <- unique(targetTrackDistance$pointIndex)

    # go through each set of points (each pointIndex is a single sensor point applied to all targets)
    groupedTargetTrackDistance=targetTrackDistance %>%
        select(time,trackNum,truthID,locationError, downrangeError, bearingError, altError) %>%
        group_by(trackNum,truthID)


    if (parallel==TRUE){
        cl <- parallel::makeCluster(parallel::detectCores())
        numbercores = parallel::detectCores(logical = TRUE)
        doParallel::registerDoParallel(cl, cores = numbercores)


        # go through each set of points (each pointIndex is a single sensor point applied to all targets)
        targets_df=foreach::foreach(i = 1:length(pointIDList), .combine="rbind", .packages=c("dplyr")) %dopar% {
            # get the time for this point
            sampleRow=targetTrackDistance %>%
                filter(pointIndex == pointIDList[i]) %>% #pull out all of the rows with the right pointIndex
                slice(1)


            this_time=as.numeric(as.character(sampleRow$time))

            current_track=as.character(sampleRow$trackNum)

            # get min and max time to make next section cleaner
            min_time <- this_time - windowSize/2
            max_time <- this_time + windowSize/2



            closest_in_window <- groupedTargetTrackDistance %>%
                # pull out sensor points in the time window
                filter(time >= min_time & time <= max_time & trackNum == current_track) %>%
                # get the average location error for all sensor points in the window against each truth target
                summarise(meanlocation = mean(locationError,na.rm=TRUE),
                          downrangeError = mean(abs(downrangeError),na.rm=TRUE),# for use in get_isFalseTrack()
                          bearingError = mean(abs(bearingError),na.rm=TRUE),
                          altError = mean(abs(altError),na.rm=TRUE)) %>%
                ungroup() %>%
                group_by(trackNum) %>%
                # pull out the target with the smallest location error for each track/target pairing
                slice(which.min(meanlocation))

            closest_target <- as.character(closest_in_window$truthID)
            targetRMS = as.double(closest_in_window$meanlocation)
            downrangeErrormean <- as.double(closest_in_window$downrangeError)
            bearingErrormean <- as.double(closest_in_window$bearingError)
            altErrormean <- as.double(closest_in_window$altError)

            data.frame(pointIndex = i, closestID = closest_target,
                       meanlocation = targetRMS, locationError = targetRMS,
                       downrangeError = downrangeErrormean, bearingError = bearingErrormean,
                       altError = altErrormean)
        }
        parallel::stopCluster(cl)


    } else {
        # pre-allocate an array we'll fill up later with target assignments
        closest_target <- rep('a', length(pointIDList))
        targetRMS = rep(0.1, length(pointIDList))
        downrangeErrormean = rep(0.1, length(pointIDList))  # for use in get_isFalseTrack()
        bearingErrormean = rep(0.1, length(pointIDList))
        altErrormean = rep(0.1, length(pointIDList))

        for(i in 1:length(pointIDList)){
            if (i %% 100 == 0) {
                progress=sprintf("%d/%d steps completed\n",i,length(pointIDList))
                cat(progress)
            }


            # get the time for this point
            sampleRow=targetTrackDistance %>%
                filter(pointIndex == pointIDList[i]) %>% #pull out all of the rows with the right pointIndex
                slice(1)


            this_time=as.numeric(as.character(sampleRow$time))

            current_track=as.character(sampleRow$trackNum)

            # get min and max time to make next section cleaner
            min_time <- this_time - windowSize/2
            max_time <- this_time + windowSize/2



            closest_in_window <- groupedTargetTrackDistance %>%
                # pull out sensor points in the time window
                filter(time >= min_time & time <= max_time & trackNum == current_track) %>%
                # get the average location error for all sensor points in the window against each truth target
                summarise(meanlocation = mean(locationError,na.rm=TRUE),
                          downrangeError = mean(abs(downrangeError),na.rm=TRUE),# for use in get_isFalseTrack()
                          bearingError = mean(abs(bearingError),na.rm=TRUE),
                          altError = mean(abs(altError),na.rm=TRUE)) %>%
                ungroup() %>%
                group_by(trackNum) %>%
                # pull out the target with the smallest location error for each track/target pairing
                slice(which.min(meanlocation))

            closest_target[i] <- as.character(closest_in_window$truthID)
            targetRMS[i] = as.double(closest_in_window$meanlocation)
            downrangeErrormean[i] <- as.double(closest_in_window$downrangeError)
            bearingErrormean[i] <- as.double(closest_in_window$bearingError)
            altErrormean[i] <- as.double(closest_in_window$altError)
        }

        targets_df <- data.frame(pointIndex = pointIDList, closestID = closest_target,
                                 meanlocation = targetRMS, locationError = targetRMS,
                                 downrangeError = downrangeErrormean, bearingError = bearingErrormean,
                                 altError = altErrormean)
    }


    targets_df = targets_df %>%
        # add the isFalseTrack column, default is false
        get_isFalseTrack(cutoff) %>%
        # these columns are aggregate (weighted averaged) quantities. We'll report the unique point values
        # to the user, which we already have in targetTrackDistance, and we'll get them back when we left_join with targetTrackDistance
        select(-locationError, -downrangeError, -bearingError, -altError)


    # assign each track to a true target
    ### Step 1: left_join onto targetTrackDistance by trackNum. Adds a column with the closest track ID, which we can compare to the truthID in the row
    target_assign <- left_join(targetTrackDistance, targets_df)
    ### Step 2: only keep a row if its truthID matches the closestID, which we added in the previous step based on the trackNum
    target_assign <- target_assign[as.character(target_assign$truthID) ==
                                       as.character(target_assign$closestID) , ] %>%
        select(-closestID) # drop extra label so the output matches target_assignment.point

    # rename columns
    target_assign= ungroup(target_assign) %>% rename(tgtAssigned=truthID)

    return(target_assign)
}






