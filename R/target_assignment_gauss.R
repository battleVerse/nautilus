#' @title Target assignment (Gaussian window method) - DO NOT DIRECTLY CALL THIS FUNCTION
#'
#' @description This function IS NOT intended to be called by the user.  The user is supposed to call target_assignment(method = 'windowGauss') to access this function.  See \code{\link{target_assignment}} for additional details.
#'
#' The Gaussian window method (method = "windowGauss") is closely related to the square window method. Instead of taking an average of points within a given window for purposes of assigning a sensor point to a target, the gauss window method takes a normally-weighted average, with the windowSize argument as the standard deviation in time of the distribution. For each point NAUTILUS:
#'
#' 1. Calculates the weight for all points based on their time, treating the time at the given point as the center of the Gaussian (i.e., if the given point in a track is at time 60 and the windowSize parameter of 10 gives weights for all of the points as determined by dnorm( allTimesInTrack , mean = 60, sd =10) )
#'
#' 2. The average distance between a track and a target is calculated as the weighted mean of the distance between all of the points in the track compared to the target at each time, weighted by the Gaussian function determined above
#'
#' 3. NAUTILUS assigns the track to the closest target or specifies that it is considered a false track if it is outside the user specified cutoff.  See \code{\link{get_isFalseTrack}} for additional details on the cutoff
#'
#' @param targetTrackDistance the output of target_track_distance()
#' @param cutoff numeric vector of length 1, 2, or 3 specifying the conditions under which a point is determined to be a false track. The different lengths imply different cutoff conditions:
#' \itemize{
#'  \item{One number (vector length 1): points are false tracks if the location error (difference between sensor point and closest target) is greater than this value in meters}
#'  \item{Two numbers (vector length 2): first number is cutoff in meters of the 2D error (sqrt(locationError^2 - altError^2)). Second number is cutoff in meters of the absolute value of the altitude error (sensor altitude reading - true target altitude)}
#'  \item{Three numbers (vector length 3): first number is range error in meters, second number is bearing error in degrees, third number is altitude error in meters}}
#'
#' @param windowSize standard deviation of the weights to be used in the weighted mean (in same units as original sensorData input, likely POSIX).
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
#'  \item{meanLocationError: (only in square and gauss window methods) mean distance between sensor point and target for all of the points included in the window}
#'  \item{isFalseTrack: boolean indicating whether a point is outside the cutoff and therefore considered a false track}
#'  \item{tgtXtrack: factor expressing the truthID.trackNum interaction}
#'  \item{segmentNumber: an integer counting the number of times during which a single track is assigned to a particular target}
#'  }
#'
#'
#' @importFrom foreach %dopar%


# function that takes truth and radar data and assigns targets to tracks
# NOTE that each of the methods requires slightly different calls for the cutoffs.



target_assignment.gauss <- function(targetTrackDistance, cutoff, windowSize, parallel=FALSE){
    ##############################################################################################################
    ### Description: This method assigns each point by weighting all of th eneighboring points
    ###  by a gaussian function (weighted mean)
    ###
    ### Input: 'cutoff' (meters) is the maximum allowed distance between a sensor point and a truth target
    ### that is considered a valid assignment (if it is greater that cutoff then the point will be marked as clutter)
    ### Input: 'windowSize' is the standard deviation of the gaussian weight function
    ##############################################################################################################

    pointIDList <- unique(targetTrackDistance$pointIndex)



    # work with smaller data frame
    simpleTargetTrackDistance=targetTrackDistance %>%
        select(time,trackNum,truthID,locationError, downrangeError, bearingError, altError) %>%
        group_by(truthID)

    ############################
    ### Parallel Calculation ###
    ############################

    if (parallel == TRUE){

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

            # get weighted mean
            closest_in_window <- simpleTargetTrackDistance %>%
                # use only data from this track
                filter(trackNum == current_track) %>%
                # regroup (not just to be careful, it might need it after using select() and filter())
                # get the weighted average location error for all sensor points in the window against each truth target
                mutate(gaussWeights = stats::dnorm(time, mean = this_time, sd = windowSize)) %>%
                summarise(meanLocationError = stats::weighted.mean(locationError, gaussWeights, na.rm=TRUE),
                          downrangeError = stats::weighted.mean(abs(downrangeError),gaussWeights, na.rm=TRUE),#for use in get_isFalseTrack()
                          bearingError = stats::weighted.mean(abs(bearingError), gaussWeights, na.rm=TRUE),
                          altError = stats::weighted.mean(abs(altError), gaussWeights, na.rm=TRUE)) %>%
                ungroup() %>%
                # pull out the target with the smallest location error for each track/target pairing
                slice(which.min(meanLocationError))

            closest_target <- as.character(closest_in_window$truthID)
            targetRMS = as.double(closest_in_window$meanLocationError)
            downrangeErrormean <- as.double(closest_in_window$downrangeError)
            bearingErrormean <- as.double(closest_in_window$bearingError)
            altErrormean <- as.double(closest_in_window$altError)

            data.frame(pointIndex = i, closestID = closest_target,
                       meanLocationError = targetRMS, locationError = targetRMS,
                       downrangeError = downrangeErrormean, bearingError = bearingErrormean,
                       altError = altErrormean)



        }

        parallel::stopCluster(cl)
    } else {

        ###############################
        ### Single Core Calculation ###
        ###############################

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

            # get weighted mean
            closest_in_window <- simpleTargetTrackDistance %>%
                # use only data from this track
                filter(trackNum == current_track) %>%
                # regroup (not just to be careful, it might need it after using select() and filter())
                # get the weighted average location error for all sensor points in the window against each truth target
                mutate(gaussWeights = stats::dnorm(time, mean = this_time, sd = windowSize)) %>%
                summarise(meanLocationError = stats::weighted.mean(locationError, gaussWeights, na.rm=TRUE),
                          downrangeError = stats::weighted.mean(abs(downrangeError),gaussWeights, na.rm=TRUE),#for use in get_isFalseTrack()
                          bearingError = stats::weighted.mean(abs(bearingError), gaussWeights, na.rm=TRUE),
                          altError = stats::weighted.mean(abs(altError), gaussWeights, na.rm=TRUE)) %>%
                ungroup() %>%
                # pull out the target with the smallest location error for each track/target pairing
                slice(which.min(meanLocationError))

            closest_target[i] <- as.character(closest_in_window$truthID)
            targetRMS[i] = as.double(closest_in_window$meanLocationError)
            downrangeErrormean[i] <- as.double(closest_in_window$downrangeError)
            bearingErrormean[i] <- as.double(closest_in_window$bearingError)
            altErrormean[i] <- as.double(closest_in_window$altError)
        }

        targets_df <- data.frame(pointIndex = pointIDList, closestID = closest_target,
                                 meanLocationError = targetRMS, locationError = targetRMS,
                                 downrangeError = downrangeErrormean, bearingError = bearingErrormean,
                                 altError = altErrormean)
    }



    targets_df=targets_df %>%
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


