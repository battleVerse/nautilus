#' @title Target assignment
#'
#' @description This is the function that determines which target each sensor point is assigned to and whether the point can be determined to be a false track. The call to target_assignment() is largely a switch that calls the target assignment subfunctions described below. After calling the subfunction it rearranges the target IDs into alphabetical order for plotting purposes and then returns a data frame with the same number of observations as the original sensor data input.
#'
#' As an example, if there were four targets (A through D) that were originally input into \code{\link{target_track_distance}}, the output of that function included the distance of every sensor point (with unique number pointIndex) to all four targets. Therefore, if sensorData had N observations, the output of target_track_distance() has 4N observations, and the output of target_assignment() has N observations.
#'
#' @param scenario a scenario containing either the output of target_track_distance(), or its component pieces (targetTruth, ownShipTruth, sensorData)
#' @param method string specifying which subfunction to call. Options include:
#' \itemize{
#'  \item{"point": point method of target assignment. Calls target_assignment.point().  Requires additional parameters "cutoff".  See \code{\link{target_assignment.point}} for additional details}
#'  \item{"wholeTrack": whole track method of target assignment. Calls target_assignment.track().  Requires additional parameters "cutoff".  See \code{\link{target_assignment.track}} for additional details}
#'  \item{"windowSquare": square window method of target assignment. Calls target_assignment.window(), requires additional parameters "cutoff" and "windowSize".  optional parameter "parallel" will parallelize computation across multiple cores.  default for this is FALSE.  See \code{\link{target_assignment.window}} for additional details}
#'  \item{"windowGauss": Gaussian window method of target assignment. Calls target_assignment.gauss().  Requires additional parameters "cutoff" and "windowSize".  optional parameter "parallel" will parallelize computation across multiple cores.  default for this is FALSE.  See \code{\link{target_assignment.gauss}} for additional details}
#'  \item{"user": user-specified method of target assignment. Calls target_assignment.user().  Requires additional parameters "userAssignedVector".  See \code{\link{target_assignment.user}} for additional details}
#' }
#' @param ... additional commands to be passed to each subfunction ( target.assignment.XXXXX() ). For example, all of the subfunctions require a cutoff parameter and the tho window methods (method = "windowSquare" or "windowGauss") each require the window size (windowSize) to be specified.
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
#'  @import scenarioMaker
#'
#' @export
#'
#' @examples
#' cutoff= 2000
#' windowSize = 35
#' myScenario=target_assignment(scenario=scenarioMaker::example2_scenario,
#'  method='windowGauss',
#'  cutoff=cutoff,
#'  windowSize=windowSize)




# function that takes truth and radar data and assigns targets to tracks
# NOTE that each of the methods requires slightly different calls for the cutoffs.

target_assignment = function(scenario, method, ...) {
    #########################################################################################
    ### Description: This code takes the data from the target_track_distance function, which
    ### calculates the difference between each sensor point and ALL truth targets at that time.
    ### THIS function takes that comparison and actually chooses (or 'assigns') tracks to targets.
    ### The exact method is determined by the user-supplied method and cutoff
    ###
    ### Input: 'scenario,' a scenario file containing at least target truth, ownship truth, and sensor dat
    ###        'method' is the assignment method (see documentation)
    ###        the various assignment methods have their own additional inputs
    ###
    ### Output: the return is similar to assignmentData, with far fewer rows, as each sensor point
    ### is now uniquely assigned to a target. The 'truthID' column is renamed to be 'tgtAssigned' (to
    ### reflect the fact that we've now assigned each track to a target). Finally, a new column,
    ### 'isFalseTrack' is added, and is TRUE for sensor points that are judged to be clutter
    ###
    ### See documentation for more information on the math and logic at work
    #########################################################################################

    # Determine the correct function to call
    func = switch(tolower(method),
                  point = target_assignment.point,
                  wholetrack = target_assignment.track,
                  windowsquare = target_assignment.window,
                  user = target_assignment.user,
                  windowgauss = target_assignment.gauss

    )

    if (!is.data.frame(scenario$targetTrackDistance)){ #if targetTrackDistance wasn't calculated
        #check to see if the components are there
        if (!is.data.frame(scenario$targetTruth)){
            stop("targetTrackDistance was not pre-calculated for this scenario, and it cannot be calculated here because the scenario is also missing targetTruth, which is required.")
        }
        if (!is.data.frame(scenario$sensorData)){
            stop("targetTrackDistance was not pre-calculated for this scenario, and it cannot be calculated here because the scenario is also missing sensorData, which is required.")
        }
        if (!is.data.frame(scenario$ownShipTruth)){
            stop("targetTrackDistance was not pre-calculated for this scenario, and it cannot be calculated here because the scenario is also missing ownShipTruth, which is required.")
        }

        cat("targetTrackDistance was not pre-calculated for this scenario. Nautilus will calculate (but not save) that information now.")

        targetTrackDistanceTemporary=scenarioMaker::target_track_distance(truthData = scenario$targetTruth,
                                                                          sensorData = scenario$sensorData,
                                                                          ownShipData = scenario$ownShipTruth)
        ret = func(targetTrackDistanceTemporary, ...) %>%
            get_track_segments( . ) #assign track segments

    } else { #if targetTrackDistance WAS calculated, proceed as normal
        ret = func(scenario$targetTrackDistance, ...) %>%
            get_track_segments( . ) #assign track segments
    }

    ### Call the function ###

    # put the target factor levels back in alphabetical order (for plotting purposes)
    ret$tgtAssigned <- factor(ret$tgtAssigned,
                              levels = sort(unique(as.character(ret$tgtAssigned))))

    scenario[['assignmentData']]=ret




    ### Create a record of the parameters used ###
    assignmentParameters=list()
    dots=list(...)
    assignmentParameters$method=method

    if (tolower(method) != "user"){ #all methods but "user" have a cutoff
        assignmentParameters$cutoff=dots$cutoff
    } else {
        assignmentParameters$cutoff=NA
    }

    if (tolower(method) %in% c("windowsquare","windowgauss")){ #window methods
        assignmentParameters$window=dots$window
    } else {
        assignmentParameters$window=NA
    }

    ### Zero out the 2nd Pass properites ###
    assignmentParameters$secondPassMethod=NA
    assignmentParameters$secondPassCutoff=NA
    assignmentParameters$secondPassWindow=NA
    assignmentParameters$excludeFirstPass=NA

    scenario[['assignmentParameters']]=assignmentParameters


    return(scenario)
}


