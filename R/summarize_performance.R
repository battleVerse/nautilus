#' @title Summarize sensor performance
#'
#' @description Table showing when targets were detected
#'
#' @param scenario must contain assignmentData, the output of target_assignment()
#'
#' @return data frame containing a summary of detection range and time, average error, average downrange error, average bearing error, and total number of sensor points for each target.  The data frame contains the collowing variables:
#'
#' \itemize{
#'  \item{"Tgt Name": Name of target}
#'  \item{"Detection Range (km)": range at which the target was first detected}
#'  \item{"Detection Time": time at which the target was first detected}
#'  \item{"Avg Abs Position Error (m)": average absolute difference between target location and location of assigned sensor point}
#'  \item{"Avg Abs Downrange Error (m)": average absolute difference between target range and range of assigned sensor point}
#'  \item{"Avg Abs Bearing Error (deg)": average absolute difference between target bearing and bearing of assigned sensor point}
#'  \item{"# of sensor points": total number of sensor points assigned to the target}}
#'
#' @export
#'
#' @examples
#' summarize_performance(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))


summarize_performance = function(scenario){
    #user facing
    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }
    assignmentData=scenario$assignmentData

    result=ungroup(assignmentData) %>%
        filter(isFalseTrack==FALSE) %>%
        arrange(time) %>%
        group_by(tgtAssigned) %>%
        rename("Tgt Name"=tgtAssigned)%>%
        summarize(
            "Detection Range (km)"=round(first(rangeToShip)/1000,digits=2),
            "Detection Time"=as.POSIXct(first(time),tz="UTC",origin="1970-01-01"),
            "Avg Abs Position Error (m)"=round(mean(abs(locationError)),digits=2),
            "Avg Abs Downrange Error (m)"=round(mean(abs(downrangeError)),digits=2),
            "Avg Abs Bearing Error (deg)"=round(mean(abs(bearingError)),digits=2),
            "# of sensor points"=n()
        )
    return(result)
}
