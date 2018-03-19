#' @title Plot [location error / bearing error / range error] by [range / time]
#'
#' @description This figure plots one of several choices for system error (location error, range error, bearing error) against time or range.  Can be split by target assigned or track number.
#'
#' @param scenario MUST contain assignmentData (from target_assignment())
#' @param rangeCutoff the position for a horizontal cutoff line. This should be equal to the cutoff you used in your analysis.  Will only draw a single line, so if the cutoff specified has 2 or 3 terms it may be less useful
#' @param xTerm (string) the variable plotted on the x-axis. Currently there are two options:
#' \itemize{
#'  \item{"rangeToShip"}
#'  \item{"time"}}
#' @param yTerm (string) the variable plotted on the y-axis. Currently there are three options:
#' \itemize{
#'  \item{"locationError"}
#'  \item{"bearingError"}
#'  \item{"downrangeError"}}
#' @param colorTerm (string) the variable to be split by color. Currently there are two options:
#' \itemize{
#'  \item{"tgtAssigned"}
#'  \item{"trackNum"}}
#' @param doFacet (default FALSE) boolean.  Determines whether the graph will also facet by colorTerm
#' @param plotFalseTracks (default FALSE) boolead. Determines whether graph will show fast track points or hide them
#'
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_error(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100), rangeCutoff=2000, xTerm="rangeToShip", yTerm="locationError", colorTerm="tgtAssigned")


# a few functions to help with plotting

#user facing

plot_error = function(scenario,rangeCutoff,xTerm = "rangeToShip", yTerm = "locationError",colorTerm = "tgtAssigned",doFacet=FALSE,plotFalseTracks=FALSE){

    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    assignmentData=scenario$assignmentData

    newData=data.frame(x=rep(1,nrow(assignmentData)),y=rep(1,nrow(assignmentData)),
                       colorBy=rep(1,nrow(assignmentData)),isFalseTrack=assignmentData$isFalseTrack)


    if (xTerm == "rangeToShip" || tolower(xTerm) == "range to ship"){
        newData$x=assignmentData$rangeToShip
        xLabel="Range to Ship (m)"

    } else if (tolower(xTerm) == "time"){
        newData$x=assignmentData$time
        newData$x=as.POSIXct(newData$x,tz="UTC",origin="1970-01-01")
        xLabel="Time"
    }


    if (yTerm=="locationError"){
        newData$y=assignmentData$locationError
        yLabel="Absolute Position Error (track-to-truth) (m)"
    } else if (yTerm=="downrangeError") {
        newData$y=assignmentData$downrangeError
        yLabel="Downrange Error (m)"
    } else if (yTerm=="bearingError") {
        newData$y=assignmentData$bearingError
        yLabel="Bearing Error (deg)"
    }

    if (colorTerm=="trackNum"){
        newData$colorBy=assignmentData$trackNum
        title="Track Number"
    } else if (colorTerm=="tgtAssigned"){
        newData$colorBy=assignmentData$tgtAssigned
        title="Truth ID"
    }

    myPlot=ggplot(data = filter(newData, isFalseTrack == FALSE),
                  aes(x = x,y = y, colour = colorBy))+
        geom_point()+
        guides(colour=guide_legend(title=title))+
        labs(x = xLabel, y = yLabel)

    if (yTerm != "bearingError"){
        myPlot=myPlot+geom_hline(aes(yintercept=rangeCutoff), linetype = 'dashed')
    }

    if (plotFalseTracks==TRUE){
        myPlot=myPlot+geom_point(data = filter(newData, isFalseTrack == TRUE),aes(x = x,y = y), colour = 'black')
    }

    if (doFacet==TRUE){
        myPlot=myPlot+facet_wrap(~colorBy)
    }

    return(myPlot)


}
