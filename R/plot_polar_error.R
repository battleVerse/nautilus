#' @title Plot [location error / bearing error / range error] by [target aspect / bearing to target]
#'
#' @description This figure plots one of several choices for system error (location error, range error, bearing error) against the target aspect or the bearing to the target
#'
#' @param scenario MUST contain assignmentData (from target_assignment())
#' @param angleTerm (string) the variable plotted as an angle. Currently there are two options:
#' \itemize{
#'  \item{"bearing" - the bearing to the target (are you looking at something straight ahead or to your side?)}
#'  \item{"aspect" - the target's aspect (is it facing you?)}}
#' @param rTerm (string) the variable plotted radially. Currently there are three options:
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
#' plot_polar_error(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100),angleTerm="bearing", rTerm = 'locationError',doFacet = TRUE, plotFalseTracks=FALSE)


# a few functions to help with plotting

#user facing
plot_polar_error = function(scenario,angleTerm="bearing", rTerm="locationError",colorTerm="tgtAssigned",doFacet=FALSE,plotFalseTracks=FALSE){


    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    assignmentData=scenario$assignmentData

    newData=data.frame(x=rep(1,nrow(assignmentData)),y=rep(1,nrow(assignmentData)),
                       colorBy=rep(1,nrow(assignmentData)),isFalseTrack=assignmentData$isFalseTrack)



    if (angleTerm == "bearing"){
        newData$x=assignmentData$relBearingToTarget
        title2="Target Relative Bearing"

    } else if (tolower(angleTerm) == "aspect"){
        newData$x=assignmentData$targetAspect
        title2="Target Aspect"
    }


    if (rTerm=="locationError"){
        newData$y=assignmentData$locationError
        title="Absolute Position Error (track-to-truth) (m)"
    } else if (rTerm=="downrangeError") {
        newData$y=assignmentData$downrangeError
        title="Downrange Error (m)"
    } else if (rTerm=="bearingError") {
        newData$y=assignmentData$bearingError
        title="Bearing Error (deg)"
    }

    if (colorTerm=="trackNum"){
        newData$colorBy=assignmentData$trackNum
        keyTitle="Track Number"
    } else if (colorTerm=="tgtAssigned"){
        newData$colorBy=assignmentData$tgtAssigned
        keyTitle="Target ID"
    }

    graphTitle=sprintf("%s vs %s",title,title2)

    myPlot=ggplot(data = filter(newData, isFalseTrack == FALSE),
                  aes(x = x, y = y, color = colorBy)) +
        guides(colour=guide_legend(title=keyTitle))+
        geom_point(alpha = 0.3) + coord_polar(theta = 'x', start = (0)) +
        scale_x_continuous(limits=c(0, 360), breaks=c(0, 45, 90, 135, 180, 225, 270, 315)) +
        xlab('') + ylab(title) + ggtitle(graphTitle)

    if (doFacet==TRUE){
        myPlot=myPlot+facet_wrap(~colorBy)
    }

    if (plotFalseTracks==TRUE){
        myPlot=myPlot+geom_point(data = filter(newData, isFalseTrack == TRUE),aes(x = x,y = y), colour = 'black')
    }

    return(myPlot)

}
