#' @title Plot target assignments
#'
#' @description Figure that shows which sensor points were assigned to which targets.  Thin lines connect the points the sensor point to the interpolated true position at that time.  It also indicates which points were determined to be false tracks.  This figure is very useful to show the differences between assignment methods but can be difficult to read when there are many targets and tracks in one figure. The figure plots all of the targets as well as all of the tracks that were not determined to be false tracks. False track points are shown as black dots.
#' The first point (in time) of each target is labeled with the name of the target.  The last recorded sensor point for each track is labeled with the number.
#'
#' @param scenario MUST contain targetTruth and assignmentData (from target_assignment())
#' @param scalePoints a scale to set the size of all of points in the figure. scalePoints affects all of the plotted objects. This is useful in figures with more or fewer data points
#' @param textSize size of label text
#' @param showFalseTracks plot false tracks as semi-tranparent black dots (default = TRUE)
#' @param hideLegend hides the legend to make the graph easier to read (default=FALSE)
#'
#' @return ggplot object
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' plot_target_assignments(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))


# a few functions to help with plotting

#user facing

plot_target_assignments=function(scenario,scalePoints=1, textSize = 3, showFalseTracks = TRUE, hideLegend = FALSE){


    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    if (!is.data.frame(scenario$targetTruth)){ #if targetTruth isn't present
        stop("This function is missing targetTruth data, though assignmentData exists. This shouldn't be possible.")
    }

    assignmentData=scenario$assignmentData
    truthData=scenario$targetTruth

    truthLabels <- truthData %>%
        group_by(truthID) %>%
        arrange(time) %>%
        summarise(lat = first(lat), lon = first(lon))

    sensorLabels <- assignmentData %>%
        filter(isFalseTrack == FALSE) %>%
        group_by(trackNum) %>%
        arrange(time) %>%
        summarise(lat = last(lat), lon = last(lon))


    myPlot=ggplot() +
        geom_path(data=truthData,aes(x=lon,y=lat,color=truthID),size=.1*scalePoints)+ #truth line
        #geom_point(data=truthData,aes(x=lon,y=lat,color=truthID),size=.2,pch=8)+ #truth point
        #
        geom_path(data=filter(assignmentData, isFalseTrack == FALSE),aes(x=lon,y=lat,color=trackNum),linetype=2,size=.1*scalePoints) + #sensor line
        geom_point(data=filter(assignmentData, isFalseTrack == FALSE),aes(x=lon,y=lat,color=trackNum), size=.2*scalePoints,shape='x')+ #sensor point
        #
        geom_segment(data=filter(assignmentData, isFalseTrack == FALSE),
                     aes(x=lon, xend=lon+lonError,y=lat,yend=lat+latError),linetype=1,size=.05*scalePoints,alpha=.65)+ #connect sensor to truth
        labs(x = "Longitude", y = "Latitude", color = 'Track Number \n or Target ID',
             title = 'Target assignments')+
        geom_label(data=truthLabels,aes(x=lon,y=lat,label=truthID), size = textSize,alpha=.7)+
        geom_label(data=sensorLabels,aes(x=lon,y=lat,label=trackNum), size = textSize,alpha=.7)+
        coord_quickmap()

    if (showFalseTracks) {
        falseSensorLabels <- assignmentData %>%
            filter(isFalseTrack == TRUE) %>%
            group_by(trackNum) %>%
            arrange(time) %>%
            summarise(lat = last(lat), lon = last(lon))

        myPlot=myPlot+geom_point(data=filter(assignmentData, isFalseTrack == TRUE),
                                 aes(x=lon,y=lat),color="black",size=.1*scalePoints,alpha=.4)+ #excluded data
            geom_label(data=falseSensorLabels,aes(x=lon,y=lat,label=trackNum), size = textSize,alpha=.7)

    }

    if (hideLegend == TRUE){
        myPlot = myPlot + theme(legend.position="none")
    }

    return(myPlot)

}

