#' @title Visualize when targets were tracked
#'
#' @description Plot truth data, color coded to show whether target was tracked
#'
#' @param scenario MUST contain targetTruth and assignmentData (from target_assignment())
#' @param scalePoints a scale to set the size of all of points in the figure. scalePoints affects all of the plotted objects. This is useful in figures with more or fewer data points
#' @param textSize size of label text
#' @param showFalseTracks plot false tracks as semi-tranparent black dots (default = TRUE)
#' @param hideLegend hides the legend to make the graph easier to read (default=FALSE)
#'
#' @import ggplot2
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_track_status(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))


# helps visualize when targets are tracked

#user facing

plot_track_status= function(scenario, scalePoints=1, textSize = 3, showFalseTracks=TRUE, hideLegend= FALSE){

    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    if (!is.data.frame(scenario$targetTruth)){ #if targetTruth isn't present
        stop("This function is missing targetTruth data, though assignmentData exists. This shouldn't be possible.")
    }

    assignmentData = scenario$assignmentData
    truthData = scenario$targetTruth

    coverageData = get_tracks_per_target(assignmentData, truthData)

    coverageData=coverageData %>%
        mutate(numTracksOnTarget=ifelse(numTracksOnTarget>2,2,numTracksOnTarget)) %>% #if greater than 2, make it 2 for plotting purposes
        mutate(numTracksOnTarget=as.factor(numTracksOnTarget)) %>%
        mutate(numTracksOnTarget = forcats::fct_recode(numTracksOnTarget, #rename levels for plotting
                                             "None" = "0",
                                             "One" = '1',
                                             "Multiple" = '2')) %>%
        arrange(time)


    truthLabels <- truthData %>%
        group_by(truthID) %>%
        arrange(time) %>%
        summarise(lat = first(lat), lon = first(lon))


    myPlot=ggplot(coverageData)+
        geom_path(aes(x=lon,y=lat,group=truthID,color=numTracksOnTarget),size=1)+
        geom_point(aes(x=lon,y=lat,group=truthID,color=numTracksOnTarget),size=2.5)+
        scale_color_manual(values=c("red", "blue", "yellow"), name = "Tracks on Target", labels=c("None","One","Multiple"))+
        xlab("Longitude")+ylab("Latitude")+
        geom_label(data=truthLabels,aes(x=lon,y=lat,label=truthID), size = textSize ,alpha=.7)+
        coord_quickmap()



    if (showFalseTracks) {
        myPlot=myPlot+geom_point(data=filter(assignmentData, isFalseTrack == TRUE),
                                 aes(x=lon,y=lat),color="black",size=.1*scalePoints,alpha=.4) #excluded data

    }

    if (hideLegend == TRUE){
        myPlot = myPlot + theme(legend.position="none")
    }


    return(myPlot)

}


