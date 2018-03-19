#' @title Plot target assignments
#'
#' @description Figure that shows which sensor points were assigned to which targets.  Thin lines connect the points the sensor point to the interpolated true position at that time.  It also indicates which points were determined to be false tracks.  This figure is very useful to show the differences between assignment methods but can be difficult to read when there are many targets and tracks in one figure. The figure plots all of the targets as well as all of the tracks that were not determined to be false tracks. False track points are shown as black dots.
#'  Plotly figures are interactive, and users can zoom in, hover over data points for more info, and click to hide tracks or targets.
#'
#'
#' @param scenario MUST contain targetTruth and assignmentData (from target_assignment())
#'
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' plot_target_assignments_plotly(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))


# a few functions to help with plotting

#user facing

plot_target_assignments_plotly=function(scenario){


    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    if (!is.data.frame(scenario$targetTruth) || !is.data.frame(scenario$ownShipTruth)){ #if targetTruth isn't present
        stop("This function is missing targetTruth and/ownShipTruth data, though assignmentData exists. This shouldn't be possible.")
    }

    assignmentData=scenario$assignmentData
    truthData=scenario$targetTruth
    ownShip=scenario$ownShipTruth

    falseTrackData=filter(assignmentData, isFalseTrack == TRUE)
    realTrackData=filter(assignmentData, isFalseTrack == FALSE)

    assignmentData=realTrackData %>% #makes a new dataframe with the start and end coords for each connecting line
        mutate(lon = lon+lonError, lat=lat+latError, alt=alt+altError) %>%
        rbind(realTrackData) %>%
        select(-lonError,altError,latError) %>%
        arrange(pointIndex) %>%
        group_by(pointIndex)

    xlabel=list(title="Longitude")
    ylabel=list(title="Latitude")

    myPlot=plotly::plot_ly(truthData, ### Truth Data
                   x=~lon,
                   y=~lat,
                   type='scatter',
                   mode='markers+lines',
                   marker=list(size=6),
                   split=~truthID,
                   line = list(color = 'rgba(0,0,255,1)'),
                   hoverinfo='text',
                   text=~paste('Target: ',truthID,'<br> Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01")))
    if (nrow(falseTrackData)!=0) {
        myPlot = myPlot %>% plotly::add_trace(data=falseTrackData, ### False Tracks ###
                                      x=~lon,
                                      y=~lat,
                                      type='scatter',
                                      mode='markers',
                                      marker=list(size=6,color = 'rgba(0,0,0,.7)'),
                                      line=NULL,
                                      split=NULL,
                                      name="False Tracks",
                                      hoverinfo='text',
                                      text=~paste('False Track',
                                                  '<br>Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                                                  '<br>Track #: ',trackNum,
                                                  '<br>Closest Target: ', tgtAssigned,
                                                  '<br>Tgt ',tgtAssigned,' Range to Sensor (m):', round(rangeToShip,1),
                                                  '<br>Track Distance to' , tgtAssigned,' (m):',round(locationError,1)))
    }
    myPlot = myPlot %>%
        plotly::add_trace(data=assignmentData, ### Assignment Lines ###
                  x=~lon,
                  y=~lat,
                  type='scatter',
                  mode='lines',
                  line = list(color = 'rgba(0,0,0,.5)'),
                  split = NULL,
                  marker=NULL,
                  name="Assignments",
                  hoverinfo='text',
                  text=~paste('Track Association',
                              '<br>Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                              '<br>Track #: ',trackNum,
                              '<br>Target Name: ', tgtAssigned,
                              '<br>Target Range to Sensor (m):', round(rangeToShip,1),
                              '<br>Track Error (m):',round(locationError,1)),
                  hoverlabel=list(bgcolor='rgba(0,0,0,1)')) %>%
        plotly::add_trace(data=realTrackData, ### Sensor Data ###
                  x=~lon,
                  y=~lat,
                  type='scatter',
                  mode='markers+lines',
                  marker=list(size=6),
                  line = list(color = 'rgba(255,0,0,1)', dash='dash'),
                  split=~trackNum,
                  hoverinfo='text',
                  text=~paste('Track Association',
                              '<br>Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                              '<br>Track #: ',trackNum,
                              '<br>Target Name: ', tgtAssigned,
                              '<br>Target Range to Sensor (m):', round(rangeToShip,1),
                              '<br>Track Error (m):',round(locationError,1)),
                  hoverlabel=list(bgcolor='rgba(0,0,0,1)')) %>%
        plotly::add_trace(data=ownShip, ### Ownship Data ###
                  x=~lon,
                  y=~lat,
                  split=~truthID,
                  type='scatter',
                  mode='lines',
                  marker=NULL,
                  line = list(color = 'rgba(0,0,0,1)'),
                  hoverinfo='text',
                  text=~paste('Ownship Location','<br> Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"))) %>%
        plotly::layout(xaxis=xlabel,yaxis=ylabel)


    # if (hideLegend == TRUE){
    #     myPlot = myPlot + theme(legend.position="none")
    # }

    return(myPlot)

}

