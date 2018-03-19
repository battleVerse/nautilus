#' @title Visualize when targets were tracked with plotly
#'
#' @description Plot truth data, color coded to show whether target was tracked.  Colors indicate whether targets were tracked with zero, one, or multiple tracks at a time.  Plotly figures are interactive, and users can zoom in, hover over data points for more info, and click to hide tracks or targets.
#'
#' @param scenario MUST contain targetTruth and assignmentData (from target_assignment())
#'
#'
#' @return plotly object
#'
#' @export
#'
#' @examples
#' plot_track_status_plotly(scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100))


# helps visualize when targets are tracked

#user facing

plot_track_status_plotly= function(scenario){


    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    if (!is.data.frame(scenario$targetTruth) || !is.data.frame(scenario$ownShipTruth)){ #if targetTruth isn't present
        stop("This function is missing targetTruth and/ownShipTruth data, though assignmentData exists. This shouldn't be possible.")
    }

    assignmentData=scenario$assignmentData
    truthData=scenario$targetTruth
    ownShipData=scenario$ownShipTruth



    xlabel=list(title="Longitude")
    ylabel=list(title="Latitude")

    coverageData=get_tracks_per_target(assignmentData, truthData)


    coverageData=coverageData %>% #number of tracks on each target, capped at 2 (for graphing purposes)
        mutate(multiTrackStatus=ifelse(numTracksOnTarget>2,2,numTracksOnTarget)) %>% #if greater than 2, make it 2 for plotting purposes
        mutate(multiTrackStatus=as.factor(multiTrackStatus)) %>%
        mutate(multiTrackStatus = forcats::fct_recode(multiTrackStatus, #rename levels for plotting
                                             "None" = "0",
                                             "One" = '1',
                                             "Multiple" = '2')) %>%
        arrange(time) %>%
        group_by(truthID)




    coverageSegments = coverageData %>% #breaks up the tracks into segments based on color
        mutate(uniqueSegment = interaction(as.factor(multiTrackStatus),as.factor(truthID))) %>%
        mutate(segmentNumber = cumsum(ifelse(uniqueSegment == lag(uniqueSegment, default = as.factor(0), n = 1), 0, 1))) %>%
        group_by(truthID,segmentNumber) #I used to cast segmentNumber as a factor, but this caused warnings. It seems to work now? BAA 3-16-2018


    myPlot=plotly::plot_ly(data=coverageSegments,
            x=~lon,
            y=~lat,
            type='scatter',
            mode='lines+markers',
            marker=NULL,
            color=~multiTrackStatus,
            colors=c("red","blue","yellow"),
            hoverinfo='text',
            text=~paste('Track Coverage',
                        '<br>Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                        '<br>Target Name: ', truthID,
                        '<br># of Tracks: ', numTracksOnTarget),
            hoverlabel=list(bgcolor='rgba(0,0,0,1)')) %>%
        plotly::add_trace(data=ownShipData,
                  x=~lon,
                  y=~lat,
                  split=~truthID,
                  type='scatter',
                  mode='lines',
                  marker=NULL,
                  color=NULL,
                  line = list(color = 'rgba(0,0,0,1)'),
                  hoverinfo='text',
                  text=~paste('Ownship Location','<br> Time: ', as.POSIXct(time,tz="UTC",origin="1970-01-01"),
                              '<br>Lat: ',round(lat,5),
                              '<br>Lon: ',round(lon,5),
                              '<br>Alt (m): ',round(alt,2))) %>%
        plotly::layout(xaxis=xlabel,yaxis=ylabel) %>%
        plotly::add_annotations( text="Tracks on Target", xref="paper", yref="paper",
                         x=1.02, xanchor="left",
                         y=.99, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE )


    return(myPlot)

}


