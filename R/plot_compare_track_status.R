#' @title Compares the track status from different assignmentDatas
#'
#' @description This figure compares the target assignments from two methods.  Each subfigure is similar to plot_track_status().  Areas of disagreement between the two methods are shown in bold.  If there are differences in points flagged as false tracks they are shown as triangles.
#'
#' @param scenario1 must contain track-to-truth assignments (the output of target_assignment()) and truthData
#' @param scenario2 must contain track-to-truth assignments (the output of target_assignment()) and truthData
#' @param nameList title of each scenario in a list, (default = the scenario names)
#' @param showFalseTracks will plot false tracks for comparison (default=FALSE)
#'
#' @import ggplot2 scenarioMaker
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pointScenario=scenarioMaker::example2_scenario %>%
#'  target_assignment("point",cutoff=100)
#'
#' gaussScenario=scenarioMaker::example2_scenario %>%
#'  target_assignment("windowGauss",cutoff=100, window=10)
#'
#' plot_compare_track_status(scenario1 = pointScenario,
#'   scenario2 = gaussScenario,
#'   nameList = list("Point Method", "Gauss Window Method"),
#'   showFalseTracks = FALSE)
#'   }


# a few functions to help with plotting

#user facing

plot_compare_track_status = function(scenario1, scenario2, nameList = NA, showFalseTracks = FALSE){

    #browser()
    if  (!("assignmentData" %in% names(scenario1))){ #if assignmentData hasn't been created
        stop("Your input for scenario1 does not contain target assignments - have you run target_assignments() yet?")
    }
    if  (!("assignmentData" %in% names(scenario2))){ #if assignmentData hasn't been created
        stop("Your input for scenario2 does not contain target assignments - have you run target_assignments() yet?")
    }

    if (!is.data.frame(scenario1$targetTruth)){ #if targetTruth isn't present
        stop("Scenario1 is missing targetTruth data, though assignmentData exists. This shouldn't be possible.")
    }

    tracksPerTargetTmp=list()
    falseTrackList=list()

    if (is.na(nameList)){
        nameList=list(scenario1$scenarioName,scenario2$scenarioName)
    }

    truthData=scenario1$targetTruth
    assignmentDataList=list(scenario1$assignmentData, scenario2$assignmentData)

    for (i in (1:length(assignmentDataList))) { #go through both inputs, get track coverage, combine them

        tracksPerTargetTmp[[i]]=get_tracks_per_target(assignmentDataList[[i]],truthData) %>% #get the track coverage on this assignmentData
            mutate(numTracksOnTarget=ifelse(numTracksOnTarget>2,2,numTracksOnTarget)) %>% #if greater than 2, make it 2 for plotting purposes
            mutate(numTracksOnTarget=as.factor(numTracksOnTarget)) %>%
            arrange(time)%>%
            mutate(sourceName=nameList[[i]])

        falseTrackList[[i]]=assignmentDataList[[i]] %>%
            filter(isFalseTrack==TRUE) %>%
            select(time, lat, lon, alt) %>% #we need to dump most of the columns so that the comparison will work properly
            mutate(sourceName=nameList[[i]])


    }

    coverageData=do.call(rbind,tracksPerTargetTmp) #make into single dataframe
    isDupRows=scenarioMaker::get_dups_between_groups(coverageData,"sourceName") #figure out which rows are isDuplicates
    agreementData=cbind(coverageData, isDup=isDupRows) #mark isDuplicates as TRUE

    falseTrackData=do.call(rbind, falseTrackList)
    isDupRows=scenarioMaker::get_dups_between_groups(falseTrackData,"sourceName")
    falseTrackComparison=cbind(falseTrackData,isDup=isDupRows)

    myPlot=ggplot(agreementData)+
        geom_point(data=filter(agreementData, isDup=="TRUE"),aes(x=lon,y=lat,group=truthID,color=numTracksOnTarget),size=.75)+
        geom_point(data=filter(agreementData, isDup=="FALSE"),aes(x=lon,y=lat,group=truthID,color=numTracksOnTarget),size=2.5)+
        scale_color_manual(values=c("red", "blue", "yellow"), name = "Tracks on Target", labels=c("None","One","Multiple"))+
        geom_path(data=truthData,aes(x=lon,y=lat,group=truthID))+
        xlab("Longitude")+ylab("Latitude")+
        coord_quickmap()+
        facet_wrap(~sourceName)

    if (showFalseTracks == TRUE) {
        myPlot=myPlot+
            geom_point(data=falseTrackComparison, aes(x=lon,y=lat, shape=isDup,size=isDup,alpha=isDup))+
            scale_shape_manual(values=c(17,19),name="False Tracks",labels=c("Disagree","Agree"))+
            scale_size_manual(values=c(2,1),name="False Tracks",labels=c("Disagree","Agree"))+
            scale_alpha_manual(values=c(.7,.3),name="False Tracks",labels=c("Disagree","Agree"))
    }

    return(myPlot)

}
