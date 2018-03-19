#' @title Compares the track status from different assignmentDatas
#'
#' @description This figure compares the target assignments from two methods.  Each subfigure is similar to plot_track_status().  Areas of disagreement between the two methods are shown in bold.  If there are differences in points flagged as false tracks they are shown as triangles.
#'
#' @param assignmentDataList list of two assignmentData dataframes (the output of target_assignment())
#' @param nameList list of two names for plotting (in the same order as assignmentDataList)
#' @param truthData truthData used in creating assignmentData sets
#' @param showFalseTracks will plot false tracks for comparison (default=FALSE)
#'
#' @import ggplot2 scenarioMaker
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples
#' pointScenario=scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100)
#' gaussScenario=scenarioMaker::example1_scenario %>% target_assignment("windowGauss",cutoff=100, window=10)
#' plot_compare_track_status(list(pointScenario$assignmentData, gaussScenario$assignmentData),list("Point Method", "Gauss Window Method"), scenarioMaker::example1_scenario$targetTruth)


# a few functions to help with plotting

#user facing

plot_compare_track_status = function(assignmentDataList, nameList, truthData, showFalseTracks = FALSE){
    #approved for non-scenario input
    tracksPerTargetTmp=list()
    falseTrackList=list()

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
    dupRows=scenarioMaker::get_dups_between_groups(coverageData,"sourceName") #figure out which rows are duplicates
    agreementData=cbind(coverageData, dup=dupRows) #mark duplicates as TRUE

    falseTrackData=do.call(rbind, falseTrackList)
    dupRows=scenarioMaker::get_dups_between_groups(falseTrackData,"sourceName")
    falseTrackComparison=cbind(falseTrackData,dup=dupRows)

    myPlot=ggplot(agreementData)+
        geom_point(data=filter(agreementData, dup=="TRUE"),aes(x=lon,y=lat,group=truthID,color=numTracksOnTarget),size=.75)+
        geom_point(data=filter(agreementData, dup=="FALSE"),aes(x=lon,y=lat,group=truthID,color=numTracksOnTarget),size=2.5)+
        scale_color_manual(values=c("red", "blue", "yellow"), name = "Tracks on Target", labels=c("None","One","Multiple"))+
        geom_path(data=truthData,aes(x=lon,y=lat,group=truthID))+
        xlab("Longitude")+ylab("Latitude")+
        coord_quickmap()+
        facet_wrap(~sourceName)

    if (showFalseTracks == TRUE) {
        myPlot=myPlot+
            geom_point(data=falseTrackComparison, aes(x=lon,y=lat, shape=dup,size=dup,alpha=dup))+
            scale_shape_manual(values=c(17,19),name="False Tracks",labels=c("Disagree","Agree"))+
            scale_size_manual(values=c(2,1),name="False Tracks",labels=c("Disagree","Agree"))+
            scale_alpha_manual(values=c(.7,.3),name="False Tracks",labels=c("Disagree","Agree"))
    }

    return(myPlot)

}
