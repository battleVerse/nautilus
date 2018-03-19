#' @title Get track segments
#'
#' @description The purpose of this function is to separate each track into segments by which target it was following. This comes up frequently later in the analysis, particularly when trying to computer total track coverage and number of tracks per target.
#'
#' get_track_segments() first creates the column tgtXtrack which is a factor created from the interaction between assigned target and sensor track number. Next, the segment number is created from the number of instances when a track follows a target. When the track changes targets the segment number increases by 1. For example, if track 1 was assigned to target A, then B, then back to A, segmentNumber would increment by 1 each time
#'
#' @param assignmentData the data frame output by target_assignment()
#'
#' @return The same data set with two additional columns: tgtXtrack and segmentNumber. tgtXtrack is a factor showing which target the point was assigned to and to which track number the point belongs. segmentNumber counts the number of instances of each tgtXtrack factor.
#'
#' @keywords internal
#'


#####################################################################################################
### These functions deal with figuring out when tracks start and stop tracking a specific target, ###
### as well as combining overlapping regions. These are generally called by graphing functions.   ###
#####################################################################################################

get_track_segments <- function(assignmentData){
        #internal function - approved for non-scenario input
        #not user facing

        #########################################################################################
        ### Description: A 'track segment' is an unbroken piece of a track belonging to a
        ### specific target. Every time a track jumps between targets this begins a new
        ### segment. We need this for various graphing operations
        ###
        ### This function is automatically called as part of the target_assignment process!
        ###
        ### Input: the 'assignmentData' dataframe from the target_assignment function
        ###
        ### Output: returns assignmentData with an additional column containing the track segment
        ### as a factor (it's a number)
        #########################################################################################

        groupedData=assignmentData %>%
                ungroup() %>%
                arrange(trackNum,time) %>% #sort by trackNum, then within each track by times
                mutate(tgtXtrack = interaction(trackNum,tgtAssigned), #make a cross term between trackNum and target - this is what we're watching to see when a new 'segment' starts
                       segmentNumber = as.factor(cumsum(ifelse(tgtXtrack == lag(tgtXtrack, default = as.factor(0), n = 1), 0, 1)))) #if the line behind me is NOT the same (i.e. I'm a new segment), then increase the cumsum by 1 (I'm a new segment with a new number!)

        return(groupedData)
}





