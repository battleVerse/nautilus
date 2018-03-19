#' @title Find overlapping track segments
#'
#' @description This function is designed to find and remove segment overlaps from a single target. If we want to know the amount of time when a target was followed by one or more tracks, then we have to be careful to remove overlapping segments before summing. For example, if track 1 followed target A from time = 1 to time = 5 and track 2 followed target A from time = 3 to time = 7, then the final segment (after removing the overlaps) will be from time = 1 to time = 7. The total time tracked should be 6, however if we naively sum the two tracks we would double-count the time when two tracks are on one target.
#'
#' startList (and stopList) should include the start (stop) times of all segments of all tracks on one target. The start and stop times are to be paired, meaning that startList[1] and stopList[1] refer to the start and stop times of the same segment. For example, if target A was followed by track 1 in two segments (say, from t = 1 to t = 3 and later from t = 7 to t = 10) and by track 2 in three segments (say, t = 2 to t = 4, t = 5 to t = 7, and t = 8 to t = 9), then startList and stopList should each have five values. In this example, startList would be c(1, 7, 2, 5, 8) and stopList would be c(3, 10, 4, 7, 9). These five segments overlap each other in such a way that there are in fact only two segments in which target A is covered by one or more targets. These final segments are from t = 1 to t = 4 and from t = 5 to t = 10. The output would be the list: list(c(1, 5), c(4, 10)). Below we have shown these two examples.
#'
#' This function is called by timeTracked(), one of the analysis functions. It is not recommended that the user try to use this function as a stand-alone function.
#'
#' @param startList vector of times when a track began to follow a specific target
#' @param stopList vector of times when a track stopped following a specific target
#'
#' @return A list of two numeric vectors. The first vector contains all of the start times of segments and the second vector contains all of the stop times of vectors.
#'
#' @keywords internal
#'
#' @examples
#' nautilus:::find_overlaps(c(1, 3), c(5, 7))
#' nautilus:::find_overlaps(c(1, 7, 2, 5, 8), c(3, 10, 4, 7, 9))



#####################################################################################################
### These functions deal with figuring out when tracks start and stop tracking a specific target, ###
### as well as combining overlapping regions. These are generally called by graphing functions.   ###
#####################################################################################################


find_overlaps <- function(startList, stopList){
        #not user facing

        ################################################################################################
        ### Description: The find_track_length tells you when each track segment starts and stops.
        ### However, there may be overlaps (i.e. there were 2 simultaneous tracks on the same target),
        ### which means that you can't simply add up the times from find_track_length to figure out
        ### how long something was tracked. This function will take the output from find_track_length
        ### and combine any overlaps to tell you how long something was tracked for (regardless of
        ### how many tracks were tracking it)
        ###
        ### Input: This function is called by get_track_coverage, and takes the paired start and stop times
        ### for each track segment as two separate lists
        ###
        ### Output: returns a list of start/stop times. For example, two tracks on the same target,
        ### one from 1 -> 7 and the other from 3 -> 9 will be returned as a single range from 1 -> 9
        ###
        ################################################################################################

        #takes list of start and stop times (MUST be paired, i.e. startList[[1]] corresponds to stopList[[1]])
        #returns new start/stop lists (joined in a single list) that remove all overlaps, i.e. 1->5 and 3->7 become 1->7

        for (i in seq(length(startList))){
                for (j in seq(length(startList))){
                        if (j <= i) {next} #only do the lower diagonal combinations
                        if (startList[[i]]<=stopList[[j]] & stopList[[i]]>= startList[[j]]){ #if the ranges do in fact overlap

                                #the new "correct" start is the EARLIER of the two starts
                                startList[length(startList)+1]=min(startList[[i]],startList[[j]])

                                #the new "correct" stop is the LATER of the two starts
                                stopList[length(stopList)+1]=max(stopList[[i]],stopList[[j]])

                                #because we've added the new "correct" start and end to the bottom of the list, remove the old ones
                                startList=startList[-c(i,j)]
                                stopList=stopList[-c(i,j)]


                                return(find_overlaps(startList,stopList)) #we've found a match and changed the data, so start over again! (recursive)
                        }
                }
        }
        return(list(startList,stopList)) #we've run through everything and found no changes, so we're done
}

