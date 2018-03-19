#' @title Determine whether a point belongs to a false track or a valid track
#'
#' @description Sensor systems frequently return tracks on things that are not actual targets (waves, reflections, dolphins, etc.), and therefore we do not have truth data to evaluate the system accuracy. We flag these as flase tracks (isFalseTrack = TRUE) to not penalize the system accuracy, however the data is still kep and returned to the user. get_isFalseTrack() is called by the target_assignment() subfunctions to perform this task. Each subfunction calculates the mean location, bearing, downrage, and altitude errors. These are passed into the function inside targetAssign.
#'
#' The user can input a cutoff as a numeric vector with length 1, 2, or 3, each of which implies different cutoff criteria:
#'
#'   - With a single number cutoff (vector length 1), a point is determined to be a false track if the mean location error (calculated based on method assignment method, in meters) is outside the cutoff.
#'
#'   - If two numbers are supplied (numeric vector length 2), the first number corresponds to a 2D surface error (in meters) calculated from the location error and altitude error (surface error = sqrt(locationError^2 - altError^2)). The second number is the absolute value of the altitude error (in meters).
#'
#'   - If three numbers are supplied the first number corresponds to the downrange error (in meters), the second number to the bearing error (in degrees), and the third number to the altitude error (in meters).
#'
#' This function is called by the target_assignment() subfunctions and is not inded for use as a stand-alone function.
#'
#' @param targetAssign a data frame containing the mean location, bearing, altitude, and downrange errors for each point
#' @param cutoff numeric vector of length 1, 2, or 3 specifying the conditions under which a point is determined to be a false track. The different lengths imply different cutoff conditions:
#' \itemize{
#'  \item{One number (vector length 1): points are false tracks if the location error (difference between sensor point and closest target) is greater than this value in meters}
#'  \item{Two numbers (vector length 2): first number is cutoff in meters of the 2D error (sqrt(locationError^2 - altError^2)). Second number is cutoff in meters of the absolute value of the altitude error (sensor altitude reading - true target altitude)}
#'  \item{Three numbers (vector length 3): first number is range error in meters, second number is bearing error in degrees, third number is altitude error in meters}}
#'
#' @return get_isFalseTrack() returns the input (targetAssign) with the additional column isFalseTrack with boolean value that indicates whether a point is a false track.
#'
#' @keywords internal
#'



# HELPER FUNCTION: add the isFalseTrack column, default is false
get_isFalseTrack <- function(targetAssign, cutoff){
        # takes the current targetAssign data frame and adds isFalseTrack based on the size of the cutoff term

        # How long "cutoff" is changes what cutoffs the user is referring to
        if(length(cutoff) == 1){
                #if only one number, assume it's location Error
                targetAssign <- targetAssign %>%
                        mutate(isFalseTrack=ifelse(locationError > cutoff, TRUE,FALSE))
        } else if(length(cutoff) == 2){
                # if two numbers, assume it's 2D circle error and alt error
                targetAssign <- targetAssign %>%
                        mutate(isFalseTrack=ifelse((sqrt(locationError^2-altError^2) > cutoff[1]) |
                                       (abs(altError) > cutoff[2]), TRUE,FALSE))
        } else if(length(cutoff) == 3){
                #if three numbers, assume it's downrangeError, bearingError, altError
                targetAssign <- targetAssign %>%
                        mutate(isFalseTrack=ifelse(((abs(downrangeError) > cutoff[1]) |
                                                            (abs(bearingError) > cutoff[2]) |
                                                            (abs(altError) > cutoff[3])), TRUE,FALSE))
        }

        return(targetAssign)
}





