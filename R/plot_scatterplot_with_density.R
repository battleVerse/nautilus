#' @title Scatterplot with density sidebars
#'
#' @description This figure plots any two continuous variables together to check for systematic bias.  We recommend using bearingError and downrangeError.
#'
#'   Note: This is a more generic function and therefore can be used for other comparisons. However, this means that the user must specify that the data to be plotted should not include points labeled as false tracks.
#'
#' @param scenario MUST contain assignmentData (from target_assignment())
#' @param xValue the variable to be plotted on the x-axis. Can be any of the columns in assignmentData. Must be put into a string. We generally recommend "bearingError", however any continuous value can be used.
#' @param yValue the variable to be plotted on the y-axis. Can be any of the columns in assignmentData. Must be put into a string. We generally recommend "downrangeError", however any continuous value can be used.
#'
#' @import ggplot2
#'
#' @return layot of ggplot objects
#'
#' @export
#'
#' @examples
#' myScenario=scenarioMaker::example1_scenario %>% target_assignment("point",cutoff=100)
#' myScenario$assignmentData = myScenario$assignmentData %>% filter(isFalseTrack == FALSE)
#' plot_scatterplot_with_density(myScenario, xValue = 'bearingError', yValue = 'downrangeError')


# a few functions to help with plotting

#user facing

plot_scatterplot_with_density <- function(scenario, xValue, yValue){


    if  (!("assignmentData" %in% names(scenario))){ #if assignmentData hasn't been created
        stop("This scenario does not contain target assignments - have you run target_assignments() yet (or clicked 'Assign Tracks to Targets' in the GUI)?")
    }

    plotData=scenario$assignmentData


    plotData <- filter(plotData, isFalseTrack == FALSE)

    # Scatter plot of x and y variables and color by groups
    scatterPlot <- ggplot(plotData, aes_string(x = xValue, y = yValue)) +
        geom_density_2d(colour = 'grey') +
        geom_point()

    # Marginal density plot of x (top panel)
    xdensity <- ggplot(plotData, aes_string(x = xValue)) +
        geom_density(alpha=.5, fill = 'black') +
        labs(y = 'Density') +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())


    # Marginal density plot of y (right panel)
    ydensity <- ggplot(plotData, aes_string(x = yValue)) +
        geom_density(alpha=.5, fill = 'black') +
        coord_flip() +
        labs(y = 'Density') +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())


    # blank placeholder
    blankPlot <- ggplot()+geom_blank(aes(1,1))+
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()
        )


    myPlot=gridExtra::grid.arrange(xdensity, blankPlot, scatterPlot, ydensity,
                                   ncol=2, nrow=2, widths=c(4, 0.8), heights=c(0.8, 4))

    return(myPlot)
}

