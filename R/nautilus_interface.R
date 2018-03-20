#' @title Nautilus interface function (shiny)
#'
#' @description Function to create shiny interface
#'
#' @import shiny miniUI rintrojs scenarioMaker dplyr
#'
#' @return none
#'
#' @keywords internal
#'




#not user facing, but still document

nautilus_interface = function() {

    getScenarios = function(sampleData=FALSE) {

        validChoices=lapply(ls(.GlobalEnv), function(df) names(eval(parse(text=df))) %in% c("scenarioName")) #scan through every name of every object
        validChoices=lapply(validChoices, function(myRow) any(myRow)) #reduce this to one entry for each object
        validChoices=unlist(validChoices)


        if (any(validChoices)){
            return(ls(.GlobalEnv)[validChoices])
        } else { #if NO valid choices, and sampleData==TRUE, return a sample dataset

            if (sampleData==TRUE){
                return(c("scenarioMaker::example1_scenario"))
            } else {
                return()
            }

        }

    }

    # getValidData = function(dataType,sampleData=FALSE) { #get valid dataframes to populate drop-downs in first pane
    #
    #
    #     columnNames=  switch(dataType,
    #                          "truth" = c("time","lat","lon","truthID","heading","alt"),
    #                          "sensor" = c("time","lat","lon","trackNum","alt"),
    #                          "ownship" = c("time","lat","lon","truthID","heading","alt"),
    #                          "assignmentData" = c("lonError","latError","altError","bearingToTarget", "trackNum","tgtAssigned","locationError",
    #                                          "pointIndex","time","bearingError","downrangeError","lon","lat","alt","rangeToShip","targetAspect",
    #                                          "isFalseTrack","tgtXtrack","segmentNumber")
    #     )
    #
    #     factorNames = switch(dataType,
    #                          "truth" = "truthID",
    #                          "sensor" = "trackNum",
    #                          "ownship" = "truthID",
    #                          "assignmentData" = "tgtAssigned"
    #     )
    #
    #     sampleReturn = switch(dataType,
    #                           "truth" = "scenario3_truthData_dropout",
    #                           "sensor" = "scenario3_sensorData",
    #                           "ownship" = "scenario3_ownShipData",
    #                           "assignmentData" = NA)
    #
    #
    #     ### Figure out which items in the global environment have the right columns ###
    #     validChoices=lapply(ls(.GlobalEnv), function(df) scenarioMaker::verify_input_data(myData=eval(parse(text=df)),columnNames=columnNames,factorNames=factorNames,stopOnFail=FALSE))
    #     validChoices=unlist(validChoices)
    #
    #     if  (any(validChoices)){ #if there are valid choices, return them
    #
    #         validChoices=which(validChoices)
    #         return( ls(.GlobalEnv)[validChoices])
    #
    #     } else { #if NO valid choices, and sampleData==TRUE, return a sample dataset
    #
    #         if (sampleData==TRUE){
    #             return(c(sampleReturn))
    #         } else {
    #             return()
    #         }
    #
    #     }
    #
    #
    #
    # }

    # if (is.null(getValidData("truth"))){
    #         assign('sampleTruth',scenario3_truthData_dropout,envir=.GlobalEnv)
    # }

    nautilusUI =
        fluidPage(
            introjsUI(),
            gadgetTitleBar("Nautilus",left=miniTitleBarButton("help","Tutorial")),

            tabsetPanel(

                ######################
                ### Parameters Tab ###
                ######################
                tabPanel("Parameters", icon = icon("sliders"),

                         textInput("runName", "Name of Event",value="Test Event"),


                         ##################
                         ### Input Data ###
                         ##################
                         wellPanel( h4("Input Data"), #user must select input data frames

                                    introBox(
                                        fluidRow(
                                            textOutput("checkData"),
                                            column(3, selectInput('scenario',"Select Scenario",c("None", getScenarios(sampleData=TRUE)))),
                                            column(3,htmlOutput("summarizeScenario")),
                                            column(3,htmlOutput("summarizeassignmentData")),
                                            column(3,htmlOutput("summarizeassignmentData2ndPass"))
                                            ),
                                        data.step=1,
                                        data.intro="<h3><center>Welcome to Nautilus!</h3></center> You start your analysis by selecting your data from these dropdown menus. Nautilus automatically
                                        figures out which of your dataframes are valid choices. 'Truth' is the truth position of your targets (often measured with GPS), 'Sensor' is the reported
                                        tracks from your sensor, and 'Ownship' is the position of the sensor itself. You must load this data in RStudio and ensure that it is correctly formatted
                                        <b>before</b> launching Interactive Nautilus (see documentation for details). <br><br> For this tutorial, we've loaded a sample set of data automatically."
                                    ),
                                    ### Conditional Data Diagnostics Panel ###
                                    introBox(
                                        introBox(
                                            checkboxInput("dataDiagnostics","Truth Data Dropout Diagnostics",value=FALSE),
                                            data.step=2,
                                            data.intro="Nautilus works by interpolating your truth (positional) data to match the times of your sensor data, which allows one-to-one comparisons.
                                            However, you may have gaps in your truth data. The longer the gap, the less valid your interpolation is. It's okay if the truth data for a given
                                            target starts late or ends early, for example, if a target was destroyed during the event. Nautilus provides a helpful diagnostic for reviewing
                                            your truth data."
                                        ),
                                        conditionalPanel(condition="input.dataDiagnostics",
                                                         plotOutput("plotTruthGaps"),
                                                         DT::dataTableOutput("diagnosticTable")
                                        ),
                                        data.step=3,
                                        data.intro="The graph shows the interval between subsequent truth updates for each target. High values mean large gaps in the data. The horizontal lines
                                        are at 2, 5, and 10 times the average interval for the given target (targets A, C, and G have large gaps). The table below contains the same information. Large dropouts mean that Nautilus's
                                        interpolations (and thus track assignments) are less accurate. This problem is less pronounced for slow moving targets, or those moving in straight lines.
                                        Ultimately, it's up to you to determine when a gap in truth data becomes a problem. Unfortunately, there is no one-size-fits-all fix for gaps in truth data."
                                    )

                         ),


                         #########################
                         ### Cutoff Parameters ###
                         #########################

                         wellPanel( h4("Cutoff Parameters"), #user controls target assignment method and cutoffs
                                    introBox(
                                        introBox(
                                            fluidRow(
                                                column(3,selectInput('method', 'Target Assignment Method', c("Point","Whole Track","Square Window", "Gauss Window"))),
                                                column(4,radioButtons("cutoffStyle", "Cutoff Shape:",
                                                                      c("Spherical" = "spherical",
                                                                        "Cylindrical" = "cylindrical",
                                                                        "Curved Wedge" = "wedge"),inline=TRUE))
                                            ),
                                            data.step=4,
                                            data.intro="<b>The track assignment method is perhaps the most important decision you'll make in your analysis! </b> <br><br>
                                                             The track assignment method is the logic that Nautilus uses to determine which target each track was tracking. We'll talk more about this
                                                             at the end of the tutorial, and focus on the 'Point' method for now. The point method simply assigns each individual sensor point to the
                                                             closest target. It's good for evaluating the positional accuracy of the sensor (i.e., when the sensor said something was there, was anything
                                                             really there?), but is less suited for evaluating track continuity and other tracking metrics for closely spaced targets, as even a momentary jump between nearby tracks will be regarded as a new track.
                                                             Also, by assinging sensor-reported positions to the closest target, it is actively minimizing the error, so it is a best-case scenario.
                                                             <br><br> Feel free to ignore the 'Cutoff Shape' options. These are for advanced users, and they might not do what you think. See documentation for details!"
                                        ),
                                        data.step=23,
                                        data.intro="There are three additional methods to choose beyond just point. <br><br> 'Whole Track' will assign entire tracks to the
                                                     closest target. It does not allow tracks to move between targets (even when they probably should). It's a fairly extreme choice,
                                                     but you may find it useful. <br><br> The two 'Window' methods (Gauss and Square) are <b>potentially very useful</b>. They both take an
                                                     additional cutoff parameter (which we'll talk about in a minute). Rather than simply assigning each sensor point to the nearest
                                                     target, they evaluate neighboring sensor points as well, assigning each sensor point based on an ensemble average of distances. This
                                                     has the effect of 'smoothing' jumps in tracks. This will tend to increase the positional error compared to the point method, but will
                                                     probably make track continuity metrics more reasonable. <br><br> <b>Remember: No method is necessarily more correct than any other.
                                                     These are just heuristic tools to help you with your analysis, rather than mathematically rigorous techniques. </b> See the
                                                     documentation for more information."
                                    ),
                                    ### Conditional panels to handle user options ###
                                    introBox(
                                        fluidRow(
                                            conditionalPanel(condition="input.cutoffStyle== 'spherical'",
                                                             column(3,numericInput("distanceCutoff", "Range Cutoff (meters)",value=1))
                                            ),
                                            conditionalPanel(condition="input.cutoffStyle== 'cylindrical'",
                                                             column(3,numericInput("distanceCutoff", "Horizontal Error Cutoff (meters)",value=1)),
                                                             column(3,numericInput("elevationCutoff", "Elevation Error Cutoff (meters)",value=1))
                                            ),
                                            conditionalPanel(condition="input.cutoffStyle== 'wedge'",
                                                             column(3,numericInput("distanceCutoff", "Downrange Error Cutoff (meters)",value=1)),
                                                             column(3,numericInput("elevationCutoff", "Elevation Error Cutoff (meters)",value=1)),
                                                             column(3,numericInput("bearingCutoff", "Bearing Error Cutoff (degrees)",value=1))
                                            )
                                        ),
                                        introBox(
                                            fluidRow(
                                                conditionalPanel(condition="input.method == 'Square Window' || input.method == 'Gauss Window'",
                                                                 column(3,numericInput("window", "Window Size (units of time)",value=0))
                                                )
                                            ),
                                            data.step=24,
                                            data.intro="The two window methods require an additional parameter. For the <b>Square Window</b>, your choice of cutoff determines
                                                             the size of the averaging window in time. For example, if you choose '10', then Nautilus will use neighboring sensor points within
                                                             5 seconds to calculate the ensemble average. For the <b>Gauss Window</b>, Nautilus uses the entire track as its ensemble average,
                                                             but uses a Gaussian (or 'normal') weight. This means that points close in time count a lot, while points far away count little. In this
                                                             case, your choice of window is the standard deviation of the Gaussian. See the documentation for a more thorough explanation."
                                        ),
                                        data.step=5,
                                        data.intro="Nautilus works by comparing tracks to truth data. Thus, it must figure out which tracks do <b>not</b> correspond
                                                         to <b>any</b> truth data. These could be tracks on clutter (e.g., waves, debris, etc.) or tracks on real things for which
                                                         we don't have truth data (e.g., non-instrumented assets in the test). Nautilus applies a simple metric: any sensor-reported
                                                         position further than a user-defined cutoff is marked as a 'false track,' and will be excluded from any calculations of sensor
                                                         performance. Put another way, anything 'wronger' than X meters isn't a bad track on a target, but a clutter or false track.
                                                         <br><br> Choosing an appropriate cutoff can be tricky! If you set it too large, you risk unfairly penalizing the sensor's positional
                                                         accuracy, if you set it too low, you risk unfairly penalizing the sensor's track continuity, while also unfairly improving the
                                                         sensor's position accuracy. The best advice is to guess-and-check. Set a starting value in the tens or low hundreds of
                                                         meters, assign tracks, and then check the 'Target Assignment' figure on the next tab (we'll show you how to do both in a moment).
                                                         Rinse and repeat until you are satisifed with the results. We've set the cutoff to 150 meters for this tutorial."
                                    ),



                                    #####################################
                                    ### Conditional Double Pass Panel ###
                                    #####################################
                                    checkboxInput("parallel","Parallel Processing",value=TRUE),

                                    introBox(

                                        checkboxInput("doublePass","Two-Pass Filtering",value=FALSE),

                                        conditionalPanel(condition="input.doublePass",
                                                         h4("1st Pass Cutoff Parameters (Spherical Cutoff Only)"),
                                                         fluidRow(
                                                             column(3,selectInput('methodFirstPass', 'Target Assignment Method', c("Point","Whole Track"))),
                                                             column(3,numericInput("cutoffFirstPass", "Range Cutoff (meters)",value=1)),
                                                             column(3,checkboxInput("discardFirstPass", "Discard 1st Pass False Tracks", value=FALSE))
                                                         )
                                        ),
                                        data.step=25,
                                        data.intro="The window methods can be slow, as can large data sets. To help speed things up, you can perform a 'two-pass filter.'
                                                         This works by first running the faster point or whole track method (your choice) to pull out the obviously false sensor points,
                                                         and then running the more expensive window method on the remaining points. If you use this, you should set a fairly large cutoff
                                                         distance. Remember, the goal is just to quickly flag the obviously bad points, and save the questionable ones for a different method. <br><br> Normally,
                                                         Nautilus will keep all the false tracks from both steps, but if you can change this by checking 'Discard 1st Pass False Tracks. This
                                                         is most useful when you have excessive obviously false tracks that you want to get rid of completely."
                                    )


                         ),
                         introBox(
                             introBox(
                                 miniButtonBlock(
                                     actionButton("runAssign", "Assign Tracks to Targets"),
                                     actionButton("saveAssociations", "Save Changes to Scenario")
                                     #downloadButton("saveassignmentData", "Save Scenario to Disk")
                                 ),

                                 data.step=6,
                                 data.intro="Once you've loaded your data, checked the truth data for gaps, and set your assignment parameters (method and cutoff),
                                                  you are ready for Nautilus to click 'Assign Tracks to Targets'. To speed things up, we've already clicked the button for you. For large data sets
                                                  and for the window methods this can take a long time. We'll come back to talk about that at the end. <b>We're now going to go to the next tab, called 'Visualize Data'.</b>"
                             ),
                             data.step = 26,
                             data.intro="Finally, once you're happy with your track assignments, you can click 'Save Calculated Track Associations to R'
                                              to save the dataframe to RStudio, or 'Save Calculated Track Associations to Disk' to save them as a csv. You can then load
                                              these results later if you'd like. <br><br> <b>This is the end of the Nautilus tutorial.</b> Nautilus has extensive documentation
                                              and built in help in RStudio (for example, try typing '?target_track_distance'). You should play around with the sample data
                                              until you're used to how Nautilus works."


                         )
                ),

                ##########################
                ### Visualize Data Tab ###
                ##########################
                tabPanel("Visualize Data", icon = icon("map-o"),
                         introBox(
                             introBox(
                                 introBox(
                                     fluidRow(column(width=3,selectInput('visType', 'Select Plot:', c("Truth and Sensor Data","Truth Data","Range vs. Time","Track Assignment","Track Status"))),
                                              column(2,radioButtons("graphType", "Graph with:",
                                                                    c("ggplot" = "ggplot",
                                                                      "plotly" = "plotly"),inline=TRUE))),
                                     data.step=7,
                                     data.intro="This tab allows you to take a look at your data in a few different ways. The default graph is 'Truth and Sensor Data.' You can also look
                                     just at truth data (helpful if you don't have any sensor data, or are trying to figure out what your truth data look like), you can look at the range
                                     of each target to the sensor platform as a function of time, or you can look at Track Assignments (we'll cover that last one in a minute)."
                                 ),
                                 data.step=10,
                                 data.intro="<b>Probably the most important graph here is 'Track Assignment,' </b> which shows you how Nautilus has assigned your sensor tracks to your
                                 truth data. For obvious reasons, you must run the track assignment on the previous tab before this graph will work.<br><br> You also have the option
                                 of plotting using the 'plotly' package instead. This is an interactive plot that lets you drag and zoom, among other things. It is very useful, but can get
                                 slow for very large data sets."
                             ),
                             introBox(
                                 uiOutput("drawSlider"),
                                 data.step=9,
                                 data.intro="By using this slider, you can truncate the start or end time of the data. This is useful if you want to look at a particular time in the event,
                                 especially if lots of tracks overlap. If you click on the slider, you can use the left and right arrow keys to move it. Clicking 'Lock Zoom Location' will
                                 keep the right-hand graph zoomed in even if you change the time slider (otherwise, it will reset the map whenever you change the time slider)."
                             ),
                             data.step=12,
                             data.intro="'Track Status' tries to boil down the complicated track assignment picture into a simple one"
                         ),
                         introBox(
                             introBox(
                                 introBox(
                                     conditionalPanel(
                                         condition="input.graphType == 'ggplot'",
                                         fillRow(
                                             plotOutput(
                                                 outputId="mainPlot",
                                                 brush = brushOpts(
                                                     id = "mainPlot_brush",
                                                     resetOnNew = TRUE
                                                 ),
                                                 height="600px"
                                             ),
                                             plotOutput(outputId="zoomPlot", height="600px"))
                                     ),
                                     conditionalPanel(
                                         condition="input.graphType == 'plotly'",
                                         fluidRow(
                                             column(width=1),
                                             column(width=10,plotly::plotlyOutput("plotlyPlot",width="1000px", height="690px")),
                                             column(width=1)
                                         )
                                     ),

                                     data.step=8,
                                     data.intro="This graph plots truth data as a solid line, and sensor data as dots connected by a line. Sensor tracks are labeled at the end of the track, and truth is labeled at the
                                     start (to reduce label clutter). <br><br> The graph on the left is the overview. If you draw a box in it with your mouse, the right-hand figure
                                     will zoom in. You can also drag the box around. <b>Note:</b> you can use this graph to look at your data before you click 'Assign Tracks to Targets.'"
                                 ),
                                 data.step=11,
                                 data.intro="This graph shows which sensor points were assigned to which truth tracks by connecting them with a black line. Sensor points that were marked as
                                 false tracks (based on the user-defined cutoff parameter) are semi-transparent black dots. After running the Track Assignment, <b>you should always check this
                                 graph to ensure that the track assignments are reasonable!</b> If not, you should adjust your parameters on the first tab. Beware that sometimes perfectly
                                 reasonable track assignments can look odd. Just because a sensor track and a truth target are close together on the map, doesn't mean that they were
                                 there at the <b>same time</b>. You can use the time slider to help investigate these cases.<br><br><b>We recommend that you come back at the end of the tutorial
                                 and play around with this graph some more.</b>"
                             ),
                             data.step=13,
                             data.intro="Track Status color codes each truth track to show when it was untracked (red), tracked by a single sensor track (blue), or tracked by
                             more than one sensor track (yellow). The black dots show false tracks (you can turn them off). This is a very useful summary figure to give you
                             a feel for the overall story. <br><br> <b>Now we'll move to the next tab.</b>"
                         )
                ),

                ###########################
                ### Summary Figures Tab ###
                ###########################
                tabPanel("Summary Figures", icon = icon("area-chart"),

                         introBox(
                             introBox(
                                 introBox(
                                     fluidRow(column(width=3,selectInput('summaryChoice', 'Select Plot:', c("Error Plot","Polar Error Plot","Overall Track Coverage","Unique Track Coverage","Range vs. Bearing Error")))),
                                     #column(width=2,downloadButton("sumFig", "Save Plot"))),
                                     data.step=14,
                                     data.intro="The 'Summary Figures' tab provides some useful figures to help you understand your results. We'll cover 3 of the 5 of them in this tutorial. You should
                                     only use any of these figures once you're happy with your track assignments."
                                 ),
                                 data.step=17,
                                 data.intro="Another useful graph is the 'Overall Track Coverage.'"
                             ),
                             data.step=19,
                             data.intro="Finally, let's take a look at 'Range vs. Bearing Error.'"
                         ),
                         uiOutput("drawSummarySlider"),
                         ### Conditional panels to handle user options ###
                         introBox(
                             conditionalPanel(condition="input.summaryChoice == 'Error Plot'",
                                              fluidRow(
                                                  column(2,selectInput('xTerm', 'Select X Variable:', c("Time","Range to Ship"))),
                                                  column(2,selectInput('yTerm', 'Select Y Variable:', c("locationError","downrangeError","bearingError"))),
                                                  column(2,selectInput('colorTerm', 'Select Factor:', c("trackNum","tgtAssigned"))),
                                                  column(2,checkboxInput("facet","Facet",value=FALSE)),
                                                  column(2,checkboxInput("plotFalse","False Tracks",value=FALSE))
                                              )),
                             data.step=16,
                             data.intro="You can customize the graph by changing the x axis (time or range to sensor), y axis (location error, range error, bearing error, and you can
                             view it either by track or by target. You can also choose to include the sensor points marked as false tracks by Nautilus. The 'Facet' option breaks the
                             graph into many mini-graphs, which may make it easier to read."
                         ),
                         conditionalPanel(condition="input.summaryChoice == 'Polar Error Plot'",
                                          fluidRow(
                                              column(2,selectInput('angleTerm', 'Select Angular Variable:', c("bearing","aspect"))),
                                              column(2,selectInput('rTerm', 'Select Radial Variable:', c("locationError","downrangeError","bearingError"))),
                                              column(2,selectInput('colorTerm', 'Select Factor:', c("tgtAssigned","trackNum"))),
                                              column(2,checkboxInput("facetPolar","Facet",value=FALSE)),
                                              column(2,checkboxInput("plotFalsePolar","False Tracks",value=FALSE))
                                          )),

                         introBox(
                             introBox(
                                 introBox(
                                     plotOutput(outputId="summaryPlot"),
                                     data.step=15,
                                     data.intro="The 'Error Plot' simply shows the sensor's positional error (also know as target location error, or TLE), as a function of either time for each track."
                                 ),
                                 data.step=18,
                                 data.intro="This graph shows each target as a white bar, starting when the target first appears, and ending when the target's truth data stops updating. The black
                                 boxes show the times during which Nautilus thinks the target was being tracked. For this graph, Nautilus doesn't care if there are multiple simultaneous tracks
                                 on the same target - it just looks to see if the target was tracked. The 'Unique Track Coverage' graph, not demonstrated in this tutorial, does a good job
                                 of showing you whether your target had multiple different tracks (simultaneous or otherwise). <br><br> Remember that your choice of track assignment method
                                 plays a role here. The point method is unforgiving toward track continuity if tracks are jumping between nearby targets, as it always chooses the closest
                                 target (even if the rest of the track is clearly tracking a different target. The whole track and window methods are 'sticky,' and tend to smooth over
                                 minor track jumps."
                             ),
                             data.step=20,
                             data.intro="This is a very handy graph that shows bearing and range error simultaneously. A negative range error means that the
                             sensor-reported is position is <b>further</b> away than the real target, and positive means that the sensor-reported position is
                             <b>closer</b>than the real target. The density graphs on the top and side of the figure help you to see if there is a bias
                             in the error. <br><br> <b>Let's go to the last tab.</b>"
                         ),

                         tags$style(type='text/css', "#sumFig {width:100%; margin-top: 25px;}")

                ),






                ##########################
                ### Summary Tables Tab ###
                ##########################
                tabPanel("Summary Tables", icon = icon("table"),
                         introBox(
                             fluidRow(column(width=3,selectInput('tableType', 'Select Table:', c("Detection Time/Accuracy","Time Tracked"))),
                                      column(width=2,downloadButton("dataTable", "Save Table"))),
                             data.step=21,
                             data.intro="This tab is quite simple. You can choose between two different tables that help to summarize sensor accuracy,
                             detection ranges, and the total time each target was tracked. <br><br> <b>Let's jump back to the Parameters tab to cover
                             a few advanced features.</b>"
                         ),
                         fluidRow(textOutput("readyForTables")),
                         DT::dataTableOutput("table"),
                         tags$style(type='text/css', "#dataTable {width:100%; margin-top: 25px;}")

                )
            )
        )


    nautilusServer <- function(input, output, session) {

        values = reactiveValues(scenario = 'None',
                                dataTable= NULL,
                                vizFig = NULL,
                                sumFig=NULL,
                                lockZoomX=NULL,
                                lockZoomY=NULL)




        ### Run Nautilus's assignment code ####################################################################################
        assignTargets = eventReactive(input$runAssign, {

            cutoffVector=switch(input$cutoffStyle,
                                "spherical" = c(input$distanceCutoff),
                                "cylindrical" = c(input$distanceCutoff, input$elevationCutoff),
                                "wedge" = c(input$distanceCutoff, input$bearingCutoff, input$elevationCutoff)

            )

            cat("Assigning Tracks to Targets\n")
            if (input$doublePass == FALSE) {
                withProgress(message = 'Calculating Track-to-Target Distances', value=0, {

                    incProgress(.5, message = "Assigning Tracks to Targets", detail="This Step May Take Several Minutes")

                    values$scenario=  switch(input$method,
                                             "Point" = target_assignment( values$scenario , method='point', cutoff=cutoffVector),
                                             "Whole Track" = target_assignment( values$scenario , method='wholeTrack', cutoff=cutoffVector),
                                             "Square Window" = target_assignment( values$scenario , method='windowSquare', cutoff=cutoffVector, windowSize=input$window, parallel=input$parallel),
                                             "Gauss Window" = target_assignment( values$scenario , method='windowGauss', cutoff=cutoffVector, windowSize=input$window, parallel=input$parallel)
                    )
                    count=values$scenario$assignmentData %>% group_by(isFalseTrack) %>% summarize("Is False Track?"=n())
                    print(count)
                })
            } else {

                withProgress(message = 'Calculating Track-to-Target Distances With 1st Method', value=0, {

                    incProgress(.25, message = "Assigning Tracks to Targets With 1st Method", detail="This Step May Take Several Minutes")

                    tmpScenario =  switch(input$methodFirstPass,
                                          "Point" = target_assignment( values$scenario , method='point', cutoff=input$cutoffFirstPass),
                                          "Whole Track" = target_assignment( values$scenario , method='wholeTrack', cutoff=input$cutoffFirstPass)
                    )

                    incProgress(.75, message = "Assigning Tracks to Targets With 2nd Method", detail="This Step May Take Several Minutes")

                    values$scenario=switch(input$method,
                                           "Point" = target_assignment_secondpass( tmpScenario , method='point', cutoff=cutoffVector,excludeFirstPass =input$discardFirstPass),
                                           "Whole Track" = target_assignment_secondpass( tmpScenario , method='wholeTrack', cutoff=cutoffVector,excludeFirstPass =input$discardFirstPass),
                                           "Square Window" = target_assignment_secondpass( tmpScenario , method='windowSquare', cutoff=cutoffVector, windowSize=input$window, parallel=input$parallel,excludeFirstPass =input$discardFirstPass),
                                           "Gauss Window" = target_assignment_secondpass( tmpScenario , method='windowGauss', cutoff=cutoffVector, windowSize=input$window, parallel=input$parallel,excludeFirstPass =input$discardFirstPass)
                    )



                })


            }
            cat("Assignment Complete\n")
        })

        ## Save calculated assignmentData
        saveAssociationData = eventReactive(input$saveAssociations, {
            req(is.list(values$scenario))
            cat("Saving Changes to Scenario\n")

            assign(as.character(input$scenario),values$scenario,envir=.GlobalEnv)
        })

        output$checkData=renderText({
            scenarioOptions=getScenarios(sampleData=FALSE)
            if (is.null(scenarioOptions)){
                showModal(modalDialog(title="Must Load Data","You must load your data in R BEFORE launching Interactive Nautilus (see documentation). For your convenience, Nautilus has automatically loaded a sample scenario.",easyClose=TRUE))
            }

            ""

        })

        output$summarizeScenario=renderText({
            if (is.list(values$scenario)){
                invalidateLater(1000)
                checkTruth=is.data.frame(values$scenario$targetTruth)
                checkSensor=is.data.frame(values$scenario$sensorData)
                checkOwnShip=is.data.frame(values$scenario$ownShipTruth)
                checkassignmentData=("assignmentData" %in% names(values$scenario))

                changesSaved=identical(eval(parse(text=input$scenario)),values$scenario)

                s=sprintf('<b>Scenario Contains:</b><br>Target Truth: %s<br>OwnShip Truth: %s<br>Sensor Data: %s<br>Target Assignment: %s<br>Changes Saved?: <b>%s</b>',
                          checkTruth,checkOwnShip,checkSensor,checkassignmentData,changesSaved)


                HTML(s)
            }

        })

        output$summarizeassignmentData=renderText({
            if (is.list(values$scenario)){
                if("assignmentParameters" %in% names(values$scenario)){
                    method=values$scenario$assignmentParameters$method
                    cutoff=values$scenario$assignmentParameters$cutoff
                    window=values$scenario$assignmentParameters$window


                    s=sprintf('<b>Target Assignment Parameters:</b><br>Method: %s<br>Cutoff: %s<br>Window: %s',
                              method,cutoff,window)


                    HTML(s)
                }
            }

        })

        output$summarizeassignmentData2ndPass=renderText({
            if (is.list(values$scenario)){
                if("assignmentParameters" %in% names(values$scenario)){

                    secondPassMethod=values$scenario$assignmentParameters$secondPassMethod
                    secondPassCutoff=values$scenario$assignmentParameters$secondPassCutoff
                    secondPassWindow=values$scenario$assignmentParameters$secondPassWindow

                    excludeFirstPass=values$scenario$assignmentParameters$excludeFirstPass

                    s=sprintf('<b>2nd Pass Assignment Parameters:</b><br>Second Pass Method: %s<br>Second Pass Cutoff: %s<br>Second Pass Window: %s<br>Exclude 1st Pass: %s',
                              secondPassMethod,secondPassCutoff,secondPassWindow,excludeFirstPass)


                    HTML(s)
                }
            }

        })



        output$readyForTables=renderText({
            if ("assignmentData" %in% names(values$scenario)){
                ""
            } else {
                "You must run Assign Tracks to Targets in the Parameters tab before this table will render."
            }

        })
        ####################################################################################################################################
        ### parameters tab #################################################################################################################
        ####################################################################################################################################

        observeEvent(input$runAssign,{ ### When you click "Assign Track to Targets", check for required dataframes and then go!


            if (!is.list(values$scenario)){
                #no scenario selected
                showModal(modalDialog(title="Cannot Assign Tracks to Targets","You must select a scenario before you can visualize any data.",easyClose=TRUE))
            }
            req(is.list(values$scenario))


            if (!is.data.frame(values$scenario$targetTruth)){
                showModal(modalDialog(title="Cannot Assign Tracks to Targets","This scenario does not contain targetTruth.",easyClose=TRUE))
            }
            if (!is.data.frame(values$scenario$sensorData)){
                showModal(modalDialog(title="Cannot Assign Tracks to Targets","This scenario does not contain sensorData.",easyClose=TRUE))
            }
            if (!is.data.frame(values$scenario$ownShipTruth)){
                showModal(modalDialog(title="Cannot Assign Tracks to Targets","This scenario does not contain ownShipTruth.",easyClose=TRUE))
            }

            req(is.data.frame(values$scenario$targetTruth))
            req(is.data.frame(values$scenario$sensorData))
            req(is.data.frame(values$scenario$ownShipTruth))

            assignTargets()
        })

        observeEvent(input$saveAssociations,{ ### save data on click
            saveAssociationData()
        })


        ### When input dataframes get selected, convert the text in the popup menu to an actual dataframe ###
        observeEvent(input$scenario,{
            if (input$scenario != "None"){
                values$scenario=eval(parse(text=input$scenario))
            } else {
                values$scenario="None"
            }
        })


        #################################
        ### Make data diagnostic plot ###
        #################################
        output$plotTruthGaps = renderPlot({
            req(is.list(values$scenario))

            scenarioMaker::plot_truth_gaps(values$scenario)

        })

        ##################################
        ### Make data diagnostic table ###
        ##################################
        output$diagnosticTable = DT::renderDataTable(({
            req(is.list(values$scenario))
            scenarioMaker::summarize_truth_gaps(values$scenario)

        }))


        ##################################################################################################################################
        ### Visualize Data Tab ###########################################################################################################
        ##################################################################################################################################

        ranges2 <- reactiveValues(x = NULL, y = NULL)

        output$drawSlider = renderUI({
            visType=input$visType

            warningMessage=""

            if (!is.list(values$scenario)){

                warningMessage="You must select a scenario before you can visualize any data."
                startTime=0 #make sure there's a default so we don't get errors
                stopTime=1

            } else {

                ### Get ready to draw the time slider ###
                startTime=0 #make sure there's a default so we don't get errors
                stopTime=1
                if (is.data.frame(values$scenario$targetTruth)){
                    startTime=min(values$scenario$targetTruth$time)
                    stopTime=max(values$scenario$targetTruth$time)
                } else if (is.data.frame(values$scenario$ownShipTruth)) { #if we're missing truthData, try ownShipData
                    startTime=min(values$scenario$ownShipTruth$time)
                    stopTime=max(values$scenario$ownShipTruth$time)
                }

                startTime=as.POSIXct(startTime,tz="UTC",origin="1970-01-01")
                stopTime=as.POSIXct(stopTime,tz="UTC",origin="1970-01-01")
            }


            if (input$graphType=="ggplot"){
                fluidPage(fluidRow(column(width=3,sliderInput("timeCutsVis", "Start/Stop Time", startTime, stopTime, value = c(startTime, stopTime), timezone="+0000")),
                                   column(width=2,checkboxInput("lockZoom","Lock Zoom Location",value=FALSE)),
                                   column(width=2,checkboxInput("hideLegend","Hide Legend", value=FALSE)),
                                   conditionalPanel(
                                       condition="input.visType== 'Truth and Sensor Data'",
                                       column(width=2,checkboxInput("useDefaultsTruthSensor", "Use Default Colors", value=TRUE))
                                   ),
                                   conditionalPanel(
                                       condition="input.visType== 'Truth Data'",
                                       column(width=2,checkboxInput("useDefaultsTruth", "Use Default Colors", value=TRUE))
                                   ),
                                   conditionalPanel(
                                       condition="input.visType== 'Range vs. Time'",
                                       column(width=2,checkboxInput("useDefaultsRangeTime", "Use Default Colors", value=TRUE))
                                   ),
                                   conditionalPanel(
                                       condition="input.visType== 'Track Assignment'",
                                       column(width=2,checkboxInput("plotFalseTracksAssignment", "Plot False Tracks", value=TRUE))
                                   ),
                                   conditionalPanel(
                                       condition="input.visType== 'Track Status'",
                                       column(width=2,checkboxInput("plotFalseTracksStatus", "Plot False Tracks", value=TRUE))
                                   )),
                          h4(warningMessage)
                )
            } else if (input$graphType=="plotly"){
                fluidPage(
                    fluidRow(column(width=3,sliderInput("timeCutsVis", "Start/Stop Time", startTime, stopTime, value = c(startTime, stopTime), timezone="+0000")),
                    conditionalPanel(
                        condition="input.visType== 'Truth and Sensor Data'",
                        column(width=2,checkboxInput("useDefaultsTruthSensor", "Use Default Colors", value=TRUE))
                    ),
                    conditionalPanel(
                        condition="input.visType== 'Truth Data'",
                        column(width=2,checkboxInput("useDefaultsTruth", "Use Default Colors", value=TRUE))
                    ),
                    conditionalPanel(
                        condition="input.visType== 'Range vs. Time'",
                        column(width=2,checkboxInput("useDefaultsRangeTime", "Use Default Colors", value=TRUE))
                    )),
                    h4(warningMessage)
                )

            }



        })


        #############################
        ### draw maps on viz data ###
        #############################
        observe({

            req(is.null(input$timeCutsVis) == FALSE)
            req(is.null(input$lockZoom) == FALSE)
            req(is.list(values$scenario) == TRUE)




            ### Let's trim the data to the slider bar times ###
            startTime=as.numeric(input$timeCutsVis[[1]])
            stopTime=as.numeric(input$timeCutsVis[[2]])

            localScenario=values$scenario
            if (is.data.frame(localScenario$ownShipTruth)){localScenario$ownShipTruth=filter(values$scenario$ownShipTruth,time>startTime,time<stopTime)}
            if (is.data.frame(localScenario$sensorData)){localScenario$sensorData=filter(values$scenario$sensorData,time>startTime,time<stopTime)}
            if (is.data.frame(localScenario$targetTruth)){localScenario$targetTruth=filter(values$scenario$targetTruth,time>startTime,time<stopTime)}
            if (is.data.frame(localScenario$assignmentData)){localScenario$assignmentData=filter(values$scenario$assignmentData,time>startTime,time<stopTime)}
            if (is.data.frame(localScenario$targetOwnShipDistance)){localScenario$targetOwnShipDistance=filter(values$scenario$targetOwnShipDistance,time>startTime,time<stopTime)}

            ### Let's get the right graph, based on the user's choices ###
            graphOut =  tryCatch(switch(input$visType,
                                        "Truth Data" = {
                                            switch(input$graphType,
                                                   'ggplot'= scenarioMaker::plot_truth_data(scenario=localScenario, offset=0, hideLegend=input$hideLegend, useDefaultColors = input$useDefaultsTruth),
                                                   'plotly'= scenarioMaker::plot_truth_data_plotly(scenario=localScenario, useDefaultColors = input$useDefaultsTruth)
                                            )
                                        },
                                        "Truth and Sensor Data" = {
                                            switch(input$graphType,
                                                   'ggplot'= scenarioMaker::plot_sensor_and_truth_data(scenario=localScenario, offset=0, hideLegend=input$hideLegend, useDefaultColors = input$useDefaultsTruthSensor),
                                                   'plotly'= scenarioMaker::plot_sensor_and_truth_data_plotly(scenario=localScenario, useDefaultColors = input$useDefaultsTruthSensor)
                                            )
                                        },
                                        "Range vs. Time" ={
                                            switch(input$graphType,
                                                   'ggplot'= scenarioMaker::plot_distance_data(scenario=localScenario, hideLegend=input$hideLegend, useDefaultColors = input$useDefaultsRangeTime),
                                                   'plotly'= scenarioMaker::plot_distance_data_plotly(scenario=localScenario, useDefaultColors = input$useDefaultsRangeTime)
                                            )
                                        },
                                        "Track Assignment"= {
                                            switch(input$graphType,
                                                   'ggplot'=plot_target_assignments(scenario=localScenario, scalePoints=30, showFalseTracks=input$plotFalseTracksAssignment, hideLegend=input$hideLegend),
                                                   'plotly'=plot_target_assignments_plotly(scenario=localScenario)
                                            )
                                        },
                                        "Track Status"= {
                                            switch(input$graphType,
                                                   'ggplot'=plot_track_status(scenario=localScenario, showFalseTracks=input$plotFalseTracksStatus, scalePoints=30, hideLegend=input$hideLegend),
                                                   'plotly'=plot_track_status_plotly(scenario=localScenario)
                                            )
                                        }

            ),error=function(err){return(err)})



            ### Let's plot the stuff now ###

            if (input$graphType == 'ggplot'){

                if (length(graphOut)<3){ #if the object is shorter than 3, it's an error, so throw a stop
                    output$mainPlot = renderPlot({stop(graphOut)})
                    output$zoomPlot = renderPlot({stop(graphOut)})
                } else { #if the object is longer than 2, it's a good graph, so graph it
                    ### Do the main (left-hand) plot ###
                    output$mainPlot = renderPlot({graphOut})

                    ### Do the zoomed (right-hand) plot ###
                    if (input$visType == "Range vs. Time"){
                        if (input$lockZoom==FALSE){ #uses coord_Cartesian
                            output$zoomPlot = renderPlot({graphOut+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)})
                        } else {
                            output$zoomPlot = renderPlot({graphOut+coord_cartesian(xlim = values$lockZoomX, ylim = values$lockZoomY)})
                        }
                    } else {
                        if (input$lockZoom==FALSE){ #uses coord_quickmap
                            output$zoomPlot = renderPlot({graphOut+coord_quickmap(xlim = ranges2$x, ylim = ranges2$y)})
                        } else {
                            output$zoomPlot = renderPlot({graphOut+coord_quickmap(xlim = values$lockZoomX, ylim = values$lockZoomY)})
                        }
                    }
                }

            } else if (input$graphType == 'plotly'){

                if (length(graphOut)>2){ #if the object is longer than 2, it's a good graph, so graph it
                    output$plotlyPlot=plotly::renderPlotly({plotlyPlot=graphOut; plotlyPlot$elementId=NULL;plotlyPlot }) #the elementId thing suppresses a meaningless warning
                } else { #if the object is 2 or shorter, it's an error, so throw it as a 'stop'
                    output$plotlyPlot=plotly::renderPlotly({stop(graphOut)})
                }

            }



        })




        ### This does the box-drawing for the viz figs
        observe({
            brush <- input$mainPlot_brush
            if (!is.null(brush)) {

                if (input$visType == "Range vs. Time") { #need this for the time x-axis for this plot
                    ranges2$x = as.POSIXct(c(brush$xmin, brush$xmax),tz="UTC",origin="1970-01-01")
                    values$lockZoomX = as.POSIXct(c(brush$xmin, brush$xmax),tz="UTC",origin="1970-01-01")

                } else {
                    ranges2$x <- c(brush$xmin, brush$xmax)
                    values$lockZoomX= c(brush$xmin, brush$xmax)

                }

                ranges2$y <- c(brush$ymin, brush$ymax)
                values$lockZoomY <- c(brush$ymin, brush$ymax)


            } else {
                ranges2$x <- NULL
                ranges2$y <- NULL
            }
        })



        ###########################################
        ### This plots stuff on summary figures ###
        ###########################################
        observe({
            #summaryChoice=input$summaryChoice

            req("assignmentData" %in% names(values$scenario))

            req(is.null(input$timeCutsSummary) == FALSE)


            ### Let's trim the data to the slider bar times ###
            startTime=as.numeric(input$timeCutsSummary[[1]])
            stopTime=as.numeric(input$timeCutsSummary[[2]])


            localScenario=values$scenario
            if (is.data.frame(localScenario$ownShipTruth)){localScenario$ownShipTruth=filter(values$scenario$ownShipTruth,time>startTime,time<stopTime)}
            if (is.data.frame(localScenario$sensorData)){localScenario$sensorData=filter(values$scenario$sensorData,time>startTime,time<stopTime)}
            if (is.data.frame(localScenario$targetTruth)){localScenario$targetTruth=filter(values$scenario$targetTruth,time>startTime,time<stopTime)}
            if (is.data.frame(localScenario$assignmentData)){localScenario$assignmentData=filter(values$scenario$assignmentData,time>startTime,time<stopTime)}
            if (is.data.frame(localScenario$targetOwnShipDistance)){localScenario$targetOwnShipDistance=filter(values$scenario$targetOwnShipDistance,time>startTime,time<stopTime)}


            output$summaryPlot =renderPlot({
                switch(input$summaryChoice,
                       "Error Plot" = plot_error(scenario=localScenario,rangeCutoff=input$distanceCutoff,xTerm=input$xTerm,yTerm=input$yTerm,colorTerm=input$colorTerm, doFacet=input$facet,plotFalseTracks=input$plotFalse),
                       "Polar Error Plot" = plot_polar_error(scenario=localScenario,angleTerm=input$angleTerm,rTerm=input$rTerm,colorTerm=input$colorTerm,doFacet=input$facetPolar,plotFalseTracks=input$plotFalsePolar),
                       "Unique Track Coverage" = plot_coverage_by_target(scenario=localScenario),
                       "Range vs. Bearing Error" = plot_scatterplot_with_density(scenario=localScenario, 'bearingError', 'downrangeError'),
                       "Overall Track Coverage" = plot_overall_coverage(scenario=localScenario))

            })


        })



        ### Draw the slider on the summary figures page ###
        output$drawSummarySlider = renderUI({
            summaryChoice=input$summaryChoice


            warningMessage=""

            if (!is.list(values$scenario)){
                warningMessage="You must select a scenario before you can visualize any data."
                startTime=0 #make sure there's a default so we don't get errors
                stopTime=1

            } else if (!("assignmentData" %in% names(values$scenario))){ #scenario exists, but assignmentData doesn't
                warningMessage="You must assign tracks to targets in this scenario before you can use the graphs on this tab."
                startTime=0 #make sure there's a default so we don't get errors
                stopTime=1
            } else {

                ### Get ready to draw the time slider ###
                startTime=0 #make sure there's a default so we don't get errors
                stopTime=1
                if (is.data.frame(values$scenario$targetTruth)){
                    startTime=min(values$scenario$targetTruth$time)
                    stopTime=max(values$scenario$targetTruth$time)
                } else if (is.data.frame(values$scenario$ownShipTruth)) { #if we're missing truthData, try ownShipData
                    startTime=min(values$scenario$ownShipTruth$time)
                    stopTime=max(values$scenario$ownShipTruth$time)
                }

                startTime=as.POSIXct(startTime,tz="UTC",origin="1970-01-01")
                stopTime=as.POSIXct(stopTime,tz="UTC",origin="1970-01-01")
            }



            fluidPage(fluidRow(column(width=3,sliderInput("timeCutsSummary", "Start/Stop Time", startTime, stopTime, value = c(startTime, stopTime), timezone="+0000"))),
                      h4(warningMessage)
            )



        })





        ##################################
        ### This handles table display ###
        ##################################

        observe({
            tableType=input$tableType
            req("assignmentData" %in% names(values$scenario))

            if (tableType== "Detection Time/Accuracy"){
                values$dataTable=summarize_performance(values$scenario)



            } else if (tableType=="Time Tracked"){
                values$dataTable=summarize_time_tracked(values$scenario)

            }
            output$table = DT::renderDataTable(({
                values$dataTable
            }))

        })

        output$dataTable <- downloadHandler(
            filename = function() {
                paste(input$runName,' - ',input$tableType,".csv",sep='')
            },
            content = function(file) {
                utils::write.csv(values$dataTable,file, row.names=FALSE)
            }
        )


        # output$saveassignmentData <- downloadHandler(
        #     filename = function() {
        #         paste(input$runName,".RData",sep='')
        #     },
        #     content = function(file) {
        #         save(values$scenario,file=file)
        #     }
        # )



        #########################
        ### Walkthrough Stuff ###
        #########################
        #start introjs when button is pressed with custom options and events
        observeEvent(input$help, {
            assign('sampleScenario',scenarioMaker::example1_scenario,envir=.GlobalEnv)
            updateSelectInput(session,"scenario",choices=c("None", getScenarios(sampleData=FALSE)),selected="sampleScenario")
            introjs(session,
                    events = list(
                        "onchange" = I("
                                       if (this._currentStep==0) {
                                       $('a[data-value=\"Visualize Data\"]').removeClass('active');
                                       $('a[data-value=\"Summary Figures\"]').removeClass('active');
                                       $('a[data-value=\"Summary Tables\"]').removeClass('active');
                                       $('a[data-value=\"Parameters\"]').addClass('active');
                                       $('a[data-value=\"Parameters\"]').trigger('click');
                                       }
                                       if (this._currentStep==1) {
                                       if (! $('#dataDiagnostics').prop('checked') ){
                                       $('#dataDiagnostics').trigger('click');
                                       }
                                       }
                                       if (this._currentStep==4) {
                                       $('#distanceCutoff').val('150');
                                       Shiny.onInputChange('distanceCutoff',150);

                                       //make sure the method is set to 'point'
                                       var $select = $(\"#method\").selectize();
                                       var selectize = $select[0].selectize;
                                       selectize.setValue('Point', false);
                                       }
                                       if (this._currentStep==5){
                                       $('#runAssign').trigger('click');
                                       }
                                       if (this._currentStep==6) {
                                       //transition to 2nd tab
                                       $('a[data-value=\"Summary Figures\"]').removeClass('active');
                                       $('a[data-value=\"Summary Tables\"]').removeClass('active');
                                       $('a[data-value=\"Parameters\"]').removeClass('active');
                                       $('a[data-value=\"Visualize Data\"]').addClass('active');
                                       $('a[data-value=\"Visualize Data\"]').trigger('click');

                                       //make sure that the first graph (Truth and Sensor Data) is selected from the dropdown menu
                                       var $select = $(\"#visType\").selectize();
                                       var selectize = $select[0].selectize;
                                       selectize.setValue('Truth and Sensor Data', false);

                                       Shiny.onInputChange('visType','Truth and Sensor Data');
                                       }
                                       if (this._currentStep==9) {
                                       //switch to the Track Assignment graph
                                       var $select = $(\"#visType\").selectize();
                                       var selectize = $select[0].selectize;
                                       selectize.setValue('Track Assignment', false);

                                       Shiny.onInputChange('visType','Track Assignment');
                                       }

                                       if (this._currentStep==11) {
                                       //switch to the Track Status graph
                                       var $select = $(\"#visType\").selectize();
                                       var selectize = $select[0].selectize;
                                       selectize.setValue('Track Status', false);

                                       Shiny.onInputChange('visType','Track Status');
                                       }
                                       if (this._currentStep==13) {
                                       //Switch to Summary figures and set the graph to Error Graph
                                       $('a[data-value=\"Summary Tables\"]').removeClass('active');
                                       $('a[data-value=\"Parameters\"]').removeClass('active');
                                       $('a[data-value=\"Visualize Data\"]').removeClass('active');
                                       $('a[data-value=\"Summary Figures\"]').addClass('active');
                                       $('a[data-value=\"Summary Figures\"]').trigger('click');

                                       var $select = $(\"#summaryChoice\").selectize();
                                       var selectize = $select[0].selectize;
                                       selectize.setValue('Error Plot', false);

                                       Shiny.onInputChange('summaryChoice','Error Plot');
                                       }
                                       if (this._currentStep==16) {
                                       //switch to the Overall Track Coverage graph
                                       var $select = $(\"#summaryChoice\").selectize();
                                       var selectize = $select[0].selectize;
                                       selectize.setValue('Overall Track Coverage', false);

                                       Shiny.onInputChange('summaryChoice','Overall Track Coverage');
                                       }
                                       if (this._currentStep==18) {
                                       //switch to the Range vs. Bearing Error graph
                                       var $select = $(\"#summaryChoice\").selectize();
                                       var selectize = $select[0].selectize;
                                       selectize.setValue('Range vs. Bearing Error', false);

                                       Shiny.onInputChange('summaryChoice','Range vs. Bearing Error');
                                       }
                                       if (this._currentStep==20) {
                                       //Switch to Summary Tables
                                       $('a[data-value=\"Summary Figures\"]').removeClass('active');
                                       $('a[data-value=\"Parameters\"]').removeClass('active');
                                       $('a[data-value=\"Visualize Data\"]').removeClass('active');
                                       $('a[data-value=\"Summary Tables\"]').addClass('active');
                                       $('a[data-value=\"Summary Tables\"]').trigger('click');
                                       }
                                       if (this._currentStep==21) {
                                       $('a[data-value=\"Visualize Data\"]').removeClass('active');
                                       $('a[data-value=\"Summary Figures\"]').removeClass('active');
                                       $('a[data-value=\"Summary Tables\"]').removeClass('active');
                                       $('a[data-value=\"Parameters\"]').addClass('active');
                                       $('a[data-value=\"Parameters\"]').trigger('click');
                                       }
                                       if (this._currentStep==22) {
                                       //make sure the method is set to 'point'
                                       var $select = $(\"#method\").selectize();
                                       var selectize = $select[0].selectize;
                                       selectize.setValue('Gauss Window', false);
                                       }
                                       if (this._currentStep==24) {
                                       if (! $('#doublePass').prop('checked') ){
                                       $('#doublePass').trigger('click');
                                       }
                                       }"

                        )
                    )

            )
        })



        observeEvent(input$done, {
            stopApp(values$assignmentData)
        })
    }
    runGadget(shinyApp(nautilusUI, nautilusServer),viewer = dialogViewer("",width=1200,height=1200))

}

#runGadget(shinyApp(ui, server),viewer = dialogViewer("Nautilus", width=1600,height=800))

#runMe = function(ownShip,sensorData,truthData){
#}
