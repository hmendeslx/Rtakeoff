## app.R ##
library(shinydashboard)
library(ggplot2)
#library(dbplyr) #Grammar for data manipulation - working with databases


# References:
#
# https://stackoverflow.com/questions/46887612/r-shiny-plot-size
# https://shiny.rstudio.com/articles/dynamic-ui.html
#
#

#########################################################################################
ui <- dashboardPage(
  
  dashboardHeader(title = "flightRtools"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      #menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem( "Rtakeoff", tabName = "graphics", icon = icon("plane")),
      
      menuItem("Data Summary", tabName = "Rtakeoff", icon = icon("th") )
      #menuSubItem("Sub-item 1", tabName = "subitem1"),
      #menuSubItem("Sub-item 2", tabName = "subitem2"))
    )),
  
  ## Body content
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "graphics",
              h2("Takeoff Graphics"),
              
              checkboxInput("vrtgvalue", "VRTG - Vertical Acceleration", FALSE),
              checkboxInput("n11value", "N11 - N1 Eng #1", FALSE),
              checkboxInput("pitchvalue", "PITCH - Pitch angle", FALSE),
              
              conditionalPanel(
                condition = "input.vrtgvalue == true",
                plotOutput("plot_vrtg")
              ),
              
              conditionalPanel(
                condition = "input.n11value == true",
                plotOutput("plot_n11")
              ),
              
              conditionalPanel(
                condition = "input.pitchvalue == true",
                plotOutput("plot_pitch")
              )
              
              
      ),
      
      
      
      
      # box(
      #   
      #   if ("value"=="TRUE") {
      #     verbatimTextOutput("value")
      #   }
      #     
      #     )
      # ),
      # 
      
      
      #box(width = "auto", height = 200),
      #    plotOutput("plot_vrtg")
      #         
      #         output$value <- renderText({ input$somevalue }),
      
      
      # #First tab content
      # tabItem(tabName = "dashboard",
      #         fluidRow(
      #           box(
      #             title = "Histogram", status = "primary", solidHeader = TRUE,
      #             collapsible = TRUE, height = 300,
      #             plotOutput("plot1", height = 250)
      #           ),
      #           
      #           box(
      #             title = "Inputs", status = "warning", solidHeader = TRUE, collapsible = TRUE,
      #             "Box content here", br(), "More box content",
      #             sliderInput("slider", "Slider input:", 1, 100, 50),
      #             textInput("text", "Text input:"), height = 300
      #           )
      #         ),
      #         fluidRow(
      #           tabBox(
      #             title = "First tabBox",
      #             # The id lets us use input$tabset1 on the server to find the current tab
      #             id = "tabset1", height = "250px",
      #             tabPanel("Tab1", "First tab content"),
      #             tabPanel("Tab2", "Tab content 2")
      #           )
      #         )
      # ),
      # 
      # Second tab content
      # tabItem(tabName = "widgets",
      #         h2("Widgets tab content")
      # ),
      # 3rd tab content
      
      
      ### convert to graphics
      # tabItem(tabName = "Rtakeoff",
      #         h2("Future contents for Rtakeoff"),
      #         
      #         box(width = "auto", height = 200,
      #             plotOutput("plot_vrtg")
      #         ),
      #checkboxInput("somevalue", "Check this box", FALSE),
      #verbatimTextOutput("value")
      
      tabItem(tabName = "Rtakeoff",
              h2("Future contents for Rtakeoff"),
              
              #box(width = "auto", height = 200,
              #  plotOutput("plot_vrtg")
              #),
              
              #box(width = "auto", height = 200,
              #    plotOutput("plot_n11")
              #),
              
              #box(width = "auto", height = 200,
              #    plotOutput("plot_pitch")
              #),
              
              box(width = "auto", height = 200,
                  plotOutput("plot_ralt1")
              ),
              
              box(width = "auto", height = 200,
                  plotOutput("plot_ptcr")
              )
      ))))




#########################################################################################
server <- function(input, output) {
  
  # Data input
  #set.seed(122)
  #histdata <- rnorm(500)
  # Load Data - sample flight
  load("/resources/rstudio/work/flightRtools/FlightDB/dadosVoo.RData")
  
  # determine the Takeoff start and stop time
  # Take-Off Start and End Points determination
  t0 <- min(which(fm_fwc==3)) - 200
  t1 <- max(which(fm_fwc==4)) + 50
  
  # Create data frame containing Takeoff area
  takeoff <- data.frame(seq(t0,t1),vrtg[t0:t1],pitch[t0:t1],raltd1[t0:t1]
                        ,n11[t0:t1],gs[t0:t1],fm_fwc[t0:t1],LG_left[t0:t1]
                        ,LG_right[t0:t1],ptcr[t0:t1])
  names(takeoff) <- c("time","VRTG","PITCH","RALT1","N11","GS","FM_FWC"
                      ,"LG_left", "LG_right", "PTCR")
  
  # testing parameters - comment for final code
  # plot(takeoff$VRTG, type="l",col="red")
  # plot(takeoff$PITCH, type="l",col="red")
  # plot(takeoff$N11, type="l",col="red")
  # plot(takeoff$RALT1, type="l",col="red")
  # plot(takeoff$GS, type="l",col="red")
  # plot(takeoff$FM_FWC, type="l",col="red")
  # End of testing parameters
  
  # Liftoff Calculation
  tlg <- min(which(LG_left[t0:t1]==0))
  trg <- min(which(LG_right[t0:t1]==0))
  
  loff <- max(tlg,trg) # Lift Off according to MLG criteria
  
  r1 <- raltd1[t0:t1]
  r2 <- raltd2[t0:t1]
  
  # loff radio alt correction
  if ( r1[loff] > 1  || r2[loff] > 1) {
    
    min <- loff - 20
    max <- loff + 10
    
    r1_positive <- min(which(r1[min:max]>0))
    lo_r1 <- min + r1_positive
    
    r2_positive <- min(which(r2[min:max]>0))
    lo_r2 <- min + r2_positive
    
    #lo_ralt <- min(lo_r1,lo_r2)
    loff <- max(lo_r1,lo_r2)    
  }
  
  #loff_sec <- loff/8 # in seconds -> use of this variable?
  ## END Liftoff Calculation
  
  # Rotation point
  rotation <- min(which(ptcr[t0:t1]>1)) - 10  # 10 assigned empirically - change if necessary
  rotation_time <- (loff - rotation)/8  # in seconds
  # END Rotation point
  
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot_vrtg <- renderPlot({
    ggplot(data=takeoff, aes(y=VRTG, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_pitch <- renderPlot({
    ggplot(data=takeoff, aes(y=PITCH, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_n11 <- renderPlot({
    ggplot(data=takeoff, aes(y=N11, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_ralt1 <- renderPlot({
    ggplot(data=takeoff, aes(y=RALT1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_ptcr <- renderPlot({
    ggplot(data=takeoff, aes(y=PTCR, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$value <- renderText({ input$somevalue })
  
  output$test <- renderText({ "Test of sth written" })
  
}


shinyApp(ui, server)