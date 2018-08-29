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
      menuItem("Rtakeoff", tabName = "rtakeoff", icon = icon("plane")),
      menuItem("Graphics", tabName = "graphics", icon = icon("line-chart"),
               # Accelerations
               checkboxInput("vrtgvalue", "VRTG - Vertical Acceleration", TRUE),
               checkboxInput("longvalue", "LONG - Longitudinal Acceleration", FALSE),
               checkboxInput("latgvalue", "LATG - Lateral Acceleration", FALSE),
               checkboxInput("fpacvalue", "FPAC - Flight Path Acceleration", FALSE),
               
               # Engines
               checkboxInput("n11value", "N11 - N1 Eng #1", TRUE),
               checkboxInput("n12value", "N12 - N1 Eng #2", FALSE),
               checkboxInput("n21value", "N21 - N2 Eng #1", FALSE),
               checkboxInput("n22value", "N22 - N2 Eng #2", FALSE),  
               checkboxInput("ff1value", "FF1 - Fuel Flow Eng #1", FALSE),
               checkboxInput("ff2value", "FF2 - Fuel Flow Eng #2", FALSE),
               checkboxInput("tla1value", "TLA1 - Thrust Lever Angle Eng #1", FALSE),
               checkboxInput("tla2value", "TLA2 - Thrust Lever Angle Eng #2", FALSE),
               checkboxInput("tla1cvalue", "TLA1C - Thrust Lever Position Eng #1", FALSE),
               checkboxInput("tla2cvalue", "TLA2C - Thrust Lever Position Eng #2", FALSE),
               checkboxInput("egt1value", "EGT1 - Exahust Gas Temp Eng #1", FALSE),
               checkboxInput("egt2value", "EGT2 - Exahust Gas Temp Eng #2", FALSE),
               checkboxInput("p01value", "P0_1 - Selected P0 (Amb Press) Eng #1", FALSE),
               checkboxInput("p02value", "P0_2 - Selected P0 (Amb Press) Eng #2", FALSE),
               checkboxInput("p21value", "P2_1 - PT2 Fan Inlet Press Eng #1", FALSE),
               checkboxInput("p22value", "P2_2 - PT2 Fan Inlet Press Eng #2", FALSE),
               checkboxInput("p31value", "P31 - PS3 HPC Exit Press Eng #1", FALSE),
               checkboxInput("p32value", "P32 - PS3 HPC Exit Press Eng #2", FALSE),
               checkboxInput("t121value", "T12.1 - PS3 HPC Exit Press Eng #1", FALSE),
               checkboxInput("t122value", "T12.2 - PS3 HPC Exit Press Eng #2", FALSE),
               checkboxInput("t251value", "T25_1 - LPC Exit Temp Eng #1", FALSE),
               checkboxInput("t252value", "T25_2 - PS3 HPC Exit Temp Eng #2", FALSE),
               
               # Other - Baro / Inertial / etc..
               checkboxInput("gsvalue", "GS - Ground Speed", TRUE),
               checkboxInput("iasvalue", "IAS - Indicated Air Speed", FALSE),
               checkboxInput("altvalue", "ALT - Standard Altitude", FALSE),
               checkboxInput("ralt1value", "RALT1 - Radio Altitude Sys#1", TRUE),
               checkboxInput("ralt2value", "RALT2 - Radio Altitude Sys#2", FALSE),
               checkboxInput("pitchvalue", "PITCH - Pitch angle", TRUE),
               checkboxInput("pitchcptvalue", "PITCH_CPT - Pitch Command CAPT", FALSE),
               checkboxInput("pitchfovalue", "PITCH_FO - Pitch Command FO", FALSE),
               checkboxInput("ptcrvalue", "PTCR - Pitch rate", FALSE),
               checkboxInput("satvalue", "SAT - Static Air Temperature", FALSE),
               checkboxInput("tatvalue", "TAT - Total Air Temperature", FALSE),
               checkboxInput("q1value", "Q1 - Dynamic Pressure Sys #1", FALSE),
               checkboxInput("q2value", "Q2 - Dynamic Pressure Sys #2", FALSE),
               checkboxInput("pt1value", "PT1 - Total Pressure Sys #1", FALSE),
               checkboxInput("pt2value", "PT2 - Total Pressure Sys #2", FALSE),
               checkboxInput("gwvalue", "GW - Gross Weight", FALSE)
               ),

      menuItem("Data Summary", tabName = "datasummary", icon = icon("th") )

      )),

  ## Body content
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "rtakeoff",
      #tabItem(tabName = "graf",
              h2("Takeoff Graphics"),
      
      conditionalPanel(
        condition = "input.vrtgvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_vrtg")
        )),
      
      conditionalPanel(
        condition = "input.longvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_long")
        )),
      
      
      conditionalPanel(
        condition = "input.latgvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_latg")
        )),
      
      conditionalPanel(
        condition = "input.fpacvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_fpac")
        )),
      
      
      conditionalPanel(
        condition = "input.n11value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_n11")
        )),
      
      conditionalPanel(
        condition = "input.n12value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_n12")
        )),
      
      conditionalPanel(
        condition = "input.n21value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_n21")
        )),
      
      conditionalPanel(
        condition = "input.n22value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_n22")
        )),
      

      conditionalPanel(
        condition = "input.ff1value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_ff1"))
      ),
      
      conditionalPanel(
        condition = "input.ff2value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_ff2")
        )),
      
      conditionalPanel(
        condition = "input.tla1value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_tla1")
        )),
      
      conditionalPanel(
        condition = "input.tla2value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_tla2")
        )),
      
      
      conditionalPanel(
        condition = "input.tla1cvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_tla1c")
        )),
      
      conditionalPanel(
        condition = "input.tla2cvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_tla2c")
        )),
      
      
      conditionalPanel(
        condition = "input.egt1value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_egt1"))
      ),
      
      conditionalPanel(
        condition = "input.egt2value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_egt2")
        )),
      
      conditionalPanel(
        condition = "input.p01value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_p01"))
      ),
      
      conditionalPanel(
        condition = "input.p02value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_p02"))
      ),
      
      conditionalPanel(
        condition = "input.p21value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_p21"))
      ),
      
      conditionalPanel(
        condition = "input.p22value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_p22"))
      ),
      
      conditionalPanel(
        condition = "input.p31value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_p31"))
      ),
      
      conditionalPanel(
        condition = "input.p32value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_p32"))
      ),
      
      conditionalPanel(
        condition = "input.t121value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_t121"))
      ),
      
      conditionalPanel(
        condition = "input.t122value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_t122"))
      ),
      
      conditionalPanel(
        condition = "input.t251value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_t251"))
      ),
      
      conditionalPanel(
        condition = "input.t252value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_t252"))
      ),
      
      conditionalPanel(
        condition = "input.gsvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_gs")
        )),
      
      conditionalPanel(
        condition = "input.iasvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_ias")
        )),
      
      conditionalPanel(
        condition = "input.altvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_alt")
        )),

      conditionalPanel(
        condition = "input.ralt1value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_ralt1")
        )),
      
      conditionalPanel(
        condition = "input.ralt2value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_ralt2")
        )),
      
      conditionalPanel(
        condition = "input.pitchvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_pitch")
        )),
              
      conditionalPanel(
        condition = "input.pitchcptvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_pitchcpt")
        )),
      
      conditionalPanel(
        condition = "input.pitchfovalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_pitchfo")
        )),
      
      
      conditionalPanel(
        condition = "input.ptcrvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_ptcr")
        )),
      
      conditionalPanel(
        condition = "input.satvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_sat")
        )),
      
      conditionalPanel(
        condition = "input.tatvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_tat")
        )),
      
      conditionalPanel(
        condition = "input.q1value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_q1")
        )),

      conditionalPanel(
        condition = "input.q2value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_q2")
        )),
      
      conditionalPanel(
        condition = "input.pt1value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_pt1")
        )),
      
      conditionalPanel(
        condition = "input.pt2value == true",
        box(width = "auto", height = 230,
            plotOutput("plot_pt2")
        )),
      
      conditionalPanel(
        condition = "input.gwvalue == true",
        box(width = "auto", height = 230,
            plotOutput("plot_gw")
        ))
      
      ),
      

       tabItem(tabName = "datasummary",
               h2("Future contents for Data Summary"))
               
               
      )))

 


#########################################################################################
server <- function(input, output) {

  # Data input
  # Load Data - sample flight
  load("/resources/rstudio/work/flightRtools/FlightDB/dadosVoo.RData")

  # determine the Takeoff start and stop time
  # Take-Off Start and End Points determination
  t0 <- min(which(fm_fwc==3)) - 200
  t1 <- max(which(fm_fwc==4)) + 50
  
  # Create data frame containing Takeoff area
  takeoff <- data.frame(seq(t0,t1),vrtg[t0:t1],pitch[t0:t1],raltd1[t0:t1]
                        ,n11[t0:t1],gs[t0:t1],fm_fwc[t0:t1],LG_left[t0:t1]
                        ,LG_right[t0:t1],ptcr[t0:t1],ff1[t0:t1],ff2[t0:t1]
                        ,n12[t0:t1],n21[t0:t1],n22[t0:t1],egt1[t0:t1],egt2[t0:t1]
                        ,raltd2[t0:t1],pitch_cpt[t0:t1],pitch_fo[t0:t1],long[t0:t1]
                        ,latg[t0:t1],sat[t0:t1],tat[t0:t1],tla1[t0:t1],tla2[t0:t1]
                        ,tla1c[t0:t1],tla2c[t0:t1],q1[t0:t1],q2[t0:t1],pt1[t0:t1],pt2[t0:t1]
                        ,p0_1[t0:t1],p0_2[t0:t1],gw1kg[t0:t1],p2_1[t0:t1],p2_2[t0:t1]
                        ,p31[t0:t1],p32[t0:t1],t12_1[t0:t1],t12_2[t0:t1],t25_1[t0:t1],t25_2[t0:t1]
                        ,alt_std[t0:t1],fpac[t0:t1],ias[t0:t1] )
  
  names(takeoff) <- c("time","VRTG","PITCH","RALT1","N11","GS","FM_FWC"
                      ,"LG_left", "LG_right", "PTCR","FF1","FF2","N12"
                      ,"N21","N22","EGT1","EGT2","RALT2","PITCH_CPT","PITCH_FO"
                      ,"LONG","LATG","SAT","TAT","TLA1","TLA2","TLA1C","TLA2C"
                      ,"Q1","Q2","PT1","PT2","P0_1","P0_2","GW","P2_1","P2_2",
                      "P31","P32","T12_1","T12_2","T25_1","T25_2","ALT","FPAC","IAS")
  
  # testing parameters - comment for final code
  # plot(latg[t0:t1], type="l",col="red")
  # plot(takeoff$VRTG, type="l",col="red")
  # plot(takeoff$PITCH, type="l",col="red")
  # End of testing parameters
  
  
  #########################
  #### General Calculations
  
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
  rotation <- min(which(ptcr[t0:t1]>1)) - 8  # 10 assigned empirically - change if necessary
  rotation_time <- (loff - rotation)/8  # in seconds
  # END Rotation point
  
  
  # output$plot1 <- renderPlot({
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
  
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
 
  output$plot_ff1 <- renderPlot({
    ggplot(data=takeoff, aes(y=FF1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_ff2 <- renderPlot({
    ggplot(data=takeoff, aes(y=FF2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_gs <- renderPlot({
    ggplot(data=takeoff, aes(y=GS, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_n12 <- renderPlot({
    ggplot(data=takeoff, aes(y=N12, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_n21 <- renderPlot({
    ggplot(data=takeoff, aes(y=N21, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_n22 <- renderPlot({
    ggplot(data=takeoff, aes(y=N22, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_egt1 <- renderPlot({
    ggplot(data=takeoff, aes(y=EGT1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_egt2 <- renderPlot({
    ggplot(data=takeoff, aes(y=EGT2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_ralt2 <- renderPlot({
    ggplot(data=takeoff, aes(y=RALT2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_pitchcpt <- renderPlot({
    ggplot(data=takeoff, aes(y=PITCH_CPT, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_pitchfo <- renderPlot({
    ggplot(data=takeoff, aes(y=PITCH_FO, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_long <- renderPlot({
    ggplot(data=takeoff, aes(y=LONG, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_latg <- renderPlot({
    ggplot(data=takeoff, aes(y=LATG, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_sat<- renderPlot({
    ggplot(data=takeoff, aes(y=SAT, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_tat<- renderPlot({
    ggplot(data=takeoff, aes(y=TAT, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_tla1<- renderPlot({
    ggplot(data=takeoff, aes(y=TLA1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_tla2<- renderPlot({
    ggplot(data=takeoff, aes(y=TLA2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
    
  output$plot_tla1c<- renderPlot({
    ggplot(data=takeoff, aes(y=TLA1C, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_tla2c<- renderPlot({
    ggplot(data=takeoff, aes(y=TLA2C, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_q1<- renderPlot({
    ggplot(data=takeoff, aes(y=Q1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_q2<- renderPlot({
    ggplot(data=takeoff, aes(y=Q2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_pt1<- renderPlot({
    ggplot(data=takeoff, aes(y=PT1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_pt2<- renderPlot({
    ggplot(data=takeoff, aes(y=PT2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_p01<- renderPlot({
    ggplot(data=takeoff, aes(y=P0_1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_p02<- renderPlot({
    ggplot(data=takeoff, aes(y=P0_2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_gw<- renderPlot({
    ggplot(data=takeoff, aes(y=GW, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_p21<- renderPlot({
    ggplot(data=takeoff, aes(y=P2_1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_p22<- renderPlot({
    ggplot(data=takeoff, aes(y=P2_2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_p31<- renderPlot({
    ggplot(data=takeoff, aes(y=P31, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_p32<- renderPlot({
    ggplot(data=takeoff, aes(y=P32, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_t121<- renderPlot({
    ggplot(data=takeoff, aes(y=T12_1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_t122<- renderPlot({
    ggplot(data=takeoff, aes(y=T12_2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_t251<- renderPlot({
    ggplot(data=takeoff, aes(y=T25_1, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_t252<- renderPlot({
    ggplot(data=takeoff, aes(y=T25_2, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_alt<- renderPlot({
    ggplot(data=takeoff, aes(y=ALT, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_fpac<- renderPlot({
    ggplot(data=takeoff, aes(y=FPAC, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  output$plot_ias<- renderPlot({
    ggplot(data=takeoff, aes(y=IAS, x=time)) + geom_line(color="red")+geom_vline(xintercept = t0+loff, color="blue")+geom_vline(xintercept = t0+rotation, color="green")
  }, height=200, width="auto")
  
  
  #output$value <- renderText({ input$somevalue })
  #output$test <- renderText({ "Test of sth written" })
   
}


shinyApp(ui, server)