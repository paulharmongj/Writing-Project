#
# README: The Shiny App created here is written by Paul Harmon. Its intent is to assess the sensitivity of the 
# Carnegie Classifications to small changes in the underlying variables used to calculate each index. 
#
# Note that the functions and data must be read in prior to running this code. The easiest way to do this right now is 
# probably is to just run all of the code in the R-Code for Carnegie Stuff.R file. We might source it in later .
#
#setwd("F:/Carnegie Classifications") #on personal computer
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sensitivity of the Carnegie Classifications"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("sosc",
                  "Number of Additional Social Science PhDs from 0:",
                  min = 0,
                  max = 60,
                  value = 0)
      ,
      
      sliderInput("other",
                  "Number of Additional Other PhDs from 9:",
                  min = -9,
                  max = 50,
                  value = 0)
      ,
      sliderInput("stem",
                  "Number of Additional STEM PhDs from 45:",
                  min = -45,
                  max = 50,
                  step = 5,
                  value = 0)
      ,
      sliderInput("hum",
                  "Number of Additional Humanities PhDs from 2:",
                  min = -2,
                  max = 50,
                  value = 0)
      ,
      sliderInput("staff",
                  "Additional Research Staff from 75:",
                  min = -75,
                  max = 2000,
                  value = 0,
                  step = 100)
      ,
      sliderInput("serd",
                  "Additional Stem Research Expenditures:",
                  min = -105000,
                  max = 2300000,
                  value = 0,
                  step = 10000)
      ,
      sliderInput("nonserd",
                  "Additional Non-Stem Research Expenditures:",
                  min = -9000,
                  max = 1000000,
                  value = 0,
                  step = 1000)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to change the classifications
cc2015Ps <- readRDS("Data/CC2015.rds")
a <- which(cc2015Ps$NAME == "Montana State University")
#define a few functions (outside the server)
minrank <- function(x){rank(x, ties.method = "min")}
AGcc <- function(x){
  #rank the data
  ranked <- data.frame(x[,1:3],sapply(x[,-c(1:3)],minrank)) 
  #get pc's
  pca.ranked <- prcomp(ranked[,-c(1:4)], scale = TRUE)
  summary <- summary(pca.ranked)
  standard.score <- scale(pca.ranked$x[,1], scale = TRUE, center = TRUE)
  #needs to return the standardized scores
  return(list(scorez = standard.score, sum =summary))
}
#function for percap
PCcc <- function(x){
  #rank the data
  ranked.dat <- data.frame(sapply(x,minrank)) 
  #get pc's
  pc.ranked <- prcomp(ranked.dat, scale = TRUE)
  summary <- summary(pc.ranked)
  standard.score <- scale(pc.ranked$x[,1], scale = TRUE, center = TRUE)
  return(list(scorez = standard.score, sum = summary))
}




server <- function(input, output) {
 # cc2015.full <- read.csv("Updated2015.csv", header = TRUE)

  
   output$distPlot <- renderPlot({
     
          ##To Move Left or Right
      # adds one phd to the number of social science phds
            cc2015Ps[a,"SOCSC_RSD"] <- cc2015Ps[a,"SOCSC_RSD"] + input$sosc
      #adds one phd to the number of other phds
            cc2015Ps[a,"OTHER_RSD"] <- cc2015Ps[a,"OTHER_RSD"] + input$other
      #adds one phd to the number of stem phds
            cc2015Ps[a,"STEM_RSD"] <- cc2015Ps[a,"STEM_RSD"] + input$stem
      #adds one phd to the number of humanities phds
            cc2015Ps[a,"HUM_RSD"] <- cc2015Ps[a,"HUM_RSD"] + input$hum
    ###To Move Up or Down:
      #adds research staff by 1 ; also adds to the FACNUM
            cc2015Ps[a,"PDNFRSTAFF"] <- cc2015Ps[a,"PDNFRSTAFF"] + input$staff
            
      #adds to NonSERD expenditures
            cc2015Ps[a,"NONS.ER.D"] <- cc2015Ps[a,"NONS.ER.D"] + input$nonserd
      #adds to SERD expenditures
            cc2015Ps[a,"S.ER.D"] <- cc2015Ps[a,"S.ER.D"] + input$serd
        
      ##now re-calculate the per-capita scores, with additional staff added in
      percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM
            
      percap <- PCcc(percap)
      ag <- AGcc(cc2015Ps)
      
      mean.percap <- 340.52
      sd.percap <- 170.51
      mean.ag <- 780.74
      sd.ag <- 413.10
      
      rawscores.percap <- sd.percap * -percap$scorez + mean.percap
      rawscores.ag <- sd.ag * ag$scorez + mean.ag
      
      plot(rawscores.ag, rawscores.percap, pch = 20, col = cc2015Ps$BASIC2015, asp = 1, xlab = "Aggregate Index", ylab = "Per Capita Index")
      title("2015 Carnegie Classifications")
      points(rawscores.ag[a],rawscores.percap[a],col = "blue", pch = 18)
      funk <- function(r,x){sqrt(r^2 - x^2)}
      x <- seq(500,984,by =1)
      r <- 984.007
      lines(x,funk(r,x), col = "gray50")
      x <- c(seq(1,409,by = 1),409.461)
      r<- 409.461
      
      lines(x,funk(r,x))
      legend('bottomright', fill = c(1,2,3), legend = c("R-1","R-2","R-3"))
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

