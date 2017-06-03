#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("F:/Carnegie Classifications") #on personal computer
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sensitivity of the Carnegie Classifications"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("sosc",
                  "Number of Additional Social Science PhDs:",
                  min = 0,
                  max = 50,
                  value = 0)
      ,
      
      sliderInput("other",
                  "Number of Additional Other PhDs:",
                  min = 0,
                  max = 50,
                  value = 0)
      ,
      sliderInput("stem",
                  "Number of Additional STEM PhDs:",
                  min = 0,
                  max = 50,
                  value = 0)
      ,
      sliderInput("hum",
                  "Number of Additional Humanities PhDs:",
                  min = 0,
                  max = 50,
                  value = 0)
      ,
      sliderInput("staff",
                  "Additional Research Staff:",
                  min = 1,
                  max = 50,
                  value = 0)
      ,
      sliderInput("serd",
                  "Additional Stem Research Expenditures:",
                  min = 1,
                  max = 100,
                  value = 0,
                  step = 10)
      ,
      sliderInput("nonserd",
                  "Additional Non-Stem Research Expenditures:",
                  min = 1,
                  max = 100,
                  value = 0,
                  step = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
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
      #adds research staff by 1  
            cc2015Ps[a,"PDNFRSTAFF"] <- cc2015Ps[a,"PDNFRSTAFF"] + input$staff
      #adds to NonSERD expenditures
            cc2015Ps[a,"NONS.ER.D"] <- cc2015Ps[a,"NONS.ER.D"] + input$nonserd
      #adds to SERD expenditures
            cc2015Ps[a,"S.ER.D"] <- cc2015Ps[a,"S.ER.D"] + input$serd
        
      ##plot the two scores with updated 
      percap <- PCcc(cc2015percap)
      ag <- AGcc(cc2015Ps)
      plot(ag$scorez, -percap$scorez, col = cc2015Ps$BASIC2015, pch = 20, xlab = "Aggregate Index", ylab = "Per Capita Index")
      title("Carnegie Classifications")
      points(ag$scorez[a],-percap$scorez[a],col = "blue", pch = 20)
      
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

