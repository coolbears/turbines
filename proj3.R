

library(shiny)
library(shinydashboard)

turbines <- read.csv('turbines.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Turbines Data"),
  
  
dashboardSidebar(),
  sidebarLayout(
    sidebarPanel(
      
      tags$img(src="windtub.jpg", height=300, width=250,alt="windtub"),
      
     
      #Select box for variable:
      selectInput("selectvar", label = h3("Choose a variable"), 
                  choices=list("Turbine Capacity"=1, "Turbine Hub Height"=2, "Project Capacity"=3,"Project Number of Turbines"=4,"State"=5), 
                  selected = 1),
      
      # Slider input for number of bins
      sliderInput("bins",
                  "Number of Bins:",
                  min =1,
                  max = 30,
                  value = 30),
      
    
      sliderInput("range", 
                  label = "Number of Turbines:",
                  min = min(turbines$Project.Number_Turbines), 
                  max = max(turbines$Project.Number_Turbines), 
                  value = max(turbines$Project.Number_Turbines)),
      
     
      
      #option to show mean of Turbine Variables
      checkboxInput("checkbox1", label="Display Mean For Turbine Values", value=FALSE),
      #project variables
      checkboxInput("checkbox3", label="Display Mean for Project Values", value=FALSE),
      
      
      #option to show sd
      checkboxInput("checkbox2", label="Display Standard Deviation for Turbine Values", value=FALSE),
      
      checkboxInput("checkbox4", label="Display Standard Deviation for Project Values", value=FALSE),
      
    
    checkboxInput("checkbox5", label="Purple Color Graph", value=FALSE),
    
    checkboxInput("checkbox6", label="Green Color Graph", value=FALSE),
    
    checkboxInput("checkbox7", label="State Statistics", value=FALSE),
  ),
  
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h6("Wind Turbines:"),
      
      
      plotOutput("distPlot"),
      
      
      hr(),
      
      p('Mean of Turbine Variable:'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      
      
      p('Standard Deviation of Turbine Variable:'),
      fluidRow(column(5, verbatimTextOutput("sd"))),
      
      p('Mean of Project Turbine Variables:'),
      fluidRow(column(5, verbatimTextOutput("meanproj"))),
      
      p('Standard Deviation of Project Turbine Variables:'),
      fluidRow(column(5, verbatimTextOutput("sdproj"))),
      
      p('State Statistics (Proportion Table):'),
      fluidRow(column(5, verbatimTextOutput("site"))),
      
    
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    turbines<-turbines[turbines$Project.Number_Turbines<= input$range,]
    
    
    if(input$selectvar == 1 & input$checkbox5 == TRUE ){
      hist(turbines$Turbine.Capacity, breaks = input$bins,main='Distribution of Turbine Capacity',xlab='Turbine Capacity (KW)',col = 'purple', border = 'darkgrey')
    }
    else if(input$selectvar == 1 & input$checkbox6 == TRUE ){
      hist(turbines$Turbine.Capacity, breaks = input$bins, main='Distribution of Turbine Capacity',xlab='Turbine Capacity (KW)',col = 'green', border = 'darkgrey')
    }
    
    if(input$selectvar == 2 & input$checkbox5 == TRUE){
      hist(turbines$Turbine.Hub_Height, breaks = input$bins, main='Distribution of Turbine Hub Heights',xlab='Turbine Hub Heights (KM)',col = 'purple', border = 'darkgrey')
      
    }
    else if (input$selectvar == 2 & input$checkbox6 == TRUE){
      hist(turbines$Turbine.Hub_Height, breaks = input$bins, main='Distribution of Turbine Hub Heights',xlab= 'Turbine Hub Heights (KM)',col = 'green', border = 'darkgrey')
      
    }
    if(input$selectvar == 3 & input$checkbox5 == TRUE ){
      hist(turbines$Project.Capacity, breaks = input$bins, main='Distribution of Project Capacity',xlab='Project Capacity (KW)',col = 'purple', border = 'darkgrey')
      
    }
    else if (input$selectvar == 3 & input$checkbox6 == TRUE ){
      hist(turbines$Project.Capacity, breaks = input$bins, main='Distribution of Project Capacity',xlab='Project Capacity (KW)',col = 'green', border = 'darkgrey')
      
    }
    if(input$selectvar == 4 & input$checkbox5  == TRUE ){
      hist(turbines$Project.Number_Turbines, breaks = input$bins,main='Distribution Number of Turbines',xlab='Number of Turbines',col = 'purple', border = 'darkgrey')
      
    }
    else if(input$selectvar == 4 & input$checkbox6  == TRUE ){
      hist(turbines$Project.Number_Turbines, breaks = input$bins,main='Distribution Number of Turbines',xlab='Number of Turbines',col = 'green', border = 'darkgrey')
      
    }
    if(input$selectvar == 5 & input$checkbox5  == TRUE ){
      barplot(table(turbines$Site.State),main='Frequency of Turbines According to State',xlab='States',ylab='Frequency of Turbines',col=c('aquamarine','purple'))
      
    }
    else if (input$selectvar == 5 & input$checkbox6  == TRUE ){
      barplot(table(turbines$Site.State),main='Frequency of Turbines According to State',xlab='States',ylab='Frequency of Turbines ',col=c('aquamarine','green'))
      
    }
  })
  
 
  
  #Display mean if selected
  output$mean <- renderPrint({ 
    
      if(input$checkbox1 == TRUE & input$selectvar == 1){
      mean(turbines$Turbine.Capacity, na.rm=TRUE)}
    
   else if(input$checkbox1 == TRUE & input$selectvar == 2) {
      mean(turbines$Turbine.Hub_Height, na.rm=TRUE)}
    
  })
  output$meanproj<-renderPrint({ 
    
    if(input$checkbox3 == TRUE & input$selectvar == 3){
      mean(turbines$Project.Capacity, na.rm=TRUE)}
    
    else if(input$checkbox1 == TRUE & input$selectvar == 4) {
      mean(turbines$Project.Number_Turbines, na.rm=TRUE)}
    
  })
  
  
  #Display sd if selected
  output$sd <- renderPrint({ 
    if(input$checkbox2 == TRUE & input$selectvar == 1){
      sd(turbines$Turbine.Capacity, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 2){
      sd(turbines$Turbine.Hub_Height, na.rm=TRUE)}
  })
  #Display sd if selected4
  output$sdproj <- renderPrint({ 
    if(input$checkbox2 == TRUE & input$selectvar == 3){
      sd(turbines$Project.Capacity, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 4){
      sd(turbines$Project.Number_Turbines, na.rm=TRUE)}
  })
  
  output$site <- renderPrint({ 
    if(input$checkbox7 == TRUE & input$selectvar == 5){
      prop.table(table(turbines$Site.State))
                 }
    
  })
  
}






# Run the application 
shinyApp(ui = ui, server = server)

