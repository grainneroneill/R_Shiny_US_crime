setwd("/Users/grainneoneill/Documents/NYCDSA/Projects")
library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("United States Crime"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # select time range to display 
      sliderInput("year",
                  "Years",
                  value = c(2005, 2015),
                  min = 1975,
                  max = 2015
      ),
      
      # add options for crime types
      selectizeInput("var", "Crime",
                   choices = c("All Crimes", "Homicides", "Rapes"))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("timePlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$timePlot <- renderPlot({
    crime <- read.csv("crime_cleaned.csv")
    crime <- crime[c("report_year", "crimes_percapita")]
    #crime = crime[!duplicated(crime$report_year), ] # temporary
    
    #if (input$)
    
    # plot (chosen year range)
    crime[(crime$report_year >= input$year[1]) & (crime$report_year <= input$year[2]), ] %>% ggplot(aes(report_year, crimes_percapita)) + geom_line()
    
    #library(fpp2)
    #require(gridExtra)
    #p1 <- autoplot(ts(crime[input$n[1]:input$n[2], "crimes_percapita"])) +
    #  ggtitle("All Crimes Per Capita")
    
    #end = dim(crime)[1]
    #start = end - 100
    
    #if (input$model == "naive"){
    #  mod <- naive(crime[start : end, "Close"])
    #} else if (input$model == "ARIMA"){
    #  mod <- auto.arima(crime[start : end, "Close"])
    #} else {
    #  mod <- nnetar(crime[start : end, "Close"])
    #}
    #data <- forecast(mod, h = input$h)
    #p2 <- autoplot(forecast(mod, h = input$h)) + 
    #  ggtitle("Forecast for next 10 Days based on past 100 Days Price")
    
    #grid.arrange(p1, ncol=1)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)