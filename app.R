library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(maps)
library(shinyWidgets)

#######
# UI
#######
ui <- dashboardPage(
  skin = "yellow-light",
  
  dashboardHeader(title = "US Crime Through Time", titleWidth = 250),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series by State", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("US Map", tabName = "map", icon = icon("map")),
      menuItem("About Me", tabName = "about", icon = icon("face-smile"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Time Series by State
      tabItem(tabName = "timeseries",
              # Sidebar
              sidebarLayout(
                sidebarPanel(
                  # select time range
                  chooseSliderSkin("Simple"),
                  sliderInput("year", "Years", value = c(2005, 2014), min = 1975, max = 2014),
                  
                  # drop down menu for crime type
                  selectizeInput("var", "Crime",
                                 choices = c("All Violent Crimes", "Homicides", 
                                             "Rapes", "Assaults", "Robberies")),
                  
                  # drop down menu for state
                  selectizeInput("st", "State", choices = state.abb, selected = "NY")),
                
                # Show plot
                mainPanel(
                  
                  textOutput("error"), # Allows me to display text in main panel when missing data
                  
                  plotOutput("timePlot")
                  )
                )
              ),
      
      # US Map
      tabItem(tabName = "map",
              # Sidebar
              sidebarLayout(
                sidebarPanel(
                  # drop down menu for year
                  selectizeInput("yr", "Year",
                                 choices = seq(1975, 2014),
                                 selected = 2014),
                  
                  # drop down menu for crime type
                  selectizeInput("cr", "Crime",
                                 choices = c("All Violent Crimes", "Homicides", 
                                             "Rapes", "Assaults", "Robberies")),
                  ),
                # Show plot
                mainPanel(
                  plotOutput("mapPlot")
                )
              )
      ),
      
      # About Me
      tabItem(tabName = "about")
      
      )
  )
  )


#######
# Server
#######

server <- function(input, output) {
  
  # Time Series
  output$timePlot <- renderPlot({
    crime_1 <- read.csv("crime_cleaned.csv")
    crime <- crime_1[c("report_year", "city", "state", "crimes_percapita", "homicides_percapita", 
                     "rapes_percapita", "assaults_percapita", "robberies_percapita")]
    
    # Which crime did they choose?
    if (input$var == "All Violent Crimes"){
      crime_subset = crime[c("report_year", "crimes_percapita", "city", "state")]
    } else if (input$var == "Homicides"){
      crime_subset = crime[c("report_year", "homicides_percapita", "city", "state")]
    } else if (input$var == "Rapes"){
      crime_subset = crime[c("report_year", "rapes_percapita", "city", "state")]
    } else if (input$var == "Assaults"){
      crime_subset = crime[c("report_year", "assaults_percapita", "city", "state")]
    } else if (input$var == "Robberies"){
      crime_subset = crime[c("report_year", "robberies_percapita", "city", "state")]
    }
    
    # plot (chosen crime, in chosen year range, in chosen state. one line for each city)
    # if there's no data for the state, display message
    if (dim(crime_subset[crime_subset$state == input$st, ])[1] == 0) {
      
    output$error <- renderText({ 
      "No data available."
      })
    
    } else {
    colnames(crime_subset) = c("report_year", "y", "city", "state") # rename columns so i can use arbitrary y
    crime_subset[(crime_subset$report_year >= input$year[1]) &
                   (crime_subset$report_year <= input$year[2]) &
                   (crime_subset$state == input$st), ] %>%
      ggplot(aes(report_year, y, group=city, color=city)) + geom_line() +
      labs(title=paste(input$var, "Per Capita"), x ="Years", y = paste(input$var, "per 100,000 people"))
    }
  })
  
  
  # Map
  output$mapPlot <- renderPlot({
    crime_1 <- read.csv("crime_cleaned.csv")
    crime_2 <- crime_1[c("report_year", "state", "population", "violent_crimes",
                     "homicides", "rapes", "assaults", "robberies")]
    
    # create empty df and fill with summed crimes by state, then calculate per capita
    crime_states <- data.frame(matrix(ncol = 0, nrow = length(unique(crime_2$state))))
    crime_states = crime_2 %>% group_by(state, report_year) %>% 
      summarise_at(vars(population, violent_crimes, homicides, rapes, assaults, robberies), sum)
    # calculate per capita
    crime_states$crimes_percapita = crime_states %>% with(100000*violent_crimes/population)
    crime_states$homicides_percapita = crime_states %>% with(100000*homicides/population)
    crime_states$rapes_percapita = crime_states %>% with(100000*rapes/population)
    crime_states$assaults_percapita = crime_states %>% with(100000*assaults/population)
    crime_states$robberies_percapita = crime_states %>% with(100000*robberies/population)

    # map (in chosen year)
    # make column of full state name in lower case
    crime_states$region = tolower(state.name[match(crime_states$state,state.abb)])
    # chosen crime
    if (input$cr == "All Violent Crimes"){
      crime_states_sub = crime_states[c("report_year", "crimes_percapita", "region")]
    } else if (input$cr == "Homicides"){
      crime_states_sub = crime_states[c("report_year", "homicides_percapita", "region")]
    } else if (input$cr == "Rapes"){
      crime_states_sub = crime_states[c("report_year", "rapes_percapita", "region")]
    } else if (input$cr == "Assaults"){
      crime_states_sub = crime_states[c("report_year", "assaults_percapita", "region")]
    } else if (input$cr == "Robberies"){
      crime_states_sub = crime_states[c("report_year", "robberies_percapita", "region")]
    }
    colnames(crime_states_sub) = c("report_year", "y", "region") # rename columns so i can use arbitrary y
    # get state dataset
    states <- map_data("state")
    # only chosen year
    crime_states_yr = crime_states_sub[crime_states_sub$report_year == input$yr, ]
    # merge my df with state df
    map.df <- merge(states,crime_states_yr, by="region", all.crime_states_yr=T)
    map.df <- map.df[order(map.df$order),]
    #plot
    ggplot(map.df, aes(x=long,y=lat,group=group)) +
      geom_polygon(aes(fill=y)) +
      geom_path() + 
      scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90") +
      labs(title=paste(input$cr, "Per Capita")) +
      coord_map()
    
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)