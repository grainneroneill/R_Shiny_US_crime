library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(maps)
library(shinyWidgets)
library(mapproj)

#######
# UI
#######
ui <- dashboardPage(
  skin = "yellow-light",
  
  dashboardHeader(title = "US Crime Through Time", titleWidth = 250),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("house")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("US Map by Year", tabName = "map", icon = icon("map")),
      menuItem("US Map Changes", tabName = "map2", icon = icon("map")),
      menuItem("About Me", tabName = "about", icon = icon("face-smile"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Welcome
      tabItem(tabName = "welcome",
              h1("Welcome!", 
                 style="text-align:center"),
              h2("This is a Shiny App all about US Crime Data!", 
                 style="text-align:center"),
              h4("Explore the interactive visualization tools in 
                 the Time Series and US Map tabs.", 
                 style="text-align:center"),
              h1("・ ・ ・ ・ ・", 
                 style="text-align:center"),
              div(img(src='map.gif', width="100%"), style="text-align: center;")
      ),
      
      # Time Series by State
      tabItem(tabName = "timeseries",
              h2("Time Series of Crime by State"), #heading
              # Sidebar
              sidebarLayout(
                sidebarPanel(
                  # select time range
                  chooseSliderSkin("Flat"),
                  sliderInput("year", "Years", value = c(2005, 2015), min = 1975, max = 2015),
                  
                  # drop down menu for crime type
                  selectizeInput("var", "Crime",
                                 choices = c("All Violent Crimes", "Homicides", 
                                             "Rapes", "Assaults", "Robberies")),
                  
                  # drop down menu for state
                  selectizeInput("st", "State", choices = state.abb, selected = "NY")),
                
                # Show plot
                mainPanel(
                  
                  textOutput("error"), # Allows me to display text in main panel when missing data
                  tags$head(tags$style("#error{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
                  
                  plotOutput("timePlot", height = 600)
                )
              )
      ),
      
      # US Map by year (1)
      tabItem(tabName = "map",
              h2("Heat Map of US Crime by Year"),
              # Sidebar
              sidebarLayout(
                sidebarPanel(
                  # drop down menu for year
                  selectizeInput("yr", "Year",
                                 choices = seq(1975, 2015),
                                 selected = 2015),
                  
                  # drop down menu for crime type
                  selectizeInput("cr", "Crime",
                                 choices = c("All Violent Crimes", "Homicides", 
                                             "Rapes", "Assaults", "Robberies")),
                ),
                # Show plot
                mainPanel(
                  plotOutput("mapPlot", height = 600)
                )
              )
      ),
      
      # US Map years difference (2)
      tabItem(tabName = "map2",
              h2("Heat Map of changes in US Crime"),
              # Sidebar
              sidebarLayout(
                sidebarPanel(
                  # slider to choose year
                  chooseSliderSkin("Flat"),
                  sliderInput("yrs", "Years", value = c(2010, 2015), min = 1975, max = 2015),
                  
                  # drop down menu for crime type
                  selectizeInput("crm", "Crime",
                                 choices = c("All Violent Crimes", "Homicides", 
                                             "Rapes", "Assaults", "Robberies")),
                ),
                # Show plot
                mainPanel(
                  plotOutput("mapPlot2", height = 600)
                )
              ),
              uiOutput("tab")
      ),
      
      # About Me
      tabItem(tabName = "about",
              h2("Grainne O'Neill", 
                 style="text-align:center"),
              h4("I created this app as part of my fellowship at NYC Data Science Academy. 
                 Read my blog to learn more about this app and some of my other projects.", 
                 style="text-align:center"),
              
              div(img(src='photo.png', height="280"), style="text-align: center;"),
              
              uiOutput("tab3"),
              uiOutput("tab1"),
              uiOutput("tab2")
      )
      
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
      output$error <- renderText({ 
        ""
      })
    }
    
    colnames(crime_subset) = c("report_year", "y", "city", "state") # rename columns so i can use arbitrary y
    crime_subset[(crime_subset$report_year >= input$year[1]) &
                   (crime_subset$report_year <= input$year[2]) &
                   (crime_subset$state == input$st), ] %>%
      ggplot(aes(report_year, y, group=city, color=city)) + geom_line() +
      labs(title=paste(input$var, "Per Capita"), x ="Years", y = paste(input$var, "per 100,000 people")) +
      theme(text = element_text(size = 15)) # font size
  })
  
  
  # Map1
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
    # create midrange function for labels
    midrange <- function(x) {
      z <- (max(x)+min(x))/2
      z
    }
    label_data = map.df %>% group_by(region) %>% summarise_at(vars(long, lat), midrange)
    label_data$region = state.abb[match(label_data$region,tolower(state.name))]
    ggplot(map.df, aes(x=long,y=lat,group=group)) +
      geom_polygon(aes(fill=y)) +
      geom_path() + 
      scale_fill_gradientn(colours=rev(heat.colors(10)), 
                           name = NULL,   # no label over colorbar
                           limits=c(0, max(crime_states_sub$y)), # same color bar for all years of each crime type
                           na.value="grey90") +
      geom_label(data = label_data,             # state labels
                 aes(x=long,y=lat,label= region, group=NULL),
                 label.size = 0,
                 size=4) +
      labs(title=paste(input$cr, "Per Capita in", input$yr), 
           x="", y="") + 
      theme(text = element_text(size = 15), # font size
            axis.ticks.x = element_blank(), # remove lat and long ticks and labels
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()) + 
      coord_map()
  })
  
  
  
  # Map2
  output$mapPlot2 <- renderPlot({
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
    if (input$crm == "All Violent Crimes"){
      crime_states_sub = crime_states[c("report_year", "crimes_percapita", "region")]
    } else if (input$crm == "Homicides"){
      crime_states_sub = crime_states[c("report_year", "homicides_percapita", "region")]
    } else if (input$crm == "Rapes"){
      crime_states_sub = crime_states[c("report_year", "rapes_percapita", "region")]
    } else if (input$crm == "Assaults"){
      crime_states_sub = crime_states[c("report_year", "assaults_percapita", "region")]
    } else if (input$crm == "Robberies"){
      crime_states_sub = crime_states[c("report_year", "robberies_percapita", "region")]
    }
    colnames(crime_states_sub) = c("report_year", "y", "region") # rename columns so i can use arbitrary y
    # get state dataset
    states <- map_data("state")
    # only chosen years
    crime_states_yr = crime_states_sub[(crime_states_sub$report_year == input$yrs[1]) | (crime_states_sub$report_year == input$yrs[2]), ]
    # drop state row with only one of the years
    n_occur = data.frame(table(crime_states_yr$region)) # make df of how many of each state (1 or 2)
    n_occur = n_occur[!(n_occur$Freq == 1),] # drop states with only 1
    crime_states_yr = crime_states_yr[(crime_states_yr$region %in% n_occur$Var1),]
    
    # make difference column in %
    crime_states_yr = crime_states_yr %>% group_by(region) %>% 
      mutate(diff =  100*(y[report_year == input$yrs[2]] - y[report_year == input$yrs[1]]) / y[report_year == input$yrs[2]])
    # merge my df with state df
    map.df <- merge(states,crime_states_yr, by="region", all.crime_states_yr=T)
    map.df <- map.df[order(map.df$order),]
    
    # plot
    # create midrange function for labels
    midrange <- function(x) {
      z <- (max(x)+min(x))/2
      z
    }
    label_data = map.df %>% group_by(region) %>% summarise_at(vars(long, lat), midrange)
    label_data$region = state.abb[match(label_data$region,tolower(state.name))]
    # ggplot plot
    maxcolor = max(c(100, max(crime_states_yr$diff), abs(min(crime_states_yr$diff))))
    ggplot(map.df, aes(x=long,y=lat,group=group)) +  # lat lon
      geom_polygon(aes(fill=diff)) +  # add colors to states
      geom_path() + 
      scale_fill_gradient2( low = "green",
                            mid = "white",
                            high = "red",
                           name = NULL,   # no label over colorbar
                           limits=c(-1*maxcolor, maxcolor), # same color bar for all years of each crime type
                           na.value="grey90") +
      geom_label(data = label_data,             # state labels
                 aes(x=long,y=lat,label= region, group=NULL),
                 label.size = 0,
                 size=4) +
      labs(title=paste("Percent Change in", input$crm, input$yrs[1], "-", input$yrs[2]), 
           x="", y="") + 
      theme(text = element_text(size = 15), # font size
            axis.ticks.x = element_blank(), # remove lat and long ticks and labels
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()) + 
      coord_map()
  })
  
  output$tab <- renderUI({
    h5("Red = There was an increase in crime between the chosen years.
       Green = There was a decrease in crime between the chosen years.")
  })
  
  
  
  
  # About Me
  
  
  linkedin_url = a("LinkedIn", href="https://linkedin.com/in/grainneroneill")
  github_link = a("GitHub", href="https://github.com/grainneroneill")
  blog_link = a("Blog", href="https://nycdatascience.com/blog/author/grainneroneill/")
  
  output$tab3 <- renderUI({
    tagList("Read my blog:", blog_link)
  })
  
  output$tab1 <- renderUI({
    tagList("Find me on LinkedIn:", linkedin_url)
  })
  output$tab2 <- renderUI({
    tagList("Check out my GitHub:", github_link)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

