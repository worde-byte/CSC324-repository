#Authors: Eamon Worden
#Assignment: Individual Project
#Due date: 3/14/2022
#Online Sources:
#https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
#https://campus.datacamp.com/courses/free-introduction-to-r/chapter-6-lists?ex=6
#https://mastering-shiny.org/ (lots of help for all of shiny)
#https://stackoverflow.com/questions/40908808/how-to-sliderinput-for-dates
#https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/#change-legend-title
#data from: https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv
#government measures data from https://github.com/OxCGRT/covid-policy-tracker
#https://statisticsglobe.com/change-formatting-of-numbers-of-ggplot2-plot-axis-in-r
#https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
#https://bookdown.org/dli/rguide/scatterplots-and-best-fit-lines-two-sets.html
#Help obtained: Prof Jimenez
#I confirm that the above list of sources is complete AND that I/we have not talked to anyone else about the solution to this problem


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinydashboard)
library(shinythemes)
library(RCurl)
library(curl)
library(maps)
library(scales)


#Begin preprocessing of data
#Reading all data up until today (by reading from the github file)
Data_hub <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", header = TRUE, sep = ",")

covid_data <- Data_hub
#reading latitude and longitude of each country
lat_long = read.csv("lat-long.csv", header = TRUE, sep = ",")

#filtering for columns I want and use in my project
covid_data <- select(covid_data, iso_code:new_cases_per_million, people_fully_vaccinated)

#for data by country, rather than continent
covid_data_by_country <- covid_data

#renaming columns
colnames(lat_long) <- c('code', 'lat', 'long', 'location')

#merging covid and latitude/longitude data sets
covid_data_by_country <- merge(covid_data_by_country, lat_long, by='location', all=TRUE)

#keeping only country which have a lat and longitude
covid_data_by_country <- filter(covid_data_by_country, !is.na(lat))

#removing weird points which record negative number of new cases per million
covid_data_by_country <- filter(covid_data_by_country, new_cases_per_million > 0)

#filtering to get data by continent
covid_data <- filter(covid_data, location=="Africa" | location == "Europe" | location == "Asia" 
                     | location == "Oceania" | location == "South America" | 
                       location == "North America")


#data about government measures
#setwd('C:\\Users\\16032\\OneDrive - Grinnell College\\Desktop\\Year 3 Sem 2\\CSC324\\CSC324-repository\\covid_shiny')
govt_data<-read.csv("government_measures.csv", header = TRUE, sep = ",")

#formating government measures data
#formatting time
govt_data$date <- strptime(as.character(govt_data$DATE_IMPLEMENTED), "%m/%d/%Y")

govt_data$date <- format(govt_data$date, "%Y-%m-%d")

#keeping only entries which have a date implemented
govt_data <-
  filter(govt_data, DATE_IMPLEMENTED!="")

govt_data <-
  govt_data %>%
  mutate(date=as.Date(DATE_IMPLEMENTED, "%m/%d/%Y"))

govt_data <-
  filter(govt_data, !is.na(DATE_IMPLEMENTED))

#renaming data
govt_data <-
  rename(govt_data, "iso_code"="ISO")

#seperating phase in and phase out of measures
govt_data_phaseout <-
  filter(govt_data, LOG_TYPE != "Introduction / extension of measures")

govt_data <-
  filter(govt_data, LOG_TYPE == "Introduction / extension of measures")

#categories of govt measures
catego <- unique(govt_data$MEASURE)
#end of data preprocessing

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Visualizing Covid with Shiny"),
  #slider for date to display on world map
  sidebarLayout(
    sidebarPanel(
      sliderInput("DatesMerge",
                  "Dates:",
                  min = as.Date("2020-02-22","%Y-%m-%d"),
                  max = as.Date(Sys.Date(),"%Y-%m-%d"),
                  value=as.Date("2020-02-22"),
                  timeFormat="%Y-%m-%d"),     #adding a sidebar with all the dates from the first entry in the covid data set until today
      selectInput("categ", "Which government measure would you like to be visualized?", catego) #letting users choose government measure they want visualized
    ), 
    
    # Show a plot of the covid cases in the world
    mainPanel("World Covid Map",
              fluidRow(
                splitLayout(cellWidths = c("50%","50%"),
                            plotOutput("plot",
                                       click = "plot1_click",
                                       brush = brushOpts(
                                         id = "plot1_brush")),
                            plotOutput("plot_react",
                                       click = "plot_react_click"))     #adding maps of world, one which is the world and the other which zooms in and can be clicked on
              ),
              fluidRow(
                column(width = 12,
                       h4("Points near click"),
                       verbatimTextOutput("click_info_react")
                )         #adding information about country clicked on on given day
              ),
              fluidRow(
                splitLayout(cellWidths = c("50%","50%"),
                            plotOutput("plot_country_cases"),
                            plotOutput("plot_country_deaths"))
              ),               #adding graphs of covid cases over time with government measures being phased in and out overlayed
              h5("A black line indicates a new measure and a blue line indicates a measure being phased out."),
              fluidRow(
                plotOutput("hist", height = 300)
                ),        #adding visualization of how each continent is doing with a histogram of cases over time
    ),
  )
)

#add smooth plot for selected country?

server <- function(input, output, session) {
  
  ranges2 <- reactiveValues(x=NULL, y = NULL)   #brush starts off null
  
  #unreactive plot of world (left) with each country mapped, which can be brushed to zoom in the reactive plot
  output$plot <- renderPlot({
    
    #reading date and filtering data for that day
    day <- input$DatesMerge
    covid_on_day <- filter(covid_data_by_country, date==day)
    
    #world map with each country given a different color point on that map
    world <- map_data("world")
    g <- ggplot()
    g + geom_map(
      data = world, map = world,
      aes(long, lat, map_id = region)
    ) + 
      geom_point(
        data = covid_on_day,
        aes(long, lat,
            color = location,
            size = new_cases_per_million),
        #alpha = .7
      ) +
      guides(color = "none") +
      scale_size_area() +
      xlab("Latitude") +
      ylab("Longitude") +
      labs(title = "New Covid Cases per Million in World", size = "New Cases per Million")
    
  })
  
  #reactive plot (right) which zooms in on brushed area of plot on left, and can be clicked on to view information about a given country
  output$plot_react <- renderPlot({
    
    #reading date
    day <- input$DatesMerge
    covid_on_day <- filter(covid_data_by_country, date==day)
    
    #world map for reactive data (uses xlim, ylim)
    world <- map_data("world")
    g <- ggplot()
    g + geom_map(
      data = world, map = world,
      aes(long, lat, map_id = region)
    ) + 
      geom_point(
        data = covid_on_day,
        aes(long, lat,
            color = location,
            size = new_cases_per_million),
        #alpha = .7
      ) +
      guides(color = "none") +
      coord_cartesian(xlim=ranges2$x, ylim = ranges2$y, expand = FALSE) +
      scale_size_area() +
      xlab("Latitude") +
      ylab("Longitude") +
      labs(title = "Reactive Graph of New Covid Cases per Million in World", size="New Cases per Million")
    
  })
  
  #displays text output with data for selected country (or china if no country has been selected)
  output$click_info_react <- renderPrint({
    #Reading data
    day <- input$DatesMerge
    #filtering by date
    covid_on_day_click <- filter(covid_data_by_country, date==day)
    
    if (!is.null(input$plot_react_click)){
      country_click <- nearPoints(covid_on_day_click, input$plot_react_click, threshold = 1000, maxpoints = 1, addDist = FALSE)$location
    }
    else {
      country_click <- "China"
    }
    covid_on_day_click <- filter(covid_on_day_click, location == country_click)
    covid_on_day_click <- select(covid_on_day_click, c(1, 5, 8, 9, 12, 13))
    names(covid_on_day_click)[4] <- "New Deaths"
    names(covid_on_day_click)[5] <- "New Cases per Million"
    names(covid_on_day_click)[3] <- "Total Deaths"
    names(covid_on_day_click)[2] <- "Total Cases"
    names(covid_on_day_click)[1] <- "Location"
    names(covid_on_day_click)[6] <- "People Fully Vaccinated"
    covid_on_day_click
  })
  
  #plotting the total number of cases overtime overlayed with government measures
  output$plot_country_cases <- renderPlot({
    #getting country from click
    if (!is.null(input$plot_react_click)){
      nearCountry <- nearPoints(covid_data_by_country, input$plot_react_click, threshold = 1000, maxpoints = 1, addDist = FALSE)
      iso_react <- as.character(nearCountry$iso_code)
      country_cases <- as.character(nearCountry$location)
    }
    else {
      iso_react <- "CHN"
      country_cases <- "China"
    } #default is china if nothing is clicked
    
    #reading category of government lockdown
    cat <- input$categ
    
    #adding lines for dates of lockdown
    #filtering data to match selected country
    govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)
    #phase in data
    govt_data_lockdown <- filter(govt_data, MEASURE==cat & iso_code==iso_react)
    #phase out data
    govt_data_end_lockdown <- filter(govt_data_phaseout, MEASURE==cat & iso_code==iso_react)
    #getting data for country over tie
    govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)
    #graph of total cases over tie
    g <-
      ggplot(govt_data_2, aes(x=as.Date(date), y=total_cases, group=1)) +
      geom_line()
    #adding vertical lines for measures
    list_dates <- list()
    #adding phase in to list
    for (obs in 1:nrow(govt_data_lockdown)){
      list_dates <- append(list_dates, as.character(govt_data_lockdown[obs,19]))
    }
    #adding black vertical lines for measures being phased in
    for (i in 1:length(list_dates)){
      g <- g + geom_vline(xintercept=as.Date(list_dates[[i]]), col = 'black')
    }
    #adding vertical lines for measures being phased out
    list_dates_end <- list()
    #adding phase outs to list
    for (obs in 1:nrow(govt_data_end_lockdown)){
      list_dates_end <- append(list_dates_end, as.character(govt_data_end_lockdown[obs,19]))
    }
    #adding phase outs as blue vertical lines
    for (i in 1:length(list_dates_end)){
      g <- g + geom_vline(xintercept=as.Date(list_dates_end[[i]]), col = 'blue')
    }
    #adding labels to graph
    g <- g + 
      xlab("Date") +
      ylab("Total Cases") +
      scale_y_continuous(labels = comma_format(scale=1))
    #graph and adding title
    g + ggtitle(paste("Total Cases from Covid-19 in", country_cases))
    
  })
  #plotting the total number of cases overtime overlayed with government measures
  output$plot_country_deaths <- renderPlot({
    #reading clicked on country
    if (!is.null(input$plot_react_click)){
      nearCountry <- nearPoints(covid_data_by_country, input$plot_react_click, threshold = 1000, maxpoints = 1, addDist = FALSE)
      iso_react <- as.character(nearCountry$iso_code)
      country_deaths <- as.character(nearCountry$location)
    }
    else {
      iso_react <- "CHN"
      country_deaths <- "China"
    } #default is china
    
    
    #reading selected measure
    cat <- input$categ
    
    #filtering by selected country
    govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)
    #phase in measures
    govt_data_lockdown <- filter(govt_data, MEASURE==cat & iso_code==iso_react)
    #phase out measures
    govt_data_end_lockdown <- filter(govt_data_phaseout, MEASURE==cat & iso_code==iso_react)
    #filtering by country
    govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)
    #graph of cases over time
    g <-
      ggplot(govt_data_2, aes(x=as.Date(date), y=total_deaths, group=1)) +
      geom_line()
    #adding phase ins to list
    list_dates <- list()
    
    for (obs in 1:nrow(govt_data_lockdown)){
      list_dates <- append(list_dates, as.character(govt_data_lockdown[obs,19]))
    }
    #adding phase ins as black vertical line
    for (i in 1:length(list_dates)){
      g <- g + geom_vline(xintercept=as.Date(list_dates[[i]]), col = 'black')
    }
    #adding phase outs to list
    list_dates_end <- list()
    
    for (obs in 1:nrow(govt_data_end_lockdown)){
      list_dates_end <- append(list_dates_end, as.character(govt_data_end_lockdown[obs,19]))
    }
    #addings phase outs as blue vertical line
    for (i in 1:length(list_dates_end)){
      g <- g + geom_vline(xintercept=as.Date(list_dates_end[[i]]), col = 'blue')
    }
    #adding axis labels
    g <- g + 
      xlab("Date") +
      ylab("Total Deaths") +
      scale_y_continuous(labels = comma_format(scale=1))
    #adding title
    g + ggtitle(paste("Total Deaths from Covid-19 in", country_deaths))
  })
  #working to make the brush on plot1, read its x and y coords, then change the plot on the right
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
    }#outputting coords of brush
    else {
      ranges2$x <- NULL
      ranges2$y <-NULL
    }
  })
  #adding histogram of continents new cases per million on selected day
  output$hist = renderPlot({
    #reading date
    day <- input$DatesMerge
    #filtering by day
    covid_on_day <- filter(covid_data, date==day)
    #making histogram of cases on selected day
    g <- ggplot(data=covid_on_day, aes(location, new_cases_per_million))
    g +
      geom_bar(stat="identity") +
      ylab("New Cases per Million") +
      xlab("Continent")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
