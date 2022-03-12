#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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


#reading data
#all covid data, until today, for each country
Data_hub <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", header = TRUE, sep = ",")

covid_data <- Data_hub

lat_long = read.csv("lat-long.csv", header = TRUE, sep = ",")

#filtering for columns I want
covid_data <- select(covid_data, iso_code:new_cases_per_million)

#for data by country
covid_data_by_country <- covid_data

colnames(lat_long) <- c('code', 'lat', 'long', 'location')

covid_data_by_country <- merge(covid_data_by_country, lat_long, by='location', all=TRUE)

covid_data_by_country <- filter(covid_data_by_country, !is.na(lat))

covid_data_by_country <- filter(covid_data_by_country, new_cases_per_million > 0)

#filtering to get data by continent
covid_data <- filter(covid_data, location=="Africa" | location == "Europe" | location == "Asia" 
                     | location == "Oceania" | location == "South America" | 
                       location == "North America")


#data about government measures
#setwd('C:\\Users\\16032\\OneDrive - Grinnell College\\Desktop\\Year 3 Sem 2\\CSC324\\CSC324-repository\\covid_shiny')
govt_data<-read.csv("government_measures.csv", header = TRUE, sep = ",")

#formating govt_data
govt_data$date <- strptime(as.character(govt_data$DATE_IMPLEMENTED), "%m/%d/%Y")

govt_data$date <- format(govt_data$date, "%Y-%m-%d")

govt_data <-
  filter(govt_data, DATE_IMPLEMENTED!="")

govt_data <-
  govt_data %>%
  mutate(date=as.Date(DATE_IMPLEMENTED, "%m/%d/%Y"))

govt_data <-
  filter(govt_data, !is.na(DATE_IMPLEMENTED))

govt_data <-
  rename(govt_data, "iso_code"="ISO")

govt_data_phaseout <-
  filter(govt_data, LOG_TYPE != "Introduction / extension of measures")

govt_data <-
  filter(govt_data, LOG_TYPE == "Introduction / extension of measures")

#categories of govt measures
catego <- unique(govt_data$CATEGORY)

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Covid cases in Shiny using ggplot2"),
  #slider for date to display on world map
  sidebarLayout(
    sidebarPanel(
      sliderInput("DatesMerge",
                  "Dates:",
                  min = as.Date("2020-02-22","%Y-%m-%d"),
                  max = as.Date(Sys.Date(),"%Y-%m-%d"),
                  value=as.Date("2020-02-22"),
                  timeFormat="%Y-%m-%d"),
      selectInput("categ", "Which government measure would you like to be visualized?", catego)
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
                                       click = "plot_react_click"))
              ),
              #code for large unzoomable map click
              # fluidRow(
              #   column(width = 12,
              #          h4("Points near click"),
              #          verbatimTextOutput("click_info")
              #   )
              # ),
              fluidRow(
                column(width = 12,
                       h4("Points near click"),
                       verbatimTextOutput("click_info_react")
                )
              ),
              fluidRow(
                plotOutput("hist", height = 300)
                ),
              fluidRow(
                splitLayout(cellWidths = c("50%","50%"),
                            plotOutput("plot_country_cases"),
                            plotOutput("plot_country_deaths"))
              ),
              fluidRow(
                plotOutput("plot_country", height = 300))
    ),
  )
)

#add smooth plot for selected country?

server <- function(input, output, session) {
  
  ranges2 <- reactiveValues(x=NULL, y = NULL)
  
  output$plot <- renderPlot({
    
    #reading date
    day <- input$DatesMerge
    #day_formatted <- str_glue("{month(day)}-{day(day)}-{year(day)}")
    covid_on_day <- filter(covid_data_by_country, date==day)
    
    #world map
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
  
  output$plot_react <- renderPlot({
    
    #reading date
    day <- input$DatesMerge
    #day_formatted <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day <- filter(covid_data_by_country, date==day)
    
    #world map
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
  
  output$click_info_react <- renderPrint({
    # Adding clicking on second plot for info on country
    day <- input$DatesMerge
    #day_formatted <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day_click <- filter(covid_data_by_country, date==day)
    country_click <- nearPoints(covid_on_day_click, input$plot_react_click, maxpoints = 1, addDist = FALSE)$location
    #covid_on_day_click <- select(covid_on_day_click, c(2, 5, 8, 9, 12, 14, 15))
    covid_on_day_click <- filter(covid_on_day_click, location == country_click)
    #covid_on_day_click <- rename(covid_on_day_click, 'new_cases' = 'New Cases')
    covid_on_day_click <- select(covid_on_day_click, c(1, 5, 8, 9, 12))
    names(covid_on_day_click)[4] <- "New Deaths"
    names(covid_on_day_click)[5] <- "New Cases per Million"
    names(covid_on_day_click)[3] <- "Total Deaths"
    names(covid_on_day_click)[2] <- "Total Cases"
    names(covid_on_day_click)[1] <- "Location"
    covid_on_day_click
  })
  
  output$plot_country_cases <- renderPlot({
    #plot of total cases over time
    nearCountry <- nearPoints(covid_data_by_country, input$plot_react_click, maxpoints = 1, addDist = FALSE)
    iso_react <- as.character(nearCountry$iso_code)
    
    #visualizing the selected measure
    cat <- input$categ
    
    #adding lines for dates of lockdown
    govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)
    
    govt_data_lockdown <- filter(govt_data, CATEGORY==cat & iso_code==iso_react)
    
    govt_data_end_lockdown <- filter(govt_data_phaseout, CATEGORY==cat & iso_code==iso_react)
    
    govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)
    
    g <-
      ggplot(govt_data_2, aes(x=as.Date(date), y=total_cases, group=1)) +
      geom_line()
    
    list_dates <- list()
    
    for (obs in 1:nrow(govt_data_lockdown)){
      list_dates <- append(list_dates, as.character(govt_data_lockdown[obs,19]))
    }
    
    for (i in 1:length(list_dates)){
      g <- g + geom_vline(xintercept=as.Date(list_dates[[i]]), col = 'black')
    }
    
    list_dates_end <- list()
    
    for (obs in 1:nrow(govt_data_end_lockdown)){
      list_dates_end <- append(list_dates_end, as.character(govt_data_end_lockdown[obs,19]))
    }
    
    for (i in 1:length(list_dates_end)){
      g <- g + geom_vline(xintercept=as.Date(list_dates_end[[i]]), col = 'blue')
    }
    
    g <- g + 
      xlab("Date") +
      ylab("Total Cases") +
      scale_y_continuous(labels = comma_format(scale=1))
    
    g
    
  })
  
  output$plot_country_deaths <- renderPlot({
    #plot of total cases over time
    nearCountry <- nearPoints(covid_data_by_country, input$plot_react_click, maxpoints = 1, addDist = FALSE)
    iso_react <- as.character(nearCountry$iso_code)
    
    #visualizing the selected measure
    cat <- input$categ
    
    #adding lines for dates of lockdown
    govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)
    
    govt_data_lockdown <- filter(govt_data, CATEGORY==cat & iso_code==iso_react)
    
    govt_data_end_lockdown <- filter(govt_data_phaseout, CATEGORY==cat & iso_code==iso_react)
    
    govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)
    
    g <-
      ggplot(govt_data_2, aes(x=as.Date(date), y=total_deaths, group=1)) +
      geom_line()
    
    list_dates <- list()
    
    for (obs in 1:nrow(govt_data_lockdown)){
      list_dates <- append(list_dates, as.character(govt_data_lockdown[obs,19]))
    }
    
    for (i in 1:length(list_dates)){
      g <- g + geom_vline(xintercept=as.Date(list_dates[[i]]), col = 'black')
    }
    
    list_dates_end <- list()
    
    for (obs in 1:nrow(govt_data_end_lockdown)){
      list_dates_end <- append(list_dates_end, as.character(govt_data_end_lockdown[obs,19]))
    }
    
    for (i in 1:length(list_dates_end)){
      g <- g + geom_vline(xintercept=as.Date(list_dates_end[[i]]), col = 'blue')
    }
    
    g <- g + 
      xlab("Date") +
      ylab("Total Deaths") +
      scale_y_continuous(labels = comma_format(scale=1))
    
    g
    
  })
  
  observe({
    #working to make the brush on plot1, read its x and y coords, then change the plot on the right
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
    }
    else {
      ranges2$x <- NULL
      ranges2$y <-NULL
    }
  })
  
  output$hist = renderPlot({
    #histogram for data by continent
    #reading date
    day <- input$DatesMerge
    #day_formatted <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day <- filter(covid_data, date==day)
    
    g <- ggplot(data=covid_on_day, aes(location, new_cases_per_million))
    g +
      geom_bar(stat="identity") +
      ylab("New Cases per Million") +
      xlab("Continent")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
