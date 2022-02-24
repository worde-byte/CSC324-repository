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



ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Covid cases in Shiny using ggplot2"),
  #slider for date to display on world map
  sidebarLayout(
    sidebarPanel(
      sliderInput("DatesMerge",
                  "Dates:",
                  min = as.Date("2020-02-22","%Y-%m-%d"),
                  max = as.Date("2022-01-31","%Y-%m-%d"),
                  value=as.Date("2020-02-22"),
                  timeFormat="%Y-%m-%d")
    ),
    
    # Show a plot of the covid cases in the world
    mainPanel("Covid Map",
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
                column(width = 6,
                       plotOutput("hist", height = 300)
                )
              )
    ),
  )
)

#add smooth plot for selected country?

server <- function(input, output, session) {
  #reading data
  covid_data = read.csv("covid-data-2.csv", header = TRUE, sep = ",")
  
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
  
  #generating latitude and longitude for continents
  covid_data <-
    covid_data %>%
    mutate(lat=ifelse(location=="Africa", 5.7,
                      ifelse(location=="North America", 47.1,
                             ifelse(location=="Europe", 54.5,
                                    ifelse(location=="Asia", 34.0,
                                           ifelse(location=="Oceania", -22.7, -8.7))))))
  
  covid_data <-
    covid_data %>%
    mutate(long=ifelse(location=="Africa", 26.17,
                       ifelse(location=="North America", -101.3,
                              ifelse(location=="Europe", 25.2,
                                     ifelse(location=="Asia", 100.6,
                                            ifelse(location=="Oceania", 140.0, -55.5))))))
  
  ranges2 <- reactiveValues(x=NULL, y = NULL)
  
  output$plot <- renderPlot({
    
    #reading date
    day <- input$DatesMerge
    day_formatted <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day <- filter(covid_data_by_country, date==day_formatted)
    
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
      scale_size_area()
    
  })
  
  output$plot_react <- renderPlot({
    
    #reading date
    day <- input$DatesMerge
    day_formatted <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day <- filter(covid_data_by_country, date==day_formatted)
    
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
      scale_size_area()
    
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    day <- input$DatesMerge
    day_formatted <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day_click <- filter(covid_data_by_country, date==day_formatted)
    covid_on_day_click <- select(covid_on_day_click, c(2, 4, 5, 8, 9, 12, 14, 15))
    nearPoints(covid_on_day_click, input$plot1_click, addDist = FALSE)
  })
  
  output$click_info_react <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    day <- input$DatesMerge
    day_formatted <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day_click <- filter(covid_data_by_country, date==day_formatted)
    covid_on_day_click <- select(covid_on_day_click, c(2, 4, 5, 8, 9, 12, 14, 15))
    nearPoints(covid_on_day_click, input$plot_react_click, addDist = FALSE)
  })
  
  observe({
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
    
    #reading date
    day <- input$DatesMerge
    day_formatted <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day <- filter(covid_data, date==day_formatted)
    
    g <- ggplot(data=covid_on_day, aes(location, new_cases_per_million))
    g +
      geom_bar(stat="identity")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
