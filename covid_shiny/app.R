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




ui <- fluidPage(
  titlePanel("Covid cases in Shiny using ggplot2"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("DatesMerge",
                  "Dates:",
                  min = as.Date("2020-02-22","%Y-%m-%d"),
                  max = as.Date("2022-01-31","%Y-%m-%d"),
                  value=as.Date("2020-02-22"),
                  timeFormat="%Y-%m-%d")
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      tabsetPanel(
        tabPanel("Covid cases",plotOutput("plot")),
      )
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    
    DatesMerge<-input$DatesMerge
    
    #reading data
    covid_data = read.csv("covid-data-2.csv", header = TRUE, sep = ",")
    #View(covid_data_2_csv)
    
    
    #filtering by columns I want
    covid_data <- select(covid_data, iso_code:new_cases_per_million)
    
    #removes all the specific countries leaving just the aggregates over the continent
    # covid_data <-
    #   covid_data %>%
    #   filter(is.na(continent))
    
    covid_data_by_country <- covid_data
    
    covid_data <- filter(covid_data, location=="Africa" | location == "Europe" | location == "Asia" 
                         | location == "Oceania" | location == "South America" | 
                           location == "North America")
    
    #generating latitude and longitude
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
    #reading date
    day <- input$DatesMerge
    day_formatted <- strtest <- str_glue("{month(day)}/{day(day)}/{year(day)}")
    covid_on_day <- filter(covid_data, date==day_formatted)
    
    #desired_day <- as.character(input$DatesMerge)
    #str_replace_all(desired_day, "-", "\")
    #covid_on_day <- filter(covid_data, date==DatesMerge")
    
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
        alpha = .7
      )
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
