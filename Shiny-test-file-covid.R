library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
#reading data
#covid_data_2_csv <- read_excel("C:/Users/16032/OneDrive - Grinnell College/Desktop/Year 3 Sem 2/CSC324/data/covid-data-2.csv.xlsx")
#View(covid_data_2_csv)
covid_data = read.csv("covid-data-2.csv", header = TRUE, sep = ",")
#View(covid_data_2_csv)


##BY COUNTRY
covid_data_by_country <- covid_data
#View(lat_long)

covid_data_by_country <- select(covid_data_by_country, iso_code:new_cases_per_million)

#getting lat and long
covid_data_by_country <-
  covid_data_by_country %>%
  mutate(lat=if(lat_long$country==location){latitude})

colnames(lat_long) <- c('code', 'lat', 'long', 'location')

covid_data_by_country <- merge(covid_data_by_country, lat_long, by='location', all=TRUE)

covid_data_by_country <- filter(covid_data_by_country, !is.na(lat))

g <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) +
  geom_point(
    data = covid_data_by_country,
    aes(long, lat,
        size = new_cases_per_million),
    alpha = .7
  )

g

##END BY COUNTRY

#filtering by columns I want
covid_data <- select(covid_data, iso_code:new_cases_per_million)



#removes all the specific countries leaving just the aggregates over the continent
# covid_data <-
#   covid_data %>%
#   filter(is.na(continent))

covid_data <- filter(covid_data, location=="Africa" | location == "Europe" | location == "Asia" 
                     | location == "Oceania" | location == "South America" | 
                       location == "North America")

#generating latitude and longitude
covid_data <-
  covid_data %>%
  mutate(lat=ifelse(location=="Africa", -8.7,
                    ifelse(location=="North America", 47.1,
                           ifelse(location=="Europe", 54.5,
                                  ifelse(location=="Asia", 34.0,
                                         ifelse(location=="Oceania", -22.7, -8.7))))))

covid_data <-
  covid_data %>%
  mutate(long=ifelse(location=="Africa", 34.5,
                     ifelse(location=="North America", -101.3,
                            ifelse(location=="Europe", 25.2,
                                   ifelse(location=="Asia", 100.6,
                                          ifelse(location=="Oceania", 140.0, -55.5))))))
day <- as.Date("2021-02-22")
#test <- strftime(as.Date(day, format="%d/%m/%Y"), format="%m/%d/%Y")
strtest <- str_glue("{month(day)}/{day(day)}/{year(day)}")
# strtest <- as.character(test)

covid_by_day <- filter(covid_data, date == strtest)


#world map
world <- map_data("world")

day <- as.Date("2020-03-20","%Y-%m-%d")

g <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) +
  geom_point(
    data = covid_by_day,
    aes(long, lat,
        color = location,
        size = new_cases_per_million),
    alpha = .7
  )

g




#shows all cases on 2020/08/31
# covid_data_his <-
#   covid_data %>%
#     filter(date == as.Date("2020-08-31"))

# covid_data_his <-
#   covid_data %>%
#   filter(new_cases_per_million < 50)
# 
# covid_data_his <-
#   covid_data %>%
#   filter(new_cases_per_million > 0, new_cases_per_million < 50)
# 
# #shows all cases in a continent
# covid_data_his <-
#   covid_data %>%
#   filter(new_cases_per_million > 50)
# 
# #creating a histogram with covid_data_his
# ggplot(covid_data_his, aes(new_cases_per_million)) +
#   geom_histogram(binwidth=1)