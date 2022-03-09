library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RCurl)
library(curl)



Data<-read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", header = TRUE, sep = ",")


# y <- list("Africa", 'Asia', 'Australia', 'Europe', 'North America', 'South America')

##BY COUNTRY
#covid_data = read.csv("covid-data-2.csv", header = TRUE, sep = ",")
covid_data<-Data
covid_data_by_country <- covid_data
#View(lat_long)

covid_data_by_country <- select(covid_data_by_country, iso_code:new_cases_per_million)

#getting lat and long
# covid_data_by_country <-
#   covid_data_by_country %>%
#   mutate(lat=if(lat_long$country==location){latitude})

colnames(lat_long) <- c('code', 'lat', 'long', 'location')

covid_data_by_country <- merge(covid_data_by_country, lat_long, by='location', all=TRUE)

covid_data_by_country <- filter(covid_data_by_country, !is.na(lat))

covid_data_by_country <- filter(covid_data_by_country, new_cases_per_million > 0)

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
  ) +
  scale_size_area()

g

##END BY COUNTRY


#filtering by columns I want
covid_data <- select(covid_data, iso_code:new_cases_per_million)

#creating a scattermatrix:
# Plot
#showing cases and deaths by continent
# matrix_data <- filter(covid_data, location=="World")
# matrix_data <- matrix_data[,c(3,5,6,8,9)]
# pairs(matrix_data[,2:5], pch = 19)



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

day <- as.Date("2021-05-22")
#test <- strftime(as.Date(day, format="%d/%m/%Y"), format="%m/%d/%Y")
strtest <- str_glue("{month(day)}/{day(day)}/{year(day)}")
# strtest <- as.character(test)

covid_by_day <- filter(covid_data, date == strtest)

p <- ggplot(data=covid_by_day, aes(location, new_cases_per_million)) +
  geom_bar(stat="identity")

p

#working on govt measurements
#setwd('C:\\Users\\16032\\OneDrive - Grinnell College\\Desktop\\Year 3 Sem 2\\CSC324\\CSC324-repository\\covid_shiny')
govt_data<-read.csv("government_measures.csv", header = TRUE, sep = ",")

govt_data <-
  filter(govt_data, DATE_IMPLEMENTED!="")

govt_data <-
  govt_data %>%
  mutate(date=as.Date(DATE_IMPLEMENTED, "%m/%d/%Y"))

govt_data <-
  filter(govt_data, !is.na(DATE_IMPLEMENTED))

govt_data <-
  filter(govt_data, LOG_TYPE == "Introduction / extension of measures")

AFG_data <-
  filter(govt_data, ISO=="AFG")

CHN_data <-
  filter(govt_data, ISO=="CHN")

afg_measures <- ggplot(data=AFG_data, aes(date, CATEGORY, color=MEASURE)) +
  geom_point(size=2)

afg_measures

chn_measures <- ggplot(data=CHN_data, aes(date, CATEGORY, color=MEASURE)) +
  geom_point(size=2)

chn_measures +
  theme(legend.position="top")

govt_data <-
  rename(govt_data, "iso_code"="ISO")

iso_react <- "GBR"
govt_data_2 <- filter(covid_data_by_country, iso_code==iso_react)

govt_data_lockdown <- filter(govt_data, CATEGORY=="Lockdown" & ISO==iso_react)

g <-
  ggplot(govt_data_2, aes(x=date, y=total_deaths, group=1)) +
  geom_line()
plot(g) +
  for (obs in govt_data_lockdown){
  abline(v=obs[13], col = "blue")
}

g <-
  ggplot(govt_data_2, aes(x=date, y=total_cases, group=1)) +
  geom_line()
g



#messing around with color
# d=data.frame(c=colors(), y=seq(0, length(colors())-1)%%66, x=seq(0, length(colors())-1)%/%66)
# ggplot() +
#   scale_x_continuous(name="", breaks=NULL, expand=c(0, 0)) +
#   scale_y_continuous(name="", breaks=NULL, expand=c(0, 0)) +
#   scale_fill_identity() +
#   geom_rect(data=d, mapping=aes(xmin=x, xmax=x+1, ymin=y, ymax=y+1), fill="white") +
#   geom_rect(data=d, mapping=aes(xmin=x+0.05, xmax=x+0.95, ymin=y+0.5, ymax=y+1, fill=c)) +
#   geom_text(data=d, mapping=aes(x=x+0.5, y=y+0.5, label=c), colour="black", hjust=0.5, vjust=1, size=3)
# 
# 
# d=expand.grid(h=seq(0,0.95,0.05), s=seq(0,0.95,0.05), v=seq(0,1,0.2))
# ggplot() +
#   coord_polar(theta="x") +
#   facet_wrap(~v) +
#   scale_x_continuous(name="hue", limits=c(0,1), breaks=seq(0.025,0.925,0.1), labels=seq(0,0.9,0.1)) +
#   scale_y_continuous(name="saturation", breaks=seq(0, 1, 0.2)) +
#   scale_fill_identity() +
#   geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h), ymin=s, ymax=s+resolution(s), fill=hsv(h,s,v)), color="white", size=0.1)

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