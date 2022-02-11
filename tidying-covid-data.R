library(tidyverse)
library(readxl)
#reading data
covid_data_2_csv <- read_excel("C:/Users/16032/OneDrive - Grinnell College/Desktop/Year 3 Sem 2/CSC324/data/covid-data-2.csv.xlsx")
View(covid_data_2_csv)


#filtering by columns I want
covid_data <- select(covid_data_2_csv, iso_code:new_cases_per_million)

#making a plot of cases over time for everything (messy)
ggplot(data=covid_data) + 
  geom_point(mapping = aes(x = date, y = total_cases))


#removes all the specific countries leaving just the aggregates over the continent
covid_data <-
  covid_data %>%
    filter(is.na(continent))

my_list <- list("Africa", "Asia", "Europe", "Oceania", "South America", "North America")

#creating a histogram for lab 2
ggplot(covid_data, aes(new_cases_per_million)) +
  geom_histogram()

#a list of all the cotinents
my_list <- list("Africa", "Asia", "Europe", "Oceania", "South America", "North America")

covid_data <- filter(covid_data, location=="Africa" | location == "Europe" | location == "Asia" | location == "Oceania" | location == "South America" | location == "North America")

covid_data_big <- filter(covid_data, new_cases_per_million > 50)

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