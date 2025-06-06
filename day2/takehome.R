#load các package cần thiết
library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(lubridate)

#Nhiệm vụ 1: Import dữ liệu
covid_cases <- readRDS("day2/data/covid_cases.rds")

view(covid_cases)

#Chuyển dữ liệu thành tibble
covid_cases <- tibble(covid_cases)
print(covid_cases)

#Dùng skim để chuyển đổi dữ liệu
skim(covid_cases)

covid_cases <- covid_cases %>% pivot_longer(
  cols = starts_with("cases_"),
  names_to = "country",
  names_prefix = "cases_",
  values_to = "cases"
)

incorrected_data <- covid_cases %>% filter(cases < 0)
corrected_data <- covid_cases %>% filter(cases >= 0)  

corrected_data <- corrected_data %>% mutate(
  date = ymd(date),              
  week_num = week(date),         
  year = year(date)              
)

covid_cases <- corrected_data %>% filter(week_num >= 3 & week_num <= 12 & year == 2020)

