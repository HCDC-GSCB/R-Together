# ====== R Scipt for take home exercise day 2 === 

library(gtsummary)
library(tidyverse)
getwd()

df <- readRDS("../day1/data/covid_cases.rds")

df1<- pivot_longer(df, cols= -date, names_to = "country", values_to = "cases") 

skimr::skim(df1)

incorrect_data <- filter(df1, cases <0)

correct_data <- filter(df1, cases >=0)

library(lubridate)

covid_cases <- correct_data %>% 
  mutate(country= str_remove(country, "cases_")) %>% 
  filter(week(date)>=3 & week(date) <=12) 
  
top_countries <- covid_cases %>%  
  group_by(country) %>% summarise(Total = sum(cases)) %>% 
  slice_max(Total,n= 5) %>% pull(country)


a <- covid_cases %>% 
     mutate(country= ifelse(country %in% top_countries, country, "Others")) %>% 
  group_by(date, country) %>% summarise(Total = sum(cases)) %>%
  
  mutate(sum_total= sum(Total),
         pct_cases = ifelse(sum_total==0, NA ,Total*100/sum_total),
         week = week(date)) %>% 
  na.omit()

a$pct_cases <- round(a$pct_cases,1)

p <- ggplot(a, aes(x= week, y =pct_cases, fill= country)) +
  geom_area()+
  scale_x_continuous(labels = function(x) paste("W",x), breaks = seq(3,12,1)) +
  scale_y_continuous(labels = function(x)
    paste(x,"%"), breaks = seq(0,100,10))

a <- ggplot(a, aes(x= date, y =pct_cases, fill= country)) +
 geom_area() +
   scale_x_date(date_labels = "W%U", date_breaks = "1 week") +
   scale_y_continuous(labels = function(x)
     paste(x,"%"), breaks = seq(0,100,10))

a
