#load các package cần thiết
library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(lubridate)

#Nhiệm vụ 1: Import dữ liệu
covid_cases <- readRDS("day2/data/covid_cases.rds")
#view(covid_cases)

#Chuyển dữ liệu thành tibble
#Dùng skim để chuyển đổi dữ liệu
#skim(covid_cases)

covid_cases %>% pivot_longer(cols = -date,
                             names_to = "country",
                             values_to = "cases") %>%
  as_tibble() %>% skimr::skim()

#solution: covid_cases %>% pivot_longer(cols = -date,names_to = "country",values_to = "cases") %>% as_tibble() %>% skimr::skim()

#solution: covid_cases %>% pivot_longer(cols = -date,
#                               names_to = "country",
#                                values_to = "cases") %>%
#                                         as_tibble() %>%
#                                       skimr::skim()

#incorrected_data <- covid_cases %>% filter(cases < 0)

 incorrect_data <- covid_cases %>%
                           pivot_longer(cols = -date,
                                        names_to = "country",
                                         values_to = "cases") %>%
                           filter(cases < 0)

#corrected_data <- covid_cases %>% filter(cases >= 0)  

 correct_data <- covid_cases %>%
               pivot_longer(cols = -date,
               names_to = "country",
               values_to = "cases") %>%
               anti_join(incorrect_data, by = c("date", "country", "cases"))



#corrected_data <- corrected_data %>% mutate(
#  date = ymd(date),              
#  week_num = week(date),         
#  year = year(date)              
# )

#covid_cases <- corrected_data %>% filter(week_num >= 3 & week_num <= 12 & year == 2020)

 covid_cases <- correct_data %>%
  mutate(country = str_remove(country, "cases_")) %>%
  filter(week(date) >= 3 & week(date) <= 12)



#Nhiệm vụ 3:

 top_countries <- covid_cases %>%
   group_by(country) %>%
   summarise(total_cases = sum(cases)) %>%
   slice_max(total_cases, n = 5) %>%
   pull(country)
 
 plot_data <- covid_cases %>%
   mutate(country = as.factor(ifelse(
     country %in% top_countries, country, "Others"
   ))) %>%
   group_by(date, country) %>%
   summarise(total_cases = sum(cases)) %>%
   mutate(
     total_cases_per_date = sum(total_cases),
     pct_cases = ifelse(
       total_cases_per_date == 0,
       NA,
       total_cases * 100 / total_cases_per_date
     ),
     week = week(date)
   ) %>%
   na.omit()
#covid_cases <- covid_cases %>% mutate(country = if_else(country %in% top_5_names, country, "Others")) 

#covid_cases$country <- as.factor(covid_cases$country)

#cases_by_date_country <- covid_cases %>% group_by(date, country) %>% summarise(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")


#Nhiệm vụ 4:

plot_data %>% 
  mutate(country = fct_relevel(country, "chn", "deu", "esp",
                               "ita", "usa", "Others")) %>% 
  ggplot(aes(x = date, y = pct_cases, fill = country)) +
  geom_area(color = "black", size = 0.5) +
  scale_x_date(date_labels = "W%U",
               date_breaks = "1 week",
               expand = c(0, 0)) +
  scale_y_continuous(
    labels = function(x)
      paste(x, "%"),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  scale_fill_discrete(
    name = "Country",
    breaks = c("chn", "deu", "esp", "ita", "usa", "Others"),
    labels = c("China", "Germany", "Spain", "Italy", "USA", "Others")
  ) +
  labs(title = "Percentage of COVID case counts per country for the first 10 weeks of 2020", x = "Date", y = "Percent of total cases") +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12, color = "black"))


