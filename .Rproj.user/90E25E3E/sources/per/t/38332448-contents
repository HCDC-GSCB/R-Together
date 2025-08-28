# Take home exercise - Day 2

#Nhiệm vụ 1: Nhập dữ liệu
covid_cases <- readRDS("C:/R-Together/day1/data/covid_cases.rds")
library(tidyverse)

#Nhiệm vụ 2: Làm sạch và lọc dữ liệu
  ## Quy tắc tidy data
covid_cases <- tibble(covid_cases)
  ## Dùng pivot_longer
covid_cases <- covid_cases %>% 
  pivot_longer(
    cols = starts_with("cases_"),
    names_to = "country",
    names_prefix = "cases_",
    values_to = "cases"
    )
as_tibble() %>% 
## DÙng skimr
library("skimr")
skim(covid_cases)

  ## Tạo bảng incorrect_data
incorrect_data <- covid_cases %>% filter(cases<0)

  ## Loại bỏ dòng sai gắn vào bảng correct_data
correct_data <- covid_cases %>% filter(!cases <0)

  ## Lọc dữ liệu để chúng ta chỉ có tuần 3-12 của năm 2020 và gán thành covid_cases.
library(lubridate)
covid_cases$date  <- as.Date(covid_cases$date)
covid_cases <- covid_cases %>% 
  mutate(
    year = year(date),
    week = isoweek(date)
  ) %>% 
  filter(year == 2020, week >= 3, week <= 12)
range(covid_cases$date)

#Nhiệm vụ 3: Chuyển đổi dữ liệu
  ## Top_countries
top_countries <- covid_cases %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>% 
  slice_max(order_by = total_cases, n = 5) %>% 
  pull(country)
  
hc_data <- covid_cases %>% 
  mutate(country = as.factor(ifelse(country %in% top_countries, country, "Others" ))) %>%
  group_by(date,country) %>% 
  summarise (total_cases = sum(cases),na.rm = TRUE) %>% 
  mutate(
    total_cases_per_date = sum (total_cases),
    pct_cases = ifelse(total_cases_per_date ==0, NA,total_cases/total_cases_per_date*100
                       ))

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

plot_data %>%
  mutate(country = fct_relevel(country, "chn", "deu", "esp",
                               "ita", "usa", "Others")) %>% 
  

