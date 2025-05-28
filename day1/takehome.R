# ====== R Scipt for take home exercise solution === 
# Hi friend
# Nhiem vu 1: Nhap du lieu
covid_cases <- readRDS("day1/data/covid_cases.rds")

# Nhiem vu 2: Tính toán đơn giản
library(janitor)
library(tidyverse)
library(skimr)
library(readxl)
library(haven)
library(lubridate)
library(dplyr)


covid_cases <- tibble(covid_cases)

first_report_date <- min(covid_cases$date, na.rm = TRUE)
  
last_report_date <- max(covid_cases$date, na.rm = TRUE)

covid_cases <- covid_cases %>% 
  mutate(across(2:ncol(covid_cases), as.numeric))

str(covid_cases)

covid_cases$case_global <- rowSums(covid_cases[,-1])

covid_cases[["percent_chn"]] <- (covid_cases$cases_chn/covid_cases$case_global)*100


##Nhiệm vụ 3: Tạo một function

compute_percent <- function(x, data){
  cases_name <- paste("cases_", x, sep = "")
  percent_cal <- (data[[cases_name]]/data$case_global)*100
  
  percent_name <- paste("percent_", x, sep = "")
  data[[percent_name]] <- percent_cal
  return(data[[percent_name]])
}

compute_percent("vnm", covid_cases)
compute_percent("usa", covid_cases)
compute_percent("sgp", covid_cases)




