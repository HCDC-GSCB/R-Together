# ====== R Scipt for take home exercise solution === 

#Exercise 1: 24/05/2025 - 30/05/2025
getwd()

#Nhiệm vụ 1: Nhập dữ liệu
covid_cases <- readRDS("~/GitHub/R-Together/day1/data/covid_cases.rds")

#Nhiệm vụ 2: Tính toán đơn giản
  #Ngày báo cáo dữ liệu
str(covid_cases)
first_report_date <- min(covid_cases$date,na.rm=T)
last_report_date <- max(covid_cases$date,na.rm=T)

  #Tạo cột mới case_global
library(tidyr)
library(dplyr)
  covid_cases <- covid_cases %>%
  mutate(case_global = rowSums(select(., starts_with("cases_")), na.rm = TRUE))
  head(covid_cases)  

  #Tạo cột mới percent_chn
library(dplyr)
covid_cases <- covid_cases %>%
  mutate(
    percent_chn = ifelse(case_global == 0, NA, 100 * cases_chn / case_global)
  )

#Nhiệm vụ 3: Tạo một function
 #Tạo một function compute_percent mà function đó
    compute_percent <- function(data,country_code) {
    country_col <- paste0("cases_", country_code)
    percent <- ifelse(data$case_global == 0, NA,
                      100 * data[[country_col]] / data$case_global)
    return(percent)
  }
  covid_cases$percent_myt <- compute_percent(covid_cases, "myt")
  covid_cases$percent_chn <- compute_percent(covid_cases, "chn")
  
 #Sử dụng function đó để tạo 3 cột mới cho bảng covid_cases:vnm,usa,sgp
  covid_cases$percent_vnm <- compute_percent(covid_cases, "vnm")
  covid_cases$percent_usa <- compute_percent(covid_cases, "usa")
  covid_cases$percent_sgp <- compute_percent(covid_cases, "sgp")
  
 #In bảng covid_cases cuối cùng, chỉ chọn các cột
  library(dplyr)
  covid_cases %>% 
    select(date,percent_chn, percent_vnm, percent_usa, percent_sgp) %>% 
    print()
#Nhiệm vụ 4: Tạo bảng tóm tắt dữ liệu
  library(skimr)
  skim(covid_cases[,c("percent_chn", "percent_vnm", "percent_usa", "percent_sgp")])
  ##Mô tả
  # + Tất cả đều bị missing 1 ca
  # + Tại Trung Quốc, tỷ lệ trung bình ca cao 38.6% độ lệch chuẩn cao =>  => quốc gia ghi nhận số ca lớn, dữ liệu biến động mạnh, không ổn định
