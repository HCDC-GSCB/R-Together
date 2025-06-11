# ====== R Scipt for take home exercise solution === 
<<<<<<< HEAD
#doc du lieu
covid_cases <- readRDS("~/GitHub/day1/data/covid_cases.rds")
head(covid_cases)
#Ngay bao cao du lieu
##Som nhat
first_report_date <- min(covid_cases$date, na.rm = T)
head(first_report_date)                    
##Muon nhat
Last_report_date <- max(covid_cases$date, na.rm = T)
head(Last_report_date)
#Tao cot moi dai dien tong so truong hop tren moi quoc gia moi ngay bao cao
library(dplyr)
covid_cases <- covid_cases %>%
  mutate(case_global = rowSums(select(., starts_with("cases_")), na.rm = TRUE))
head(covid_cases)
#Tao cot moi dai dien cho ty le phan tram TrungQuoc
covid_cases <- covid_cases %>%
  mutate(percent_chn = 100*cases_chn/case_global)
##Tao mot Function
compute_percent <- function(data, country_code) {
  # Tạo tên cột quốc gia
  country_col <- paste0("cases_", country_code)
  
  # Kiểm tra cột có tồn tại không
  if (!(country_col %in% names(data))) {
    stop(paste("Cột", country_col, "không tồn tại trong dữ liệu."))
  }
  
  if (!("case_global" %in% names(data))) {
    stop("Cột case_global chưa có trong dữ liệu.")
  }
  
  # Tính phần trăm
  percent <- 100 * data[[country_col]] / data$case_global
  return(percent)
}
#su dung function tao 3 cot moi
library(dplyr)
covid_cases <- covid_cases %>%
  mutate(
    percent_chn = compute_percent(., "chn"),
    percent_vnm = compute_percent(., "vnm"),
    percent_usa = compute_percent(., "usa"),
    percent_sgp = compute_percent(., "sgp")
  )
#in bang chi chon cac cot
covid_cases %>%
  select(date, percent_chn, percent_vnm, percent_usa, percent_sgp) %>%
  print()
#Tạo bảng tóm tắt dữ liệu
library(skimr)
skim(covid_cases[,c("percent_chn", "percent_vnm", "percent_usa", "percent_sgp")])
##Mô tả
# + Tất cả đều bị missing 1 ca
# + Tại Trung Quốc, tỷ lệ trung bình ca cao 38.6% độ lệch chuẩn cao =>  => quốc gia ghi nhận số ca lớn, dữ liệu biến động mạnh, không ổn định
=======

#Hi
>>>>>>> 687fa488083afa8f56b6410ea02e6542f4220a96
