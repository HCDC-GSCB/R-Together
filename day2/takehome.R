# ====== R Scipt for take home exercise day 2 === 

# Nhiệm vụ 1 - data import - package call  ###########
covid_cases <- readRDS("E:/R-Together/day1/data/covid_cases.rds")
df <- readRDS("E:/R-Together/day1/data/covid_cases.rds")

library(tidyverse)

# Nhiệm vụ 2 ###########
head(covid_cases)
skimr::skim(covid_cases)

# xoay trục dữ liệu
covid_cases <-  covid_cases %>% 
  pivot_longer(
  cols = starts_with("cases_"),
  names_to = "country",
  values_to = "case"
)
# xem dữ liệu
skimr::skim(covid_cases)
# có số case âm -> không hợp lý

# Dữ liệu sai
incorrect_data <- covid_cases %>% filter(case < 0)
# Dữ liệu đúng
correct_data <- covid_cases %>% filter(case >= 0)
# gán vào covid_case
covid_cases<- covid_cases %>% filter(case >= 0)

# Lọc dữ liệu theo ngày
covid_cases <- covid_cases %>%
  filter(year(date) == 2020,              # Năm 2020
         isoweek(date) >= 3,              # Tuần từ 3
         isoweek(date) <= 12) %>%            # Đến tuần 12
  mutate(
    week = isoweek(date)
  )
 
# Nhiệm vụ 3  ###########
# Nhóm dữ liệu và tính tổng số ca bệnh theo mỗi quốc gia.

# Tạo top 5 quốc gia
top_countries <- covid_cases %>%
  group_by(country) %>%
  summarise(total_cases = sum(case, na.rm = TRUE))%>%
  slice_max(order_by = total_cases, n = 5)%>%
  # Lấy ra cột country_code thành vector
  pull(country)

# tạo data tổng hợp
covid_cases_pct <- covid_cases %>%
  # 1. Chuyển tất cả các country không nằm trong top 5 thành "Others"
  mutate(country = if_else(country %in% top_countries, country, "Others")) %>%
  
  # 2. Biến country thành factor (với thứ tự: 5 tên top rồi tới "Others")
  mutate(country = factor(country, levels = c(top_countries, "Others"))) %>%
  
  # 3. Nhóm theo date + country, tính tổng ca/ngày cho mỗi nhóm
  group_by(date, country) %>%
  summarise(daily_cases = sum(case, na.rm = TRUE), .groups = "drop") %>%
  
  # 4. Trong mỗi ngày, tính pct_cases = 100 * daily_cases / tổng ca của ngày đó
  group_by(date) %>%
  mutate(pct_cases = daily_cases / sum(daily_cases, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  
  # 5. Loại bỏ các dòng có NA (nếu có)
  drop_na()

# Kiểm tra kết quả
covid_cases_pct

# Nhiệm vụ 4


  

