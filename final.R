                ## CUỐI KHÓA CAPSTONE

   ## Phần 1: Đọc dữ liệu
library("tidyverse")
library("skimr")
library("lubridate")
library("janitor")

setwd("C:/R-Together")
getwd()
library(readr)
df <- read_csv("C:/R-together/day1/data/life-exp.csv")

     ## Phần 2: Khám phá Dữ liệu
skimr::skim(df)
df <- clean_names(df)
str(df)

    ## Phần 3: Xử lý và phân tích dữ liệu
df_diseases <- df %>% select(measles,polio,diphtheria)
head(df_diseases)

df[2,5]

df[c(2,5), c("measles","polio")]

df <- df %>% 
  mutate(life_expectancy = ifelse (
    is.na(life_expectancy),
    mean(life_expectancy,na.rm = TRUE),
    life_expectancy
  ))
sum(is.na(df$life_expectancy))

#5 quốc gia có tuổi thọ trung bình cao nhất năm 2015 và gán thành top_5.
top_5 <- df %>% 
  filter(year==2015) %>% 
  slice_max(order_by = life_expectancy,n=5)

#5 quốc gia có tuổi thọ trung bình thấp nhất năm 2015 và gán thành bot_5.
bot_5 <- df %>% 
  filter(year==2015) %>% 
  slice_min(order_by = life_expectancy,n=5)

#Tạo 1 data frame chứa 2 đối tượng top_5 và bot_5 và gán thành df_top_bot
df_top_bot <- bind_rows(top_5,bot_5)

#Tạo biến mới bmi_lv là biến thứ tự gồm các nhóm: dưới 18.5, từ 18.5 đến 23, trên 23.
df <- df %>% 
  mutate(bmi = case_when(
          bmi<18.5 ~ "Duoi 18.5",
          bmi>=18.5 & bmi <23 ~ "Tu 18.5 den 23",
          bmi >=23 ~ "Tren 23",
          TRUE ~ NA_character_)
          )

#Sử dụng df nhóm theo country, tính trung bình GDP và gán thành df_gdp_by_country.
df_gdp_by_country <- df %>% 
  group_by(country) %>% 
  summarise(mean_gdp = mean(gdp, na.rm = TRUE))

    ## Phần 4: Tạo bảng thống kê năm 2015
#Các biến: Status, Life Expectancy, Adult Mortality, GDP, Schooling, BMI.
library(tidyverse) 
library(gtsummary) 
library(labelled)

  df_2015 <- df %>%
  filter(year == 2025) %>%
  select(status, life_expectancy, adult_mortality, gdp, schooling, bmi) %>%
  na.omit() 

  tbl_2015 <- df_2015 %>% 
    tbl_summary(
      by = status,
      label = list (
        status ~ "Trạng thái",
        life_expectancy ~ "Tuổi thọ trung bình",
        adult_mortality ~ "Tỷ lệ tử vong ở người lớn",
        gdp ~ "Tổng sản phẩm quốc nội (GDP)",
        schooling ~ "Số năm đi học",
        bmi ~ "Chỉ số khối cơ thể (BMI)"
      ),
      statistic = list(
        bmi ~ "{median} ({p25} - {p75})",
        all_continuous() ~ "{mean} ({min} - {max})"
      ),
      digits = bmi ~3
    ) %>% 
    add_overall() %>% 
    add_p(test = all_continuous() ~ "t.test", 
          pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
    bold_labels() %>%
    modify_caption("**Bảng 1. Thống kê mô tả theo Tình trạng phát triển (Status), năm 2025**")



