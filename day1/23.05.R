#23/05/2025: Data management, data cleaning and R style

sum(is.na(df))

#1.missing
install.packages("skimr") 

library(skimr)
skim(df)

#2.1 lam sach ten cot
library(readxl)
df <-read_excel("day1/data/vaccine_data.xlsx", sheet = 1)
head(df)

install.packages('janitor')
library(janitor)
df <- clean_names(df)
head(df)

#2.2 đoi ten 
library(dplyr)
df <- df%>% 
rename(vgb_truoc_24 = vgb_24, 
       vgb_sau_24 =vgb_24_2
)

#3 doi kieu dư lieu
library(lubridate)
str(df) #kiem tra 
date_cols <-c("ngaysinh", "vgb_truoc_24","vgb_sau_24","vgb_1","vgb_2","vgb_3","vgb_4","hg_1","hg_2","hg_3","hg_4","uv_1","uv_2","uv_3","uv_4")

df <- df %>% 
    mutate( # Each column
    gioitinh = as.factor("gioitinh"),
    tinhtrang = ifelse(tinhtrang == "theo dõi", TRUE, FALSE)
  ) %>% 
  mutate_at( # Multiple column
    date_cols, 
    dmy
  ) %>% 
  rename(
    theodoi = tinhtrang
  )

str(df)

#4 Loc du lieu
df %>% filter(huyen == "Quận 2") %>% select(id, huyen)

df %>% filter(vgb_1 < as.Date("2024-02-20")) %>% select(id, vgb_1)

df %>% filter(huyen == "Quận 2" | vgb_1 < as.Date("2024-02-20")) %>% select(id, huyen, vgb_1)

df %>% filter(huyen == "Quận 2" & vgb_1 < as.Date("2024-02-20")) %>% select(id, huyen, vgb_1)

#5 thay the gia tri NA cua vgb_24 thanh ngay hien tai
library(tidyr)
library(lubridate)
df <- df %>% 
  mutate(vgb_truoc_24 = replace_na(vgb_truoc_24,today()))

table(df$vgb_truoc_24==today())

#6 Function de tinh cho cac quoc gia khac

covid_cases <- readRDS("~/GitHub/R-Together/day1/data/covid_cases.rds")

compute_stats <- function(data, colname) {
  return(
    c(
      min = min(data[[colname]]),
      q1 = quantile(data[[colname]], 0.25, names=FALSE),
      median = median(data[[colname]]),
      q3 = quantile(data[[colname]], 0.75, names=FALSE),
      max = max(data[[colname]])
    )
  )
}
 compute_stats(covid_cases, "cases_jpn") 
 compute_stats(covid_cases, "cases_vnm") 
 
head(df)

