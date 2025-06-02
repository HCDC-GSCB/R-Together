df <- read.table(file = "data/life-exp.csv", header =  F, sep = ",")
head(df)
## read libary file

library(readxl)
df1 <- read_excel(path = "data/vaccine_data.xlsx", sheet = 1,col_names = T)
head(df1)
?read_excel()

library(haven)
df <- read_dta(file = "data/linelist-raw.dta")
head(df)
max(x = df1$ngaysinh, na.rm = T)
max(x = df1$`UV_4+`, na.rm = F)

library(janitor)
df1 <- clean_names(df1)
head(df1)
View(df1)
class(df1$gioitinh)
df1$gioitinh <- as.factor(df1$gioitinh)
class(df1$ngaysinh)
library(readxl)
#Nhap data
library(readxl)
df <- read_excel(path = "data/vaccine_data.xlsx", sheet = 1)
head(df)               
df <- rea_excel(path = "data/vaccine_data.xlsx", sheet = 1)
#missing
sum(is.na(df))
skimr::skim(df)
#lam sach ten cot
library(janitor)
df <- clean_names(df)
head(df)
#doi ten
library(dplyr)
df <- df %>% 
rename(vgb_truoc_24 = vgb_24,
       vgb_sau_24 = vgb_24_2)
#doi kieu du lieu
str(df)
library(lubridate)
dlngaythang <- c("ngaysinh", "vgb_truoc_24","vgb_sau_24","vgb_1","vgb_2","vgb_3","vgb_4","hg_1","hg_2","hg_3","hg_4","uv_1","uv_2","uv_3","uv_4")
df <- df %>% 
  mutate( # Each column
    gioitinh = as.factor(gioitinh),
    tinhtrang = ifelse(tinhtrang == "theo dõi", TRUE, FALSE)
  ) %>% 
  mutate_at( # Multiple column
    dlngaythang, 
    dmy
  ) %>% 
  rename(
    theodoi = tinhtrang
  )
#loc du lieu
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
