df <- read.table(file ="day1/data/life-exp.csv", header = T, sep = ",")
head(df)

library(readr)
library(readxl)
df1 <-read_excel("day1/data/vaccine_data.xlsx", sheet = 1, col_names = T)
head(df1)
view(df1)

library(haven)
df2 <-read_dta("day1/data/linelist-raw.dta")

max(x = df1$id, na.rm = FALSE)


install.packages('janitor')
library(janitor)
df1 <-clean_names(df1)
head(df1)
View(df1)

class(df1$gioitinh)
df1$gioitinh <- as.factor(df1$gioitinh)
class(df1$gioitinh)

class(df1$ngaysinh)
df1$ngaysinh <- as.Date(df1$ngaysinh)
class(df1$ngaysinh)

head(x = df)
max(x=df$Year,na.rm =TRUE)

df7 <- read.table(file ="day1/data/life-exp.csv", header = T, sep = ",")
head(df)


#23/05/2025
df <- read_xlsx("day1/data/vaccine_data.xlsx", sheet = 1, col_names = T)   
