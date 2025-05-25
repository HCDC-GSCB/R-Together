#Gọi file CSV
getwd()
df1 <- read.table(file = "C:/Users/ASUS/Documents/day1/data/life-exp.csv",header=T, sep=",") 
#nếu F thì lấy tên cột là v1, v1....
# nếu T thì lấy tên cột là chính nó
head (df)
#gọi file XLSX
library(readxl) #excel
library (haven) #stata

df2<-read_excel(path = "day1/data/vaccine_data.xlsx",
           sheet=1)
#goi file stata
df3<- read_dta (file = "day1/data/linelist-raw.dta")

max (x=df2$id, na.rm=TRUE)
max (x=df2$ngaysinh, na.rm=TRUE) #TRUE bỏ các giá trị bị thiếu

#lam sach du lieu
library(janitor)
## tên cột
df <- df %>% 
  clean_names() %>% # Tự động làm sạch tên cột
  rename( 
    vgb_truoc_24 = vgb_24, # Tên mới = Tên cũ
    vgb_sau_24 = vgb_24_2
  )
view(df2)
## Đổi tên cột
class(df2$gioitinh)
df2$gioitinh <-as.factor(df2$gioitinh)

class(df2$ngaysinh)
df2$ngaysinh<- as.Date (df2$ngaysinh)


## kiểu dữ liệu 
## kiểm tra giá trị


