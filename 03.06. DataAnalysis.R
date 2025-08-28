#Data Analysis 03/06
library(tidyverse)
install.packages("gtsummary") 
library(gtsummary) 

# Tạo bảng pttk
linelist <- read_rds("https://raw.githubusercontent.com/HCDC-GSCB/swaper/main/data/simulated_covid.rds") %>% mutate_if(is.Date, ymd) 
linelist[,-2] %>% tbl_summary()
sum(is.na(linelist$date_admission))
sum(is.na(linelist$date_first_contact))

#Điều chỉnh bảng thống kê
  ## Dùng label
linelist %>% 
  tbl_summary(include = c(age,sex,outcome,date_onset,date_admission,date_first_contact,date_last_contact,outbreak),
  ##Loại bỏ giá trị Unknown
              missing ="no",
  ##Đổi tên biến
              label = list(
                age ~ "Tuổi",
                sex ~ "Giới tính",
                outcome ~ "Kết quả",
                date_onset ~ "Ngày phát bệnh",
                date_admission ~ "Ngày nhập viện",
                date_first_contact ~ "Ngày tiếp xúc đầu tiên",
                date_last_contact ~ "Ngày tiếp xúc cuối cùng"
              ),
              
  ## Thống kê theo nhóm qua tham số by
              by = "outbreak",
  ##Đổi cách trình bày
              statistic = list(
                                sex ~ "{n} / {N} ({p}%)",
                                outcome ~ "{n} / {N} ({p}%)",
                                all_continuous() ~ "{mean} [{min}, {max}]"
                                
              ),
  digits = age~2
            )
  ## ĐỔi giá trị của biến
linelist$sex <- ifelse(linelist$sex =="f","Nữ","Nam")

  ## Kết hợp các cột

# KIểm định Chi bình phương
linelist %>% 
  mutate(
    outbreak = factor(outbreak,
                      levels = c("1st outbreak","2nd outbreak"),
                      labels = c("Đợt 1","Đợt 2"))) %>% 
  tbl_summary(
       include = c(age, outbreak),
       by = outbreak,
       label = age ~ "Tuổi"
     ) %>% 
  add_p(test = age ~ t.test)

  hist(linelist$age)
  shapiro.test(linelist$age)
  
  