library(tidyverse)
library(gtsummary)
linelist <- read_rds("https://raw.githubusercontent.com/HCDC-GSCB/swaper/main/data/simulated_covid.rds") %>% mutate_if(is.Date, ymd)
linelist[,-2] %>% tbl_summary()
sum(is.na(linelist$date_admission))
#chon cac bien dươc hien thi tren bang thong ke
linelist %>% tbl_summary(
  include = c(age, sex, outcome, date_onset, date_admission,
              date_first_contact, date_last_contact, outbreak),
  missing = "no",
  # ----- Thay đổi cách hiển thị tên biến ------
  label = list(
    age ~ "Tuổi",
    sex ~ "Giới tính",
    outcome ~ "Kết quả",
    date_onset ~ "Ngày phát bệnh",
    date_admission ~ "Ngày nhập viện",
    date_first_contact ~ "Ngày tiếp xúc đầu tiên",
    date_last_contact ~ "Ngày tiếp xúc cuối cùng"
  ),
  by = outbreak,
  statistic = list(
    sex ~ "{n}/{N} ({p}%)",
    outcome ~ "{n}/{N} ({p}%)",
    all_continuous() ~ "{mean} [{min}, {max}]"
  ),
  digits = age ~ 2)
)
linelist$sex <- ifelse(linelist$sex == "f", "Nu", "Nam")
linelist$outcome <- ifelse(linelist$outcome == "died", "Chet", "Hoi phuc")                         
#Kiem dinh T
linelist %>% 
  mutate(
    outbreak = factor(outbreak, 
                      levels = c("1st outbreak", "2nd outbreak"), 
                      labels = c("Đợt 1", "Đợt 2"))
  ) %>% 
  tbl_summary(
    include = c(age, outbreak),
    label = age ~ "Tuổi",
    by = outbreak
  ) %>% 
  modify_header(label = "**Đặc điểm**")  %>% 
  add_p(test = age ~ "t.test")
shapiro.test(linelist$age)
hist(linelist$age)
