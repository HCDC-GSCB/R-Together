#Gọi cac package
library(tidyverse) 
library(gtsummary) 
library(dplyr)

?tbl_summary

# data import
linelist <- read_rds("https://raw.githubusercontent.com/HCDC-GSCB/swaper/main/data/simulated_covid.rds") 
%>% mutate_if(is.Date, ymd) 
head(linelist)

# truoc dau phay la hang, au day phay la truy xuat cot, 
linelist[,-2] %>% tbl_summary() # lay tat ca cac hang, tru hang o 2
sum(is.na(linelist$date_admission))

# Tạo bảng với tb1_summary
linelist%>% 
  tbl_summary(
    include = c(age, sex, outcome, date_onset, date_admission, date_first_contact, 
                date_last_contact, outbreak), 
    missing="no", # không lấy các giá trị missing
    label = list( # nên dùng list đề giữ nguyên các định dạng của các biến
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
      all_continuous() ~ "{mean} ({sd})",
      sex ~ "{n} / {N} ({p}%)",
      outcome ~ "{n} / {N} ({p}%)",
      digits= age ~2
    ))


linelist%>% 
  tbl_summary(
    include = c(age, sex, outcome, date_onset, date_admission, date_first_contact, 
                date_last_contact, outbreak), 
    missing="no", # không lấy các giá trị missing
    label = list( # nên dùng list đề giữ nguyên các định dạng của các biến
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
      sex ~ "{n} / {N} ({p}%)",
      outcome ~ "{n} / {N} ({p}%)",
      all_continuous() ~ "{mean} [{min}, {max}]"),
    digits = age ~ 2
    )


linelist %>%
  tbl_summary(
    include = c(age, outbreak),
    label = age ~ "Tuổi",
    by = "outbreak"
  ) %>%
  add_p(test = age ~ "t.test")

# chi.square
# ----- Bảng kết quả cho đợt dịch đầu ---- 
tbl_first_outbreak <- linelist %>%
  mutate(
    sex = factor(sex, levels = c("f", "m"), labels = c("nữ", "nam")),
    outcome = factor(outcome, 
                     levels = c("died", "recovered"), 
                     labels = c("tử vong", "khỏi bệnh"))
  ) %>% 
  filter(outbreak == "1st outbreak") %>%
  tbl_summary(
    include = c(sex, outcome), 
    label = outcome ~ "Kết quả",
    by = sex) %>%
  # điều chỉnh header để các bảng có chung header
  modify_header(all_stat_cols() ~ "**{level}**") %>% 
  add_p(test = outcome ~ "chisq.test")

tbl_first_outbreak
shapiro.test(linelist$age)
?add_p
