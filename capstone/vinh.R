# Gọi packages
library(tidyverse)
library(janitor)
library(gtsummary)
df <- read_csv("E:/R-Together/day1/data/life-exp.csv")
skimr::skim(df)

# Phần 2: Khám phá Dữ liệu #########
# Dữ liệu có bao nhiêu dòng và bao nhiêu cột?
  # Number of rows             1649  
  # Number of columns          22  

# Dữ liệu gồm những loại biến nào?
  #Column type frequency:           
  # character                2     
  # numeric                  20  
  
#  Dữ liệu có cột nào có missing không? Nếu có thì gồm những cột nào?
  # Không có missing

#  Tên các cột có đúng quy tắc chưa? Nếu chưa thì đổi tên cột theo đúng quy tắc.
  # QUy tắc 
  #Tên ngắn
  # Không có khoảng trắng (thay thế bằng dấu gạch dưới_)
  # Không có ký tự lạ (&, #, <, >, …)
  # Thống nhất cách định danh
names(df)
df<- clean_names(df)
names(df)

# Kiểu dữ liệu của mỗi cột có phù hợp chưa? Nếu chưa thì đổi thành kiểu dữ liệu phù hợp với biến.
df <- df %>%
  pivot_longer(
    cols = starts_with("thinness"),
    names_to = "age_group",
    values_to = "thinness"
  )

# Phần 3: Xử lý và phân tích dữ liệu #########
 
 # Sử dụng df truy xuất ra những cột Measles, Polio, Diphtheria và gán thành df_diseases. Sau đó in kết quả những dòng đầu tiên.
df_diseases <- df %>% select(measles, polio, diphtheria)


  # Sử dụng df truy xuất hàng 2, cột 5.
a <- df[2, 5]
a

  #Sử dụng df truy nhất hàng 2,5 và cột Measles, Polio.
b <- df[c(2, 5), c("measles", "polio")]
b
  # Thay thế những giá trị missing của cột Life Expectancy bằng giá trị trung bình của những giá trị

df <- df %>%
  mutate(life_expectancy = ifelse(
    is.na(life_expectancy),
    mean(life_expectancy, na.rm = TRUE),
    life_expectancy
  ))

table(is.na(df$life_expectancy))

# 5 quốc gia có tuổi thọ trung bình cao nhất năm 2015
top_5 <- df %>%
  filter(year == 2015) %>%
  distinct(country, .keep_all = TRUE) %>%   # Loại trùng lặp quốc gia
  slice_max(order_by = life_expectancy, n = 5, with_ties = T)

# 5 quốc gia có tuổi thọ trung bình thấp nhất năm 2015
bot_5 <- df %>%
  filter(year == 2015) %>%
  distinct(country, .keep_all = TRUE) %>%
  slice_min(order_by = life_expectancy, n = 5, with_ties = FALSE)

  # Tạo 1 data frame chứa 2 đối tượng top_5 và bot_5 và gán thành df_top_bot
df_top_bot <- bind_rows(top_5, bot_5)

  # Tạo biến mới bmi_lv là biến thứ tự gồm các nhóm: dưới 18.5, từ 18.5 đến 23, trên 23.
df <- df %>% mutate(bmi= case_when(
  bmi < 18.5 ~ "Dưới 18.5",
  bmi >= 18.5 & bmi <= 23 ~ "Từ 18.5 đến 23",
  bmi > 23 ~ "Trên 23",
  TRUE ~ NA_character_    # Nếu bị thiếu dữ liệu
)
)
  # Sử dụng df nhóm theo country để tính trung bình GDP và gán thành df_gdp_by_country.
df_gdp_by_country <- df %>%
  group_by(country) %>%
  summarise(mean_gdp = mean(gdp, na.rm = TRUE))


# Phần 4: Tạo bảng thống kê năm 2015 ########

df <- read_csv("E:/R-Together/day1/data/life-exp.csv")

df<- clean_names(df) %>%
  mutate(
    bmi = as.numeric(bmi),
    life_expectancy = as.numeric(life_expectancy),
    adult_mortality = as.numeric(adult_mortality),
    gdp = as.numeric(gdp),
    schooling = as.numeric(schooling)
  )
# Tạo bảng thống kê mô tả
tbl <- df %>%
  select(status, life_expectancy, adult_mortality, gdp, schooling, bmi) %>%
  tbl_summary(
    by = status,
    label = list(
      life_expectancy ~ "**Tuổi thọ**",
      adult_mortality ~ "**Tử vong người lớn**",
      gdp ~ "**GDP**",
      schooling ~ "**Đi học**",
      bmi ~ "**BMI**"
    ),
    missing = "no",
    statistic = c(
      all_continuous() ~ "{mean} [{min}-{max}]",
      bmi ~ "{median} ({p25}-{p75})"
    )) %>%  
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  bold_labels() %>%
  modify_caption("**Bảng mô tả các đặc trưng theo Status**")

tbl



# Phần 5: Trực quan hóa ###########
# Chuẩn bị data
plot5 <- df %>%
  select(country, life_expectancy, year) %>%
  filter(country %in% c("Tunisia", "Thailand", "Cambodia", "Turkey")) %>%
  group_by(country, year)

# Ploting
ggplot(plot5, aes(x = year, y = life_expectancy, color = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    limits = c(2000, 2015),             # Giới hạn trục x từ 2000 đến 2015
    breaks = seq(2000, 2015, by = 3)    # Nhãn năm cách nhau mỗi 3 năm
  ) +
  labs(
    title = "Xu hướng tuổi thọ trung bình theo năm",
    x = "Năm",
    y = "Tuổi thọ (năm)",
    color = "Quốc gia"
  ) +
  theme_minimal(base_size = 14)

# 5.2
df_sub <- df %>%
  filter(country %in% c("Tunisia", "Thailand", "Cambodia", "Turkey")) %>%
  filter(gdp > 0)  # Lọc bỏ GDP bằng 0 để dùng log10

ggplot(df_sub, aes(
  x = gdp, 
  y = life_expectancy, 
  color = status, 
  size = population
)) +
  geom_point(alpha = 0.8) +
  scale_x_log10(labels = scales::dollar_format()) +  # Trục x log10, đơn vị tiền
  scale_size_continuous(name = "Dân số (người)", labels = label_number(scale = 1e-6, suffix = " triệu")) +
  labs(
    title = "Mối liên hệ giữa GDP và tuổi thọ trung bình (4 quốc gia)",
    x = "GDP (USD)",
    y = "Tuổi thọ (năm)",
    color = "Tình trạng phát triển",
    size = "Dân số"
  ) +
  facet_wrap(~ country) +   # Tạo nhiều biểu đồ nhỏ theo từng quốc gia
  theme_minimal(base_size = 14)