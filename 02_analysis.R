library(needs)
library(openxlsx)
library(dplyr)
library(writexl)
library(readxl)
library(flextable)
needs(here, tidyverse, foreign, haven, openxlsx, GGally, broom, ggeffects, ggpubr, ggrepel, gridExtra, gt, gratia)

# 分析
# (a) 記述統計-------------------------------------------------------
# 1.
data <- read_excel("~/Documents/01  OSIPP/99  その他/RA Boot Camp/03  課題/submit/all_combined_data.xlsx")

na_counts <- sapply(data, function(col) sum(is.na(col)))
print(na_counts)

# 2.


# 3.
data$total_graduate_4yr <- as.numeric(data$total_graduate_4yr)

average_by_year <- data %>%
  group_by(year) %>%
  summarise(mean_graduate_4yr = mean(total_graduate_4yr, na.rm = TRUE))

figure1 <- ggplot(data = average_by_year) +　
  geom_line(aes(x = year, y = mean_graduate_4yr)) +
  labs(title = "Four-Year Graduation Rates", 
       x = "Year",
       y = "4-Year Graduation Rate") + 
  scale_y_continuous(limits = c(0.25, 0.45)) + 
  theme_minimal() 
  
print(figure1)


# 4.
data_with_dummy <- data %>%
  group_by(instnm) %>%
  mutate(dummy = if_else(any(semester == 1), 1, 0)) %>%
  ungroup()

semester_rate <- data_with_dummy %>%
  group_by(year) %>%
  summarise(
    total = n(),                          
    dummy_count = sum(dummy == 1),       
    proportion = dummy_count / total)

figure2 <- ggplot(data = semester_rate) +　
  geom_line(aes(x = year, y = proportion)) +
  labs(title = "Fraction of Schools on Semesters", 
       x = "Year",
       y = "Fraction of Schools on Semesters") + 
  scale_y_continuous(limits = c(0.8, 1)) + 
  theme_minimal() 

print(figure2)

5. 


# (b) 回帰分析-------------------------------------------------------

model1 <- lm(total_graduate_4yr~semester,data = data_with_dummy)

summary(model1)
