library(needs)
library(openxlsx)
library(dplyr)
library(writexl)
needs(here, tidyverse, foreign, haven, openxlsx, GGally, broom, ggeffects, ggpubr, ggrepel, gridExtra, gt, gratia)

# データ整理と変換
# (a) Semester Dataの整形-------------------------------------------------------

# 1.
dataset1 <- read.csv("~/Documents/01  OSIPP/99  その他/RA Boot Camp/03  課題/warmup training package/01_data/raw/semester_dummy/semester_data_1.csv", header=TRUE)

dataset2 <- read.csv("~/Documents/01  OSIPP/99  その他/RA Boot Camp/03  課題/warmup training package/01_data/raw/semester_dummy/semester_data_2.csv", header=FALSE)

# 2.
new_colnames <- as.character(dataset1[1, ])
dataset1 <- dataset1[-1, ]
colnames(dataset1) <- new_colnames

colnames(dataset2) <- new_colnames
dataset2 <- dataset2[-1, ]

# 3.
dataset_combined <- rbind(dataset1, dataset2)

# 4.
dataset_combined <- dataset_combined[, !colnames(dataset_combined) %in% "Y"]


# (b) Gradrate Dataの整形-------------------------------------------------------

# 1.
files <- list.files("Documents/01  OSIPP/99  その他/RA Boot Camp/03  課題/warmup training package/01_data/raw/outcome/", full.names = TRUE)

data_list <- lapply(files, function(file) {
  tryCatch({
    read_excel(file)
  }, error = function(e) {
    message(paste("Error reading file:", file))
    NULL
  })
})

data_list <- Filter(Negate(is.null), data_list)

combined_graddata <- do.call(rbind, data_list)

# 2. 
combined_graddata$women_gradrate_4yr <- combined_graddata$women_gradrate_4yr * 0.01

# 3.
# total
combined_graddata$tot4yrgrads <- as.numeric(as.character(combined_graddata$tot4yrgrads))
combined_graddata$totcohortsize <- as.numeric(as.character(combined_graddata$totcohortsize))

combined_graddata$total_graduate_4yr <- combined_graddata$tot4yrgrads / combined_graddata$totcohortsize

# men
combined_graddata$m_4yrgrads <- as.numeric(as.character(combined_graddata$m_4yrgrads))
combined_graddata$m_cohortsize <- as.numeric(as.character(combined_graddata$m_cohortsize))

combined_graddata$men_graduate_4yr <- combined_graddata$m_4yrgrads / combined_graddata$m_cohortsize 

# 4.
combined_graddata$total_graduate_4yr <- sprintf("%.3g", combined_graddata$total_graduate_4yr)
combined_graddata$men_graduate_4yr <- sprintf("%.3g", combined_graddata$men_graduate_4yr)

# 5.
graddata_filtered <- subset(combined_graddata, year >= 1991 & year <= 2010)


# (c) Covariates Dataの整形-------------------------------------------------------

# 1.
covdata <- read_excel("~/Documents/01  OSIPP/99  その他/RA Boot Camp/03  課題/warmup training package/01_data/raw/covariates/covariates.xlsx")

# 2.
covdata <- covdata %>%
  rename(unitid = university_id)

# 3.
covdata$unitid <- gsub("aaaa", "", covdata$unitid)

# 4.
covdata_wide <- covdata %>%
  pivot_wider(names_from = category, values_from = value)

# 5.
covdata_wide_filtered <- subset(covdata_wide, year >= 1991 & year <= 2010)

# 6.
common_unitids <- intersect(graddata_filtered$unitid, covdata_wide_filtered$unitid)

graddata_filtered <- graddata_filtered %>%
  filter(unitid %in% common_unitids)

covdata_wide_filtered <- covdata_wide_filtered %>%
  filter(unitid %in% common_unitids)

# (d) Master Dataの作成-------------------------------------------------------

dataset_combined$year <- as.numeric(dataset_combined$year)
graddata_filtered$year <- as.numeric(graddata_filtered$year)
covdata_wide_filtered$year <- as.numeric(covdata_wide_filtered$year)

combined_with_grads <- dataset_combined %>%
  left_join(graddata_filtered, by = c("unitid", "year"))

all_combined_data <- combined_with_grads %>%
  left_join(covdata_wide_filtered, by = c("unitid", "year"))

# （データセットの保存）
write_xlsx(all_combined_data, path = "~/Documents/01  OSIPP/99  その他/RA Boot Camp/03  課題/all_combined_data.xlsx")



