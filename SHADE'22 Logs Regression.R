library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

static_data <- read.csv("../data/merged_data_cleaned.csv")
static_data <- static_data %>% 
  mutate(gender = case_when(
    str_detect(gender, "M") ~ 1,
    str_detect(gender, "F") ~ 0
  ))
static_data$proportion_within_range <- static_data$proportion_within_range*100
log.model <- glm(hospital_expire_flag ~ proportion_within_range + admission_age + gender + charlson_score + apsiii, data=static_data, family='binomial')
summary(log.model)
