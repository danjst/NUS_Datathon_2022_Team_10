library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

static_data <- read.csv("../data/eicu_static.csv")
dynamic_data <- read.csv("../data/eicu_temp_dynamic.csv")

dynamic_filtered <- dynamic_data %>%
  group_by(patientunitstayid) %>%
  filter(hours <= 48,temperature<47,temperature>11) %>%
  mutate(max_temp=max(temperature), min_temp=min(temperature), temp_gap=max_temp-min_temp, median_temp=median(temperature))

dynamic_filtered <- dynamic_filtered %>%
  group_by(patientunitstayid) %>%
  mutate(patient_id=!duplicated(patientunitstayid)) %>%
  filter(patient_id == TRUE)

dynamic_filtered <- dynamic_filtered[-c(2,3,8)]
  
merged_data <- left_join(dynamic_filtered, static_data, by="patientunitstayid")

merged_data <- merged_data %>%
  mutate(age=as.character(age))

merged_data[which(merged_data$age=="> 89"),"age"]="90"

merged_data$age <- as.numeric(merged_data$age)

merged_data <- merged_data[!(merged_data$hosp_death=="" | merged_data$icu_death==""), ]

ggplot(merged_data) +
  geom_point(aes(x=patientunitstayid, y=median_temp))

readr::write_csv(merged_data, 'merged_data_eicu.csv')