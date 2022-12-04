library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

dynamic_data <- read.csv("../data/dynamic.csv")
static_data <- read.csv("../data/static.csv")
merged_data <- read.csv("../data/merged_data_cleaned.csv")

a <- select(static_data, subject_id, tbi, cns_infect)
id <- merged_data$subject_id
los_hosp <- merged_data$los_hospital
los_icu <- merged_data$los_icu
age <- merged_data$admission_age
sex <- merged_data$gender
severity_score <- merged_data$apsiii
mortality_icu <- merged_data$icu_death
mortality_hosp <- merged_data$hospital_expire_flag

merged_data <- left_join(merged_data, a, by="subject_id")

comorbidsa <- merged_data[35:47]
comorbidsb <- merged_data[49:50]

merged_data <- merged_data %>% 
  mutate(ethnicity = case_when(
  str_detect(race, "HISPANIC") ~ "HISPANIC",
  str_detect(race, "BLACK") ~ "BLACK",
  str_detect(race, "ASIAN") ~ "ASIAN",
  str_detect(race, "WHITE") ~ "WHITE",
  str_detect(race, "AMERICAN INDIAN|PORTUGUESE|NATIVE HAWAIIAN|SOUTH AMERICAN|PATIENT DECLINED TO ANSWER|MULTIPLE RACE|UNABLE TO OBTAIN|UNKNOWN|OTHER") ~ "OTHER"
))

ethnicity <- merged_data$ethnicity

result <- data.frame(id, los_hosp, los_icu, age, sex, severity_score, mortality_icu, mortality_hosp, comorbidsa, comorbidsb, ethnicity)

result2 <- result %>% 
  summarise(median_los_hosp=median(los_hosp, na.rm=TRUE), median_los_icu=median(los_icu, na.rm=TRUE), mortality_icu=100*sum(result$mortality_icu)/nrow(result), 
            mortality_hosp=100*sum(result$mortality_hosp)/nrow(result), mean_age=mean(age, na.rm=TRUE), mean_severity_score=mean(severity_score, na.rm=TRUE), 
            malignant_cancer_p=100*(sum(result$malignant_cancer)/nrow(result)), chf_p=100*(sum(result$chf)/nrow(result)), copd_p=100*(sum(result$copd)/nrow(result)),
            ckd_p=100*(sum(result$ckd)/nrow(result)), atrial_fibrillation_p=100*(sum(result$atrial_fibrillation)/nrow(result)), cld_p=100*(sum(result$cld)/nrow(result)),
            diabetes_p=100*(sum(result$diabetes)/nrow(result)), ihd_p=100*(sum(result$ihd)/nrow(result)), stroke_p=100*(sum(result$stroke)/nrow(result)),
            hypertension_p=100*(sum(result$hypertension)/nrow(result)), cardiac_arrest_p=100*(sum(result$cardiac_arrest)/nrow(result)), 
            sepsis_p=100*(sum(result$sepsis)/nrow(result)), aki_p=100*(sum(result$aki)/nrow(result)), cns_infect_p=100*(sum(result$cns_infect)/nrow(result)), tbi_p=100*(sum(result$tbi)/nrow(result)))

readr::write_csv(merged_data, 'merged_data.csv')

sample_result <- sample_n(result, 100)
ggplot(sample_result) +
  geom_line(aes(x=id, y=age, col=ethnicity))

            