library(here)
library(tidyverse)
library(knitr)
library(kableExtra)
library(haven)
# require(devtools)
# install_version("zipcode", version = "1.0", repos = "http://cran.us.r-project.org")
library(zipcode)
library(ggthemr)

subset = scored %>%
  filter(Week %in% c(0,1)) %>%
  select(CaregiverID, Week, fear, disability) %>%
  spread(Week, fear)

source(here("Scripts/score data.R"))
# test difference in trend ------------------------------------------------


difference = subset$`1`-subset$`0`

t.test(difference~subset$disability)


disability = scored %>%
  filter(disability == "1")


# figures -----------------------------------------------------------------


# figure 1: fear trends by week, separated by group
mean_d = scored %>%
  group_by(Week, disability) %>%
  summarize(fear = mean(fear, na.rm=T))
write.csv(mean_d, here("figure data/0504_average_fear_by_disability.csv"))

sum_d = scored %>%
  group_by(Week, disability) %>%
  filter(!is.na(fear_current_lots)) %>%
  summarize(count = sum(fear_current_lots, na.rm=T),
            n = n()) %>%
  mutate(percent = count/n)
write.csv(mean_d, here("figure data/0504_percent_lots_fear_by_disability.csv"))

# figure 2: mental health by group
scored %>%
  group_by(CaregiverID) %>%
  filter(Week == max(Week)) %>%
  group_by(disability) %>%
  summarize(anxiety = mean(anxiety, na.rm=T),
            depression = mean(depress, na.rm=T),
            stress = mean(stress, na.rm=T),
            fussy = mean(fussy, na.rm=T),
            fear = mean(fear, na.rm=T)) %>%
  write.csv(., here("figure data/0504_average_emotion_by_disability.csv"))

# figure 2: mental health by group
scored %>%
  select(CaregiverID, Week, disability, anxiety, depress, stress, fussy, fear) %>%
  mutate(Week = ifelse(Week == 0, "Pre-pandemic", "Post-pandemic")) %>%
  group_by(CaregiverID, Week, disability) %>%
  summarize_at(.vars = c("anxiety", "depress", "stress", "fussy", "fear"), 
               mean, na.rm=T) %>%
  ungroup() %>%
  group_by(Week, disability) %>%
  summarise_at(.vars = c("anxiety", "depress", "stress", "fussy", "fear"), 
               mean, na.rm=T) %>%
  write.csv(., here("figure data/0504_average_emotion_by_disability_and_time.csv"))

# figure 2a: caregiver mental health by group and week
source(here("Functions/pomp.R"))
scored %>%
  select(CaregiverID, Week, disability, anxiety, depress, stress) %>%
  mutate_at(.vars=c("anxiety", "depress"), pomp_ad) %>%
  mutate_at(.vars=c("stress"), pomp_stress) %>%
  gather("variable", "wb", anxiety, depress, stress) %>%
  group_by(CaregiverID, Week, disability) %>%
  summarize(wb = mean(wb, na.rm=T)) %>%
  group_by(Week, disability) %>%
  summarize(wb = mean(wb, na.rm=T)) %>%
  write.csv(., here("figure data/0504_caregiver_wellbeing_by_disability_and_time.csv"))

# figure 2b: child mental health by group and week

scored %>%
  select(CaregiverID, Week, disability, fussy, fear) %>%
  group_by(Week, disability) %>%
  summarize_at(.vars = c("fussy", "fear"), mean, na.rm = T)  %>%
  write.csv(., here("figure data/0504_child_wellbeing_by_disability_and_time.csv"))


source(here("Functions/summarizing_report.R"))

scored %>%
  select(CaregiverID, Week, disability, contains("lots")) %>%
  mutate(Week = ifelse(Week == 0, "Pre-pandemic", "Post-pandemic")) %>%
  group_by(CaregiverID, Week, disability) %>%
  summarize_at(vars(contains("lots")), mean, na.rm=T) %>%
  mutate_at(vars(contains("lots")), ceiling) %>%
  group_by(Week, disability) %>%
  summarise_at(vars(contains("lots")), percent.report) %>%
  write.csv(., here("figure data/0504_percent_lots_emotion_by_disability_and_time.csv"))

# figure 3: parent and child trends, diability only

scored %>%
  select(CaregiverID, Week, anxiety, depress, stress, fussy, fear) %>%
  gather("person", "value", -CaregiverID, - Week) %>%
  mutate(person = ifelse(grepl("^f", person), "Child", "Parent")) %>%
  group_by(CaregiverID, Week, person) %>%
  summarize(value = mean(value, na.rm=T)) %>%
  ungroup() %>%
  group_by(Week, person) %>%
  summarize(mental = mean(value, na.rm=T)) %>%
  mutate(max_score = ifelse(person == "Parent", 4, 2),
         proportion = mental/max_score) %>%
  write.csv(., here("figure data/0504_weekly_average_wellbeing_parent_child.csv"))

