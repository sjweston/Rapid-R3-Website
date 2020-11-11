library(here)
library(tidyverse)
library(knitr)
library(kableExtra)
library(haven)
# require(devtools)
# install_version("zipcode", version = "1.0", repos = "http://cran.us.r-project.org")
library(zipcode)
library(ggthemr)

source(here("Scripts/score data.R"))

outcomes = c("anxiety", "depress", "lonely", "stress", "fussy", "fear")
outcomes_p = paste0(outcomes, "_current_lots")

source(here("Functions/pomp.R"))

scored = scored %>%
  mutate_at(vars("anxiety","depress"), pomp_ad) %>%
  mutate_at("stress", pomp_stress) %>%
  mutate_at("lonely", pomp_lonely) %>%
  mutate_at(vars("fussy", "fear"), pomp_child) 


# immediate change --------------------------------------------------------

change = scored %>%
  filter(!is.na(poverty)) %>%
  select(CaregiverID, Week, poverty, all_of(outcomes)) %>%
  group_by(CaregiverID) %>%
  arrange(Week) %>%
  filter(row_number() %in% c(1,2)) %>%
  mutate(Week = ifelse(Week == 0, "Before", "After")) %>%
  ungroup() %>%
  gather("emotion", "value",all_of(outcomes)) %>%
  spread(Week, value) %>%
  mutate(Change = After-Before) %>%
  group_by(poverty, emotion) %>%
  summarize(sd = sd(Change, na.rm=T),
            Change = mean(Change, na.rm=T),
            n = n(),
            sem = sd/sqrt(n),
            cv = qt(p = .975, df = n-1),
            `Margin of Error` = cv*sem)%>%
  mutate(income = ifelse(poverty == 0, "Middle- to high-income families", "Low-income families")) %>%
  ungroup() %>%
  select(income, emotion, Change, `Margin of Error`)
write.csv(change, here("figure data/0509_immediate_change_by_income.csv"))

change = scored %>%
  filter(!is.na(poverty)) %>%
  select(CaregiverID, Week, poverty, all_of(outcomes_p)) %>%
  group_by(CaregiverID) %>%
  arrange(Week) %>%
  filter(row_number() %in% c(1,2)) %>%
  mutate(Week = ifelse(Week == 0, "Before", "After")) %>%
  ungroup() %>%
  gather("emotion", "value",all_of(outcomes_p)) %>%
  spread(Week, value) %>%
  mutate(Change = After-Before) %>%
  group_by(poverty, emotion) %>%
  summarize(sd = sd(Change, na.rm=T),
            Change = mean(Change, na.rm=T),
            n = n(),
            sem = sd/sqrt(n),
            cv = qt(p = .975, df = n-1),
            `Margin of Error` = cv*sem)%>%
  mutate(income = ifelse(poverty == 0, "Middle- to high-income families", "Low-income families")) %>%
  ungroup() %>%
  select(income, emotion, Change, `Margin of Error`)
write.csv(change, here("figure data/0509_immediate_change_percentage_by_income.csv"))

# first 3 weeks --------------------------------------------------------

first3 = scored %>%
  filter(!is.na(poverty)) %>%
  filter(Week > 0) %>%
  filter(Week < 4) %>%
  select(CaregiverID, Week, poverty, all_of(outcomes)) %>%
  gather("emotion", "value", all_of(outcomes)) %>%
  group_by(Week, poverty, emotion) %>%
  filter(!is.na(value)) %>%
  summarize(m = mean(value),
            s = sd(value),
            n = n(),
            sem = s/sqrt(n),
            cv = qt(p = .975, df = n-1),
            moe = sem*cv,
            m = round(m, 2),
            moe = round(moe, 2)) %>%
  ungroup() %>%
  mutate(income = ifelse(poverty == 0, "Middle- to high-income families", "Low-income families")) %>%
  select(income, Week, emotion, m, moe) %>%
  rename("Average" = m, 
         `Margin of Error` = moe)
  
write.csv(first3, here("figure data/0509_weekly_emotion_by_income.csv"))

first3_index = scored %>%
  filter(!is.na(poverty)) %>%
  filter(Week > 0) %>%
  filter(Week < 4) %>%
  select(CaregiverID, Week, poverty, all_of(outcomes)) %>%
  gather("emotion", "value", all_of(outcomes)) %>%
  mutate(emotion = case_when(
    emotion %in% c("fussy", "fear") ~ "child",
    !(emotion %in% c("fussy", "fear")) ~ "parent")) %>%
  group_by(Week, CaregiverID, poverty, emotion) %>%
  summarize(value = mean(value,na.rm=T)) %>%
  ungroup() %>%
group_by(Week, poverty, emotion) %>%
  filter(!is.na(value)) %>%
  summarize(m = mean(value),
            s = sd(value),
            n = n(),
            sem = s/sqrt(n),
            cv = qt(p = .975, df = n-1),
            moe = sem*cv,
            lower = m - moe,
            upper = m + moe) %>%
  ungroup() %>%
  mutate(income = ifelse(poverty == 0, "Middle- to high-income families", "Low-income families")) %>%
  select(income, Week, emotion, m, lower, upper) %>%
  rename("Average" = m, 
        "Upper Bound" = upper,
        "Lower Bound" = lower)

write.csv(first3_index, here("figure data/0509_weekly_index_by_income.csv"))

first3Percent = scored %>%
  filter(!is.na(poverty)) %>%
  filter(Week > 0) %>%
  filter(Week < 4) %>%
  select(CaregiverID, Week, poverty, all_of(outcomes_p)) %>%
  gather("emotion", "value", all_of(outcomes_p)) %>%
  group_by(Week, poverty, emotion) %>%
  filter(!is.na(value)) %>%
  summarize(count = sum(value),
            n = n(),
            Percent = 100*count/n,
            Percent = round(Percent, 1)) %>%
  ungroup() %>%
  mutate(income = ifelse(poverty == 0, "Middle- to high-income families", "Low-income families")) %>%
  select(income, Week, emotion, Percent) 

write.csv(first3Percent, here("figure data/0509_weekly_emotion_percent_by_income.csv"))


# pre-pandemic ------------------------------------------------------------

prepan = scored %>%
  filter(Week == 0) %>%
  filter(!is.na(poverty)) %>%
  select(poverty, anxiety, depress, stress, lonely, fussy, fear) %>%
  gather("emotion", "value", -poverty) %>%
  filter(!is.na(value)) %>%
  group_by(poverty, emotion) %>%
  summarize(mean = mean(value),
            sd = sd(value),
            n = n(),
            sem = sd/sqrt(n),
            cv = qt(.975, df = n-1),
            moe = cv*sem, 
            upper = mean + moe, 
            lower = mean - moe) %>%
  mutate(income = ifelse(poverty == 0, "Middle- to high-income families", "Low-income families")) %>%
  select(income, emotion, mean, upper, lower)

write.csv(prepan, here("figure data/0509_prepandemic_emotion_by_income.csv"))

prepan = scored %>%
  filter(Week == 0) %>%
  filter(!is.na(poverty)) %>%
  select(poverty, all_of(outcomes_p)) %>%
  gather("emotion", "value", -poverty) %>%
  filter(!is.na(value)) %>%
  group_by(poverty, emotion) %>%
  summarize(count = sum(value),
            n = n(),
            percent = round(100*count/n),2) %>%
  ungroup() %>%
  mutate(income = ifelse(poverty == 0, "Middle- to high-income families", "Low-income families")) %>%
  select(income, emotion, percent)

write.csv(prepan, here("figure data/0509_prepandemic_emotion_by_income_percent.csv"))
