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

outcomes = c("anxiety", "depress", "stress", "lonely", "fussy", "fear")


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
            moe = sem*cv) %>%
  ungroup() %>%
  mutate(income = ifelse(poverty == 0, "Middle- to high-income families", "Low-income families")) %>%
  select(income, Week, emotion, m, moe) %>%
  rename("Average" = m, 
         `Margin of Error` = moe)
  
write.csv(first3, here("figure data/0509_weekly_emotion_by_income.csv"))

