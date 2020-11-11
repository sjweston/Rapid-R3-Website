# figures for single care givers post

library(lme4)
library(broom.mixed)
library(sjPlot)
library(ggpubr)
library(plotly)
library(here)

source(here("Scripts/score data.R"))
source(here("Functions/core_funs.R"))

wb_vars = c("mental_health", "child_mental",
            "anxiety", "depress", "stress", "lonely",
            "fussy", "fear")

scored = scored %>%
  mutate(wellbeing = 100-mental_health,
         child_wellbeing = 100-child_mental,
         unemployed_cat = ifelse(unemployed == 1, "Unemployed", "Employed"))

last_assessment = scored %>%
  group_by(CaregiverID) %>%
  filter(Week == max(Week)) %>%
  ungroup()

scored = scored %>%
  mutate(Week = case_when(
    Week < 17 ~ Week,
    Week %% 2 == 0 ~ NA_real_,
    TRUE ~ Week
  )) %>%
  filter(!is.na(Week)) 


# wellbeing by group ------------------------------------------------------

last_assessment %>%
  gather(variable, value, mental_health, child_mental) %>%
  filter(!is.na(value)) %>%
  filter(!is.na(single_cat)) %>%
  group_by(single_cat, variable) %>%
  summarize(n = n(),
            mean = mean(value),
            moe = 1.96*sd(value)/sqrt(n),
            lowerci = mean-moe,
            upperci = mean+moe) %>%
  select(single_cat, variable, mean, lowerci, upperci) %>%
  mutate_if(is.numeric, round, 1) %>%
  write.csv(here("figure data/110520_mentalhealth_single.csv"))

# wellbeing by group ------------------------------------------------------

last_assessment %>%
  gather(variable, value, mental_health, child_mental) %>%
  filter(!is.na(value)) %>%
  filter(!is.na(single_cat)) %>%
  group_by(Week, single_cat, variable) %>%
  summarize(n = n(),
            Date = min(Date,na.rm = T),
            mean = mean(value),
            moe = 1.96*sd(value)/sqrt(n),
            lowerci = mean-moe,
            upperci = mean+moe) %>%
  select(Date, single_cat, variable, mean, lowerci, upperci) %>%
  mutate_if(is.numeric, round, 1) %>%
  write.csv(here("figure data/110520_mentalhealth_single_time.csv"))

# specifci wellbeing by group (mean) ------------------------------------------------------

last_assessment %>%
  gather(variable, value, "anxiety", "depress", "stress", "lonely",
         "fussy", "fear") %>%
  filter(!is.na(value)) %>%
  filter(!is.na(single_cat)) %>%
  group_by(single_cat, variable) %>%
  summarize(n = n(),
            mean = mean(value),
            moe = 1.96*sd(value)/sqrt(n),
            lowerci = mean-moe,
            upperci = mean+moe) %>%
  select(single_cat, variable, mean, lowerci, upperci) %>%
  mutate_if(is.numeric, round, 1) %>%
  write.csv(here("figure data/110520_mentalhealth_specific_mean_single.csv"))

# specifci wellbeing by group (percent) ------------------------------------------------------

last_assessment %>%
  gather(variable, value, anxiety_current_some, depress_current_some, stress_current_some, 
         lonely_current_some, fussy_current_some, fear_current_some) %>%
  filter(!is.na(value)) %>%
  filter(!is.na(single_cat)) %>%
  group_by(single_cat, variable) %>%
  summarize(n = n(),
            percent = sum(value)/n) %>%
  mutate(moe = map2_dbl(percent, n, moe.p)) %>%
  mutate_at(vars(percent, moe), ~100*.x) %>%
  mutate(lowerci = percent-moe, 
         upperci = percent+moe) %>%
  select(single_cat, variable, percent, lowerci, upperci) %>%
  mutate_if(is.numeric, round, 1) %>%
  write.csv(here("figure data/110520_mentalhealth_specific_percent_single.csv"))


# material hardship -------------------------------------------------------

last_assessment %>%
  select(single_cat, starts_with("diff_pay")) %>%
  gather(variable, value, -single_cat) %>%
  group_by(single_cat, variable) %>%
  filter(!is.na(value)) %>%
  summarize(n = n(),
            percent = sum(value)/n) %>%
  mutate(moe = map2_dbl(percent, n, moe.p)) %>%
  mutate_at(vars(percent, moe), ~100*.x) %>%
  mutate(lowerci = percent-moe, 
         upperci = percent+moe,
         variable = str_remove(variable, "diff_pay_")) %>%
  select(single_cat, variable, percent, lowerci, upperci) %>%
  mutate_if(is.numeric, round, 1)  %>%
  write.csv(here("figure data/110520_materialHardship_single.csv"))
  
