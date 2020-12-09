library(here)
library(tidyverse)
library(knitr)
library(kableExtra)
library(haven)
# require(devtools)
# install_version("zipcode", version = "1.0", repos = "http://cran.us.r-project.org")
library(janitor)
library(psych)
library(zipcode)
library(jsonlite)
library(readr)
library(zoo) # for rolling averages and sums!

load(here("../../Data Management R3/R Data/scored.Rdata"))

scored_in_environ = length(which(grepl("scored", ls()))) > 0

# source functions --------------------------------------------------------

source(here("Functions/score_report.R"))
source(here("Scripts/demo groups.R"))
source(here("Functions/pomp.R"))
source(here("Functions/fpl.R"))


if(!scored_in_environ){

data(zipcode)
master = read_sav(here("../../Data Management R3/CC_Clean Survey Data/00_R3 MasterFile/MasterFile.sav"))

master = filter(master, CaregiverID != "") 
master = master %>%
  filter(is.na(Exclude)) %>%
  group_by(CaregiverID, Week) %>%
  filter(row_number() == max(row_number())) %>%
  ungroup()


# get variable names and levels -------------------------------------------

max.week = max(master$Week)

master.names = names(master)
master.labels = sjlabelled::get_label(master)
master.levels = sjlabelled::get_labels(master)

master.levels = lapply(master.levels, paste, collapse = "; ")

master.data = data.frame(Variable = master.names,
           Item = master.labels,
           Responses = unlist(master.levels), stringsAsFactors = F)

num_responses = function(x){length(which(!is.na(x)))}
master.num = master %>%
  group_by(Week) %>%
  summarize_all(num_responses) %>%
  gather("Variable", "num_responses",-Week) %>%
  mutate(Week = paste0("Week", Week),
         Week = factor(Week, levels = paste0("Week", c(1:max.week)))) %>%
  spread(Week, num_responses)

master.data = full_join(master.data, master.num)

save(master.data, file = "allvariables.Rdata")


# score data --------------------------------------------------------------

scored = score_report(data = master, master = T)

# poverty threshold --------------------------------------------------------------

census = readxl::read_xls(here("data/thresh19.xls"), sheet = 2)

scored = scored %>%
  select(CaregiverID, Week, BaselineWeek, income,
         household_size, num_children_raw) %>%
  mutate(household_size = ifelse(household_size > 9, 9, household_size),
         num_children_raw = ifelse(num_children_raw > 8, 8, num_children_raw)) %>%
  left_join(census) %>%
  mutate(poverty150s = ifelse(income < poverty_threshold*1.5,1,0),
         poverty200s = ifelse(income < poverty_threshold*2,1,0)) %>%
  select(CaregiverID, Week, poverty150s, poverty200s) %>%
  full_join(scored)

if(!(contains_items("FPL\\.", master))){
  scored$poverty100 = scored$poverty150s
  scored$poverty125 = scored$poverty150s
  scored$poverty150 = scored$poverty150s
  scored$poverty200 = scored$poverty200s
}

# baseline week -----------------------------------------------------------

pre_pandemic = scored %>%
  select(CaregiverID, Week, contains("_pre")) %>%
  group_by(CaregiverID) %>% 
  mutate_at(vars(-CaregiverID, -Week), select_first) %>% 
  filter(Week == min(Week)) %>%
  ungroup() %>%
  mutate(Week = 0) 

names(pre_pandemic) = gsub("_pre", "_current", names(pre_pandemic))
names(pre_pandemic) = gsub("_current$", "", names(pre_pandemic))
scored = scored %>%
  select(-contains("_pre")) %>%
  full_join(pre_pandemic)

# new baseline week

baseline = scored %>%
  filter(Week != 0) %>%
  group_by(CaregiverID) %>%
  filter(Week == min(Week)) %>%
  select(CaregiverID, Week) %>%
  ungroup() %>%
  rename(BaselineWeek = Week)

scored = scored %>%
  select(-BaselineWeek) %>%
  full_join(baseline)

# repeat ------------------------------------------------

scored = scored %>%
  filter(Week > 0) %>%
  group_by(CaregiverID) %>%
  summarize(n = n()) %>%
  mutate(returner = ifelse(n > 1, 1, 0)) %>%
  full_join(scored)

# variables assessed only at baseline

scored = scored %>%
  group_by(CaregiverID) %>%
  arrange(Week) %>%
  mutate_at(.vars = c("language","income", "household_size", "num_parents", "num_children_raw", "gender", 
                      "race_cat", "black", "white", "minority", "native", "asian",
                      "hawaii", "other_race", "latinx", "age", "edu", "edu_cat",
                      "zip", "state", "region", "insurance_type", "childinsurance_type",
                      "single", "disability", "work_status", 
                      "child_age03", "child_age45", "num_children_age03", "num_children_age45", "num_children_age612", 
                      "current_income", "poverty100", "poverty125", "poverty150", "poverty200"), 
            ~case_when(
              is.na(.x) ~ lag(.x),
              TRUE ~ .x)) %>% # carry these variables down through NA's
  mutate_at(.vars = c("language","income", "household_size", "num_parents", "num_children_raw", "gender", 
                      "race_cat", "black", "white", "minority", "native", "asian",
                      "hawaii", "other_race", "latinx", "age", "edu", "edu_cat",
                      "zip", "state", "region", "insurance_type", "childinsurance_type",
                      "single", "disability",  "work_status", 
                      "poverty100", "poverty125", "poverty150", "poverty200"), 
            ~case_when(
              is.na(.x) ~ lead(.x),
              TRUE ~ .x)) %>% # carry these variables down through NA's
  ungroup()


# employment change -------------------------------------------------------

# isolate work status pre

scored = scored %>%
  filter(Week == 0) %>%
  select(CaregiverID, work_status) %>%
  rename(work_status_pre = work_status) %>%
  full_join(scored)

scored = scored %>%
  mutate(
    employment_change = case_when(
      (work_status_pre == "Employed" & work_status == "Unemployed") ~ "Became Unemployed",
      (work_status_pre == "Unemployed" & work_status == "Employed") ~ "Became Employed",
      (work_status_pre == "Employed" & work_status == "Other") ~ "Became Unemployed",
      (work_status_pre == "Other" & work_status == "Employed") ~ "Became Employed",
      (work_status_pre == "Employed" & work_status == "Employed") ~ "Stable Employed",
      (work_status_pre == "Unemployed" & work_status == "Unemployed") ~ "Stable Unemployed",
      (work_status_pre == "Other" & work_status == "Other") ~ "Stable Other",
      TRUE ~ NA_character_))


# # federal poverty level ---------------------------------------------------
# 
# scored = scored %>%
#   rowwise() %>%
#   mutate(FPL = FPL.function(JOB.002, STATE_CODED, income))
# 
# scored = scored %>%
#   mutate(
#     poverty100 = case_when(
#       FPL == 1 ~ 1, 
#       !is.na(FPL) ~ 0),
#     poverty125 = case_when(
#       FPL == 1 ~ 1, 
#       FPL == 2 ~ 1, 
#       !is.na(FPL) ~ 0),
#     poverty150 = case_when(
#       FPL == 1 ~ 1, 
#       FPL == 2 ~ 1, 
#       FPL == 3 ~ 1, 
#       !is.na(FPL) ~ 0),
#     poverty200 = case_when(
#       FPL == 1 ~ 1, 
#       FPL == 2 ~ 1, 
#       FPL == 3 ~ 1, 
#       FPL == 4 ~ 1, 
#       !is.na(FPL) ~ 0),
#     )


# days sheltering in place ------------------------------------------------


shelter = readxl::read_xlsx(here("data/shelter_finra.xlsx"), sheet = 1)
shelter = shelter %>%
  mutate(Order = as.Date(Order))

scored = scored %>%
  full_join(shelter) %>%
  mutate(days_sheltering = Date-Order) %>%
  mutate(days_sheltering = ifelse(!is.na(state) & is.na(days_sheltering), 0, days_sheltering)) 


# COVID-19 state statistics -----------------------------------------------


# state_covid = jsonlite::fromJSON("https://covidtracking.com/api/v1/states/daily.json")
# state_covid = state_covid %>%
#   mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
#   rename(Date = date)
# 
# scored = scored %>%
#   left_join(state_covid)


# COVID-19 county statistics ----------------------------------------------
# data from US Census bureau https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/

pop_est = read.csv(here("data/co-est2019-alldata.csv"), stringsAsFactors = F)

pop_est = pop_est %>%
  filter(COUNTY !=0) %>%
  mutate(state_fips = str_pad(STATE, width = 2, side = "left", pad = "0"),
         county_fips = str_pad(COUNTY, width = 3, side = "left", pad = "0"),
         fips = paste0(state_fips, county_fips)) %>%
  rename(population  = POPESTIMATE2019) %>%
  select(fips, population)

github.location = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
nyt_data = read_csv(url(github.location))

nyt_data = nyt_data %>%
  full_join(pop_est) # add population estimates

nyt_data = nyt_data %>%
  group_by(fips) %>%
  arrange(date) %>%
  mutate(
    new_cases = ifelse(row_number() == 1, 1, cases - lag(cases, default = cases[1])),
    new_deaths = deaths - lag(deaths, default = deaths[1]),
    total_cases = cumsum(new_cases),
    total_deaths = cumsum(new_deaths),
    new_cases_twoweeks = rollapplyr(new_cases, FUN = sum, partial = T, width = 14),
    new_deaths_twoweeks = rollapplyr(new_deaths, FUN = sum, partial = T, width = 14),
    growth_cases_oneweek = 100*(rollapplyr(new_cases, FUN = sum, partial = T, width = 7))/lag(total_cases, 7),
    growth_deaths_oneweek = 100*(rollapplyr(new_deaths, FUN = sum, partial = T, width = 7))/lag(total_deaths, 7),
    doubling_time_cases = 7*(70/growth_cases_oneweek),
    total_cases_per1000 = (total_cases/population)*1000,
    total_deaths_per1000 = (total_deaths/population)*1000,
    new_cases_per1000 = (new_cases_twoweeks/population)*1000,
    new_deaths_per1000 = (new_deaths_twoweeks/population)*1000) %>%
  select(fips, date, total_cases, total_deaths, 
         new_cases_twoweeks, new_deaths_twoweeks, growth_cases_oneweek, growth_deaths_oneweek, 
         doubling_time_cases, total_cases_per1000, total_deaths_per1000, 
         new_cases_per1000, new_deaths_per1000)

# ADD IN GROWTH FOR DEATHS, WHEN CODE FINALIZED


#data from https://simplemaps.com/data/us-zips  

county_crosswalk = read.csv(here("data/uszips.csv"), stringsAsFactors = F)
county_crosswalk = county_crosswalk %>%
  select(zip, county_fips, density, population) %>%
  rename(fips = county_fips) %>%
  mutate_all(as.character) %>%
  right_join(nyt_data) %>%
  rename(Date = date) %>%
  filter(zip %in% scored$zip) %>%
  filter(!is.na(zip))

after0 = scored %>%
  filter(Week > 0) %>%
  left_join(county_crosswalk, by = c("zip", "Date"))

scored = scored %>%
  filter(Week == 0) %>%
  full_join(after0)

scored = scored %>%
  mutate(density = as.numeric(as.character(density)),
         rural = case_when(
           density < 500 ~ "rural",
           density >= 1000 ~ "urban", 
           TRUE ~ NA_character_
         ))


# med income by zip -------------------------------------------------------

zipincome = read.csv(here("../../Data Management R3/CC_Clean Survey Data/00_R3 MasterFile/Archive/Income_Data.csv"),
                     na.strings = "-")

zipincome = select(zipincome, Zip_Code, Housholds_Median_Income)

names(zipincome) = c("zip", "median_income")
zipincome$median_income = as.numeric(as.character(zipincome$median_income))
zipincome$zip = as.character(zipincome$zip)

zipincome = filter(zipincome, !is.na(zip))

scored = left_join(scored, zipincome)


# composites --------------------------------------------------------------

scored[,c("anxiety","depress", "lonely", "stress", "fussy", "fear")] = apply(scored[,c("anxiety","depress", "lonely", "stress", "fussy", "fear")], 2, pomp)

scored = scored %>%
  rowwise() %>%
  mutate(mental_health = mean(c(anxiety, depress, lonely, stress), na.rm=T),
         child_mental = mean(c(fussy, fear), na.rm=T)) %>%
  mutate(material_hardship = case_when(
    Week == 0 ~ NA_real_,
    TRUE ~ sum(c_across(starts_with("diff_pay_")), na.rm=T))) %>%
  ungroup() %>%
  mutate(
    black_cat = factor(black, labels = c("non-Black", "Black")),
    latinx_cat = factor(latinx, labels = c("non-Latinx", "Latinx")),
    poverty_cat = factor(poverty150, labels = c("Above 1.5 x FPL", "Below 1.5 x FPL")),
    single_cat = factor(single, labels = c("Dual parent", "Single parent")),
    disability_cat = factor(disability, labels = c("No disability", "Child with disability")),
    race_ethnic = case_when(
      black == 1 ~ "Black",
      latinx == 1 ~ "Latinx",
      !is.na(black) ~ "White",
      !is.na(latinx) ~ "White",
      TRUE ~ NA_character_),
    race_poverty = case_when(
      black == 1 & poverty150 == 1 ~ "Low income, Black",
      black == 1 & poverty150 == 0 ~ "High income, Black",
      latinx == 1 & poverty150 == 1 ~ "Low income, Latinx",
      latinx == 1 & poverty150 == 0 ~ "High income, Latinx",
      white == 1 & poverty150 == 1 ~ "Low income, White",
      white == 1 & poverty150 == 0 ~ "High income, White",
      TRUE ~ NA_character_)) %>%
  mutate(num_hardship = material_hardship,
    material_hardship = ifelse(material_hardship > 0, 1, 0))  %>%
  group_by(Week) %>%
  mutate(Date_group =case_when(
    Week == 0 ~ as.Date("2020-03-01"),
    TRUE ~ min(Date)
  )) %>% ungroup()

scored = scored %>%
  mutate(missed_vaccine_any = case_when(
    miss_vaccine.1 == 1 ~ 1,
    miss_vaccine.2 == 1 ~ 1,
    miss_vaccine.3 == 1 ~ 1,
    miss_vaccine.4 == 1 ~ 1,
    miss_vaccine.5 == 1 ~ 1,
    !is.na(miss_vaccine.1) ~ 0,
    !is.na(miss_vaccine.2) ~ 0,
    !is.na(miss_vaccine.3) ~ 0,
    !is.na(miss_vaccine.4) ~ 0,
    !is.na(miss_vaccine.5) ~ 0
  ))

# state medicaid ----------------------------------------------------------

scored = scored %>%
  mutate(medicaid_expansion = case_when(
    state %in% c("WA", "OR", "CA", "AK", "HI",
                 "ID", "NV", "MT", "UT", "AZ",
                 "CO", "NM", "ND", "MN", "IA",
                 "IL", "AR", "LA", "MI", "IN",
                 "KY", "OH", "WV", "VA", "PA",
                 "NY", "ME", "VT", "NH", "MA",
                 "RI", "CT", "NJ", "DE", "MD", 
                 "DC") ~ "Medicaid Expansion",
    state %in% c("NE", "MO", "OK") ~ "Not expanded",
    state %in% c("WY", "SD", "WI", "KS", "TX",
                 "TN", "NC", "SC", "MS", "AL",
                 "GA", "FL") ~ "Not expanded"))


# childcare subsidy ----------------------------------------------------

scored = scored %>%
  mutate(childcare_sub = case_when(
    state %in% c("AK", "WA", "OR", "CA", "HI",
                 "NV", "UT", "CO", "NM", "OK",
                 "TX", "IA", "LA", "WI", "IL",
                 "KY", "TN", "MS", "MI", "NC",
                 "PA", "VA", "SC", "NY", "NJ",
                 "MD", "DC", "FL", "VT", "MA",
                 "CT", "ME", "RI") ~ "Funding to childcare providers",
    !is.na(state) ~ "No legislation",
    TRUE ~ NA_character_))

# Clean workspace ---------------------------------------------------------

save(scored, master, file = paste0(here("../../Data Management R3/R Data/"), "scored.Rdata"))
} else{
  rm(scored)
  load(here("../../Data Management R3/R Data/scored.Rdata"))
}
