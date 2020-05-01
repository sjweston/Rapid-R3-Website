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

data(zipcode)
master = read_sav(here("../../Data Management R3/CC_Clean Survey Data/00_R3 MasterFile/MasterFile.sav"))

master = filter(master, CaregiverID != "")

# source functions --------------------------------------------------------

source(here("Functions/score_report.R"))
source(here("Scripts/demo groups.R"))

# score data --------------------------------------------------------------

scored = score_report(data = master, master = T)




# poverty threshold --------------------------------------------------------------

census = readxl::read_xls(here("data/thresh19.xls"), sheet = 2)

scored = scored %>%
  select(CaregiverID, Week, BaselineWeek, income, 
         household_size, num_children_raw) %>%
  left_join(census) %>%
  mutate(poverty = ifelse(income < poverty_threshold,1,0)) %>%
  select(CaregiverID, Week, BaselineWeek, income, household_size, num_children_raw, poverty) %>%
  full_join(scored)

# baseline week -----------------------------------------------------------

pre_pandemic = scored %>%
  select(CaregiverID, Week, BaselineWeek, all_of(demos), income, state, contains("_pre")) %>%
  select(-working_current) %>%
  group_by(CaregiverID) %>%
  filter(Week == min(Week)) %>%
  ungroup() %>%
  mutate(Week = 0)

names(pre_pandemic) = gsub("_pre", "_current", names(pre_pandemic))
scored = scored %>%
  select(-contains("_pre")) %>%
  full_join(pre_pandemic)

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
    total_cases = cumsum(cases),
    total_deaths = cumsum(deaths),
    new_cases_twoweeks = rollapplyr(cases, FUN = sum, partial = T, width = 14),
    new_deaths_twoweeks = rollapplyr(deaths, FUN = sum, partial = T, width = 14),
    growth_cases_oneweek = 100*(rollapplyr(cases, FUN = sum, partial = T, width = 7))/lag(total_cases, 7),
    doubling_time_cases = 7*(70/growth_cases_oneweek),
    total_cases_per1000 = (total_cases/population)*1000,
    total_deaths_per1000 = (total_deaths/population)*1000,
    new_cases_per1000 = (new_cases_twoweeks/population)*1000,
    new_deaths_per1000 = (new_deaths_twoweeks/population)*1000) %>%
  select(fips, date, total_cases, total_deaths, 
         new_cases_twoweeks, new_deaths_twoweeks, growth_cases_oneweek, 
         doubling_time_cases, total_cases_per1000, total_deaths_per1000, 
         new_cases_per1000, new_deaths_per1000)

# ADD IN GROWTH FOR DEATHS, WHEN CODE FINALIZED


#data from https://simplemaps.com/data/us-zips  

county_crosswalk = read.csv(here("data/uszips.csv"), stringsAsFactors = F)
county_crosswalk = county_crosswalk %>%
  select(zip, county_fips) %>%
  rename(fips = county_fips) %>%
  mutate_all(as.character) %>%
  right_join(nyt_data) %>%
  rename(Date = date)

after0 = scored %>%
  filter(Week > 0) %>%
  left_join(county_crosswalk, by = c("zip", "Date"))

scored = scored %>%
  filter(Week == 0) %>%
  full_join(after0)

