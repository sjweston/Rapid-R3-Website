library(here)
library(tidyverse)
library(knitr)
library(kableExtra)
library(haven)
# require(devtools)
# install_version("zipcode", version = "1.0", repos = "http://cran.us.r-project.org")
library(zipcode)


data(zipcode)
master = read_sav(here("../../Data Management R3/CC_Clean Survey Data/00_R3 MasterFile/MasterFile.sav"))

# source functions --------------------------------------------------------

source(here("Functions/score_report.R"))

# score data --------------------------------------------------------------

scored = score_report(data = master, master = T)

# poverty threshold --------------------------------------------------------------

census = readxl::read_xls(here("thresh19.xls"), sheet = 2)

scored = scored %>%
  select(CaregiverID, Week, BaselineWeek, income, household_size, num_children_raw) %>%
  full_join(census) %>%
  mutate(poverty = ifelse(income < poverty_threshold,1,0)) %>%
  select(CaregiverID, Week, BaselineWeek, income, household_size, num_children_raw, poverty) %>%
  full_join(scored)


# days sheltering in place ------------------------------------------------


shelter = readxl::read_xlsx(here("shelter_finra.xlsx"), sheet = 1)
shelter = shelter %>%
  mutate(Order = as.Date(Order))

scored = scored %>%
  full_join(shelter) %>%
  mutate(days_sheltering = Date-Order) %>%
  mutate(days_sheltering = ifelse(!is.na(state) & is.na(days_sheltering), 0, days_sheltering)) 
