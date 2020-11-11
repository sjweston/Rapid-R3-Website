
# load packages -----------------------------------------------------------

library(here)
library(haven)
# require(devtools)
# install_version("zipcode", version = "1.0", repos = "http://cran.us.r-project.org")
library(zipcode)
library(tidyverse)


# load data ---------------------------------------------------------------

data(zipcode)
r1data = read_sav(here("../Survey 1/R3.R1 DATA CLEAN/R3.R1.13APR20.CLEAN.sav"))
r2data = read_sav(here("../Survey 2/R3.R2 Merged/R3.R2.18APR20.ALL.sav"))


# source functions --------------------------------------------------------

source(here("Functions/score_report.R"))

# score data --------------------------------------------------------------

# data = r1data
# week = 1
scored = score_report(data = r1data, week = 1)
scored_2 = score_report(data = r2data, week = 2)
