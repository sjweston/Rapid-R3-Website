
# load packages and data --------------------------------------------------


library(here)
library(tidyverse)
library(haven)

master_2020 <- read_sav(here("../../Data Management R3/CC_Clean Survey Data/00_R3 MasterFile/MasterFile_groupings_2020.sav"))
master_2021 <- read_sav(here("../../Data Management R3/CC_Clean Survey Data/00_R3 MasterFile/MasterFile_groupings_2021.sav"))

master <- full_join (master_2020, master_2021)

master = filter(master, !is.na(Week))

#create dataset to store new variable

data03 = master %>% select(CaregiverID, Week)


# child ages --------------------------------------------------------------

# function to identify values
find_num = function(x, value){
  
  if(sum(grepl(value, x)) > 0){
    answer = "Yes"
  }else{
      answer = "No"
  }
  # make sure not all NA
  if(sum(!is.na(x)) == 0){answer = NA}
  
  return(answer)
}

# extract age variables
age_data = master %>%
  select(contains("DEMO.004")) 

has_0 = apply(age_data, 1, find_num, "0")
has_1 = apply(age_data, 1, find_num, "1")
has_2 = apply(age_data, 1, find_num, "2")
has_3 = apply(age_data, 1, find_num, "3")

data03$has_03 = apply(cbind(has_0, has_1, has_2, has_3), 1, find_num, "Yes")

num_children = apply(age_data, 1, FUN = function(x) sum(!is.na(x)))
num_children[num_children == 0] = NA
num_children[num_children == 9] = NA

num_03 = apply(cbind(has_0, has_1, has_2, has_3), 1, FUN = function(x) sum(x == "Yes"))
data03$num_children = num_children
data03$num_03 = num_03


# school learning ---------------------------------------------------------

learning_data = master %>%
  select(CaregiverID, Week,
         contains("SCHOOL.002")) 


learning_data = learning_data %>%
  filter(
      !is.na(SCHOOL.002_1) |
      !is.na(SCHOOL.002_2) |
      !is.na(SCHOOL.002_3) |
      !is.na(SCHOOL.002_4) |
      !is.na(SCHOOL.002_6) |
      !is.na(SCHOOL.002_7) ) %>%
  mutate(
    across(
      everything(), 
      replace_na, 0)) 

data03 %>%
  full_join(learning_data)

learning_data = master %>%
  select(CaregiverID, Week,
         contains("SCHOOL.003")) 


learning_data = learning_data %>%
  filter(
    !is.na(SCHOOL.003_1) |
      !is.na(SCHOOL.003_2) |
      !is.na(SCHOOL.003_3) |
      !is.na(SCHOOL.003_4) |
      !is.na(SCHOOL.003_5) |
      !is.na(SCHOOL.003_6) |
      !is.na(SCHOOL.003_7) |
      !is.na(SCHOOL.003_8) |
      !is.na(SCHOOL.003_9) |
      !is.na(SCHOOL.003_10) |
      !is.na(SCHOOL.003_11) ) %>%
  mutate(
    across(
      everything(), 
      replace_na, 0)) 

data03 %>%
  full_join(learning_data)

table(master$SCHOOL.005)

school_hours = str_extract(master$SCHOOL.005, "[0-9]*")
school_hours = as.numeric(school_hours)
#censor at 100
school_hours[school_hours>100] = 100

data03$school_hours = school_hours


# health insurance --------------------------------------------------------

data03$insurance = case_when(
  master$HEALTH.002 == 0 ~ "No",
  master$HEALTH.002 == 1 ~ "Yes",
  master$HEALTH.002 == 2 ~ "I don't know"
)

master = master %>%
  mutate(
    insurance_type = case_when(
      HEALTH.002 == 0 ~ "None/Other",
      HEALTH.002.a_1 == 1 ~ "Private",
      HEALTH.002.a_2 == 1 ~ "Public", # Medicare
      HEALTH.002.a_3 == 1 ~ "Public", # Medigap
      HEALTH.002.a_4 == 1 ~ "Public", # Medicaid
      HEALTH.002.a_5 == 1 ~ "Public", # CHIP
      HEALTH.002.a_6 == 1 ~ "Public", # Military
      HEALTH.002.a_7 == 1 ~ "Public", # Indian
      HEALTH.002.a_8 == 1 ~ "Public", # State-sponsored
      HEALTH.002.a_9 == 1 ~ "Public", # Other govt
      HEALTH.002.a_10 == 1 ~ "None/Other", # IDK
      HEALTH.002.a_11 == 1 ~  "None/Other", # Other
      HEALTH.002.a_12 == 1 ~  "None/Other", # N/A
      HEALTH.002.a.2_1 == 1 ~ "Private", # Through current or former employer
      HEALTH.002.a.2_2 == 1 ~ "Private", # Direct from insurance company
      HEALTH.002.a.2_3 == 1 ~ "Public", # Medicare
      HEALTH.002.a.2_4 == 1 ~ "Public", # Medicaid
      HEALTH.002.a.2_5 == 1 ~ "Public", # Military
      HEALTH.002.a.2_6 == 1 ~ "Public", # VA
      HEALTH.002.a.2_7 == 1 ~ "Public", # Indian
      HEALTH.002.a.2_8 == 1 ~ "None/Other", # Other insurance
      HEALTH.002.a.2_9 == 1 ~ "None/Other", # Other
      HEALTH.002.a.2_10 == 1 ~ "None/Other", # N/A
      TRUE ~ NA_character_)
  )

data03$insurance_type = master$insurance_type

data03$missed_wellbaby = case_when(
  master$HEALTH.004 == 0 ~ "No",
  master$HEALTH.004 == 1 ~ "Yes",
  master$HEALTH.004 == 2 ~ "NA")

missed_wb_data = master%>%
  select(CaregiverID, Week, HEALTH.004, matches("HEALTH.004.a_[1-8]$")) %>%
  filter(HEALTH.004 == 1) %>%
  select(-HEALTH.004) %>%
  mutate(
    across(
      everything(), 
      replace_na, 0)) 

data03 = full_join(data03, missed_wb_data)

well_baby_details = master %>%
  select(CaregiverID, Week, 
         contains("HEALTH.009"),
         contains("HEALTH.010"),
         contains("HEALTH.011"),
         contains("HEALTH.012"),
         contains("HEALTH.013")) %>%
  gather(variable, value, starts_with("HEALTH")) %>%
  mutate(variable = 
           str_replace(
             variable, 
             "HEALTH.009.", "child1_" ),
         variable = 
           str_replace(
             variable, 
             "HEALTH.010.", "child2_" ),
         variable = 
           str_replace(
             variable, 
             "HEALTH.011.", "child3_" ),
         variable = 
           str_replace(
             variable, 
             "HEALTH.012.", "child4_" ),
         variable = 
           str_replace(
             variable, 
             "HEALTH.013.", "child5_"),
         variable = 
           str_replace(
             variable, 
             "_a", "_age" ),
         variable = 
           str_replace(
             variable, 
             "_b$", "_vaccine"),
         variable = 
           str_replace(
             variable, 
             "_b.2", "_vaccine2"),
         variable = 
           str_replace(
             variable, 
             "_c", "_vaccine3"),
         variable = 
           str_replace(
             variable, 
             ".*010", "child2_miss"),
         variable = 
           str_replace(
             variable, 
             ".*011", "child3_miss"),
         variable = 
           str_replace(
             variable, 
             ".*012", "child4_miss"),
         variable = 
           str_replace(
             variable, 
             ".*013", "child5_miss")) %>%
  separate(variable, into = c("child", "variable")) %>%
  spread(variable, value)

well_baby_details = well_baby_details %>%
  filter(!is.na(age) | !is.na(miss)) %>%
  mutate(miss = ifelse(child == "child1",1,miss)) %>%
  filter(age <= 12) %>%
  select(-vaccine2, -vaccine3) %>%
  mutate(age = case_when(
    age == 1 ~ "First visit (3-5 day)",
    age == 2 ~ "1 month",
    age == 3 ~ "2 months",
    age == 4 ~ "4 months",
    age == 5 ~ "6 months",
    age == 6 ~ "9 months",
    age == 7 ~ "12 months",
    age == 8 ~ "15 months",
    age == 9 ~ "18 months",
    age == 10 ~ "24 months",
    age == 11 ~ "30 months",
    age == 12 ~ "3 years"
  ))
  

well_baby_summary = well_baby_details %>%
  group_by(CaregiverID, Week) %>%
  summarize(
    num_missed = sum(miss, na.rm=T),
    vaccines_missed = sum(vaccine, na.rm = T))

disability = master %>%
  select(CaregiverID, Week, contains("HEALTH.005")) %>%
  filter(
    !is.na(HEALTH.005_1) |
      !is.na(HEALTH.005_2) |
      !is.na(HEALTH.005_3) |
      !is.na(HEALTH.005_4) |
      !is.na(HEALTH.005_5) |
      !is.na(HEALTH.005_6)) %>%
  mutate(
    across(
      everything(), 
      replace_na, 0)) 

data03 = full_join(data03, disability)


# riser -------------------------------------------------------------------

riser = master %>%
  select(CaregiverID, Week, contains("RISER")) %>%
  gather(variable, value, contains("RISER")) %>%
  filter(!is.na(value)) %>%
  mutate(Week = case_when(
    str_detect(variable, "a$") ~ 0,
    TRUE ~ Week),
    variable = str_remove(variable, "..$")) %>% 
  group_by(CaregiverID, Week, variable) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  spread(variable, value)

data03 = data03 %>% full_join(riser)


# CBCL --------------------------------------------------------------------

cbcl = master %>%
  select(CaregiverID, Week, starts_with("CBCL")) %>%
  gather(variable, value, starts_with("CBCL")) %>%
  mutate(
    variable = str_remove(variable, "CBCL."),
   child = case_when(
     str_detect(variable, "001") ~ 1,
     str_detect(variable, "002") ~ 1,
     str_detect(variable, "003") ~ 2,
     str_detect(variable, "004") ~ 2,
     str_detect(variable, "005") ~ 2,
     str_detect(variable, "006") ~ 3,
     str_detect(variable, "007") ~ 3,
     str_detect(variable, "008") ~ 3,
     str_detect(variable, "009") ~ 4,
     str_detect(variable, "010") ~ 4,
     str_detect(variable, "011") ~ 4,
     str_detect(variable, "012") ~ 5,
     str_detect(variable, "013") ~ 5,
     str_detect(variable, "014") ~ 5,
   ),
   measure = case_when(
     str_detect(variable, ".a$") ~ "fussy",
     str_detect(variable, ".b$") ~ "fear",
     variable %in% c("001", "004", "007", "010", "013") ~ "age",
     variable %in% c("003", "006", "009", "012") ~ "another"),
   time = case_when(
     measure == "age" ~ "now",
     measure == "another" ~ "now",
     str_detect(variable, "001") ~ "pre",
     str_detect(variable, "002") ~ "now",
     str_detect(variable, "004") ~ "pre",
     str_detect(variable, "005") ~ "now",
     str_detect(variable, "007") ~ "pre",
     str_detect(variable, "008") ~ "now",
     str_detect(variable, "010") ~ "pre",
     str_detect(variable, "011") ~ "now",
     str_detect(variable, "013") ~ "pre",
     str_detect(variable, "014") ~ "now")) %>%
  select(-variable) %>%
  filter(!is.na(value)) %>%
  spread(measure, value) %>%
  select(-another) %>% # don't need this
  group_by(CaregiverID, child) %>%
  fill(c("age"), .direction = "downup") %>% # fill in age down, then up for each child within each caregiver
  filter(!is.na(age)) %>% # remove any rows that don't have ages
  mutate(Week = ifelse(time == "pre", 0, Week)) %>% # move "pre-covid" responses to week 0
  select(-time) %>% # don't need this any more
  filter(age <= 26)  %>% # select only ages 3 years and younger 
  filter(!is.na(fear) | !is.na(fussy)) %>% # only weeks with data
  group_by(CaregiverID, Week, child) %>%
  filter(row_number() == 1) %>% #select one row per week per child, just in case there are duplicate pre-covid
  ungroup() %>%
  mutate(age = case_when(
    age == 1 ~ "0 months",
    age == 2 ~ "1 month",
    age == 3 ~ "2 months",
    age == 4 ~ "3 months",
    age == 5 ~ "4 months",
    age == 6 ~ "5 months",
    age == 7 ~ "6 months",
    age == 8 ~ "7 months",
    age == 9 ~ "8 months",
    age == 10 ~ "9 months",
    age == 11 ~ "10 months",
    age == 12 ~ "11 months",
    age == 13 ~ "12 months",
    age == 14 ~ "13 months",
    age == 15 ~ "14 months",
    age == 16 ~ "15 months",
    age == 17 ~ "16 months",
    age == 18 ~ "17 months",
    age == 19 ~ "18 months",
    age == 20 ~ "19 months",
    age == 21 ~ "20 months",
    age == 22 ~ "21 months",
    age == 23 ~ "22 months",
    age == 24 ~ "23 months",
    age == 25 ~ "24 months",
    age == 26 ~ "36 months"
  ))

# SWYC --------------------------------------------------------------------

swyc = master %>%
  select(CaregiverID, Week, starts_with("SWYC")) %>%
  gather(variable, value, starts_with("SWYC")) %>%
  mutate(
    variable = str_remove(variable, "SWYC."),
    child = case_when(
      str_detect(variable, "001") ~ 1,
      str_detect(variable, "002") ~ 1,
      str_detect(variable, "003") ~ 2,
      str_detect(variable, "004") ~ 2,
      str_detect(variable, "005") ~ 3,
      str_detect(variable, "006") ~ 3,
      str_detect(variable, "007") ~ 4,
      str_detect(variable, "008") ~ 4),
    measure = case_when(
      str_detect(variable, "a$") ~ "learning",
      str_detect(variable, "b$") ~ "behavior",
      str_detect(variable, "001") ~ "age",
      str_detect(variable, "003") ~ "age",
      str_detect(variable, "005") ~ "age",
      str_detect(variable, "007") ~ "age",
      str_detect(variable, "009") ~ "age",
      str_detect(variable, "002") ~ "another",
      str_detect(variable, "004") ~ "another",
      str_detect(variable, "006") ~ "another",
      str_detect(variable, "008") ~ "another",
    )) %>%
  select(-variable) %>%
  filter(!is.na(value)) %>%
  spread(measure, value) %>%
  select(-another) %>% # don't need this
  group_by(CaregiverID, child) %>%
  fill(c("age"), .direction = "downup") %>% # fill in age down, then up for each child within each caregiver
  filter(!is.na(age)) %>% # remove any rows that don't have ages
  filter(age <= 26)  %>% # select only ages 3 years and younger 
  filter(!is.na(learning) | !is.na(behavior)) %>% # only weeks with data
  group_by(CaregiverID, Week, child) %>%
  filter(row_number() == 1) %>% #select one row per week per child, just in case there are duplicate pre-covid
  ungroup() %>%
  mutate(age = case_when(
    age == 1 ~ "0 months",
    age == 2 ~ "1 month",
    age == 3 ~ "2 months",
    age == 4 ~ "3 months",
    age == 5 ~ "4 months",
    age == 6 ~ "5 months",
    age == 7 ~ "6 months",
    age == 8 ~ "7 months",
    age == 9 ~ "8 months",
    age == 10 ~ "9 months",
    age == 11 ~ "10 months",
    age == 12 ~ "11 months",
    age == 13 ~ "12 months",
    age == 14 ~ "13 months",
    age == 15 ~ "14 months",
    age == 16 ~ "15 months",
    age == 17 ~ "16 months",
    age == 18 ~ "17 months",
    age == 19 ~ "18 months",
    age == 20 ~ "19 months",
    age == 21 ~ "20 months",
    age == 22 ~ "21 months",
    age == 23 ~ "22 months",
    age == 24 ~ "23 months",
    age == 25 ~ "24 months",
    age == 26 ~ "36 months"
  ))

# fill variables collected at baseline ------------------------------------

data03 = data03 %>%
  group_by(CaregiverID) %>%
  fill(all_of(c("has_03", "num_children", "num_03", 
                "insurance", "insurance_type", 
                "HEALTH.005_1", "HEALTH.005_2", "HEALTH.005_3",
                "HEALTH.005_4", "HEALTH.005_5", "HEALTH.005_6")),
       .direction = "downup") %>%
  ungroup()

save(data03, well_baby_details, cbcl, swyc, file = here("../../Data Management R3/R Data/zero_to_three.Rdata"))
