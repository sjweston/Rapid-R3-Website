pop = read_sav(here("../../Data Management R3/CC_Clean Survey Data/00_R3 MasterFile/Archive/Week 7 MasterFile/Population_Estimates.sav"))

pop = pop %>%
  mutate(region = case_when(
    Region == 1 ~ "Northeast",
    Region == 2 ~ "Midwest",
    Region == 3 ~ "South",
    Region == 4 ~ "West"
  ))

scored_first = scored %>%
  filter(Week > 0) %>%
  group_by(CaregiverID) %>%
  filter(Week == min(Week)) %>%
  ungroup() 