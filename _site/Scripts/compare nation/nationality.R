nation = c(
  "native",
  "asian",
  "black",
  "hawaii",
  "white",
  "other",
  "multiple")

nation_labels = c(
  "American Indian/Alaska Native",
  "Asian",
  "Black/African American",
  "Native Hawaiian/Pacific Islander",
  "White/Caucasian",
  "Other race",
  "Multiple races")

nation_labels_breaks = c(
  "American Indian/\nAlaska Native",
  "Asian",
  "Black/\nAfrican American",
  "Native Hawaiian/\nPacific Islander",
  "White\nCaucasian",
  "Other\nrace",
  "Multiple\nraces")


sample_nation = scored_first %>%
  filter(!is.na(race_cat))%>%
  group_by(race_cat) %>%
  summarize(Count = n()) %>%
  mutate(Percent = 100*Count/sum(Count)) %>%
  mutate(group = "RAPID")

pop %>%
  select(TotalPop.Count, 
         (contains("Race") & contains("Count"))) %>%
  gather(demo, count) %>%
  group_by(demo) %>%
  summarize(count = sum(count)) %>%
  spread(demo, count) %>%
  mutate(native.Percent = 100*Race.Amer.Ind.Count/TotalPop.Count,
         asian.Percent = 100*Race.Asian.Count/TotalPop.Count,
         black.Percent = 100*Race.Black.Count/TotalPop.Count,
         hawaii.Percent = 100*Race.Nat.HI.Count/TotalPop.Count,
         white.Percent = 100*Race.White.Count/TotalPop.Count,
         other.Percent = 100*Race.Other.Count/TotalPop.Count,
  ) %>%
  gather("demo", "Percent", contains("Percent")) %>%
  mutate(demo = gsub("\\.Percent", "", demo),
         race_cat = factor(demo, levels = nation, labels = nation_labels)) %>%
  select(race_cat, Percent) %>%
  mutate(group = "US Population") %>%
  full_join(sample_nation) %>%
  mutate(race_cat = factor(race_cat, 
                           levels = nation_labels, 
                           labels = nation_labels_breaks)) %>%
  ggplot(aes(x = race_cat, y = Percent, group = group)) +
  geom_bar(aes(fill = group),
           stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Percent, 1), y = Percent + 5), position = position_dodge(1)) +
  labs(x = "", fill = "")+
  theme_minimal(base_size = 10)+
  theme(legend.position = "top")