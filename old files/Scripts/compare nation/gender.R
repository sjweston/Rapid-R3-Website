sample_gender = scored_first %>%
  filter(!is.na(gender) & !is.na(region)) %>%
  group_by(gender, region) %>%
  summarize(Count = n()) %>%
  group_by(region) %>%
  mutate(Percent = 100*Count/sum(Count)) %>%
  mutate(group = "RAPID")

pop %>%
  filter(!is.na(region)) %>%
  select(region, TotalPop.Count, Male.Count, Female.Count) %>%
  gather(gender, count, -region) %>%
  group_by(region, gender) %>%
  summarize(count = sum(count)) %>%
  spread(gender, count) %>%
  mutate(Female.Percent = 100*Female.Count/TotalPop.Count,
         Male.Percent = 100*Male.Count/TotalPop.Count,
         Other.Percent = 100*(TotalPop.Count - Female.Count - Male.Count)/TotalPop.Count) %>%
  select(region, contains("Percent")) %>%
  gather("gender", "Percent", contains("Percent")) %>%
  mutate(gender = gsub("\\.Percent", "", gender)) %>%
  mutate(group = "US Population") %>%
  full_join(sample_gender) %>%
  ggplot(aes(x = region, y = Percent, group = group)) +
  geom_bar(aes(fill = group),
           stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Percent, 1), y = Percent + 5), position = position_dodge(1)) +
  facet_grid(.~gender) +
  labs(x = "", fill = "")+
  theme_minimal(base_size = 10)+
  theme(legend.position = "top")