
sample_nation = scored_first %>%
  filter(!is.na(latinx) & !is.na(region)) %>%
  group_by(latinx, region) %>%
  summarize(Count = n()) %>%
  group_by(region) %>%
  mutate(Percent = 100*Count/sum(Count)) %>%
  mutate(group = "RAPID") %>%
  filter(latinx == 1)

pop %>%
  filter(!is.na(region)) %>%
  select(region, TotalPop.Count, 
         (contains("Race") & contains("Count"))) %>%
  gather(demo, count, -region) %>%
  group_by(region, demo) %>%
  summarize(count = sum(count)) %>%
  spread(demo, count) %>%
  mutate(Percent = 100*Race.Hispanic.Count/TotalPop.Count) %>%
  select(region, contains("Percent")) %>%
  mutate(group = "US Population") %>%
  full_join(sample_nation) %>%
  ggplot(aes(x = region, y = Percent, group = group)) +
  geom_bar(aes(fill = group),
           stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Percent, 1), y = Percent + 2), 
            position = position_dodge(1), size= 2) +
  labs(x = "", fill = "", 
       title = "Representation of Hispanic/Latinx Caregivers by region")+
  theme_minimal(base_size = 8)+
  theme(legend.position = "top") 