
sample_nation = scored_first %>%
  filter(!is.na(race_cat) & !is.na(region)) %>%
  group_by(race_cat, region) %>%
  summarize(Count = n()) %>%
  group_by(region) %>%
  mutate(Percent = 100*Count/sum(Count)) %>%
  mutate(group = "RAPID")

pop %>%
  filter(!is.na(region)) %>%
  select(region, TotalPop.Count, 
         (contains("Race") & contains("Count"))) %>%
  gather(demo, count, -region) %>%
  group_by(region, demo) %>%
  summarize(count = sum(count)) %>%
  spread(demo, count) %>%
  mutate(native.Percent = 100*Race.Amer.Ind.Count/TotalPop.Count,
         asian.Percent = 100*Race.Asian.Count/TotalPop.Count,
         black.Percent = 100*Race.Black.Count/TotalPop.Count,
         hawaii.Percent = 100*Race.Nat.HI.Count/TotalPop.Count,
         white.Percent = 100*Race.White.Count/TotalPop.Count,
         other.Percent =100*Race.Other.Count/TotalPop.Count
  ) %>%
  select(region, contains("Percent")) %>%
  gather("demo", "Percent", contains("Percent")) %>%
  mutate(demo = gsub("\\.Percent", "", demo),
         race_cat = factor(demo, 
                           levels = nation, 
                           labels = nation_labels)) %>%
  mutate(group = "US Population") %>%
  full_join(sample_nation) %>%
  mutate(race_cat = factor(race_cat, levels = nation_labels, 
                           labels = nation_labels_breaks)) %>%
  filter(!is.na(region)) %>%
  ggplot(aes(x = race_cat, y = Percent, group = group)) +
  geom_bar(aes(fill = group),
           stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Percent, 1), y = Percent + 5), 
            position = position_dodge(1), size= 2) +
  facet_wrap(~region, scales = "free") +
  labs(x = "", fill = "")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_minimal(base_size = 8)+
  theme(legend.position = "top") 