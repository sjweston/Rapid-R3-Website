trend_week_binary = function(variable, dataset, label, groups, group_labels){
  dataset %>%
    rename("variable" = variable,
           "groups" = groups) %>%
  filter(!is.na(groups)) %>%
    select(variable, groups, Week) %>%
    group_by(Week, groups) %>%
    summarize(m = mean(variable, na.rm=T),
              n = n(),
              sd = sd(variable, na.rm=T),
              se = sd/sqrt(n),
              cv = qt(p = .975, df = n-1),
              moe = se*cv) %>%
    ggplot(aes(x = Week, y = m, color = as.factor(groups))) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = m-moe, ymax = m+moe), width = .5, alpha = .5) +
    labs(x = "Week", y = label) +
    scale_color_discrete("Group", labels = group_labels) +
    theme_minimal() %>%
    return()
}

  