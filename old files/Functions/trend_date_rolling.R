
trend_date_binary = function(variable, dataset, label, groups, group_labels){
  
  demo_data = dataset %>%
    select(CaregiverID, 
           Week, 
           all_of(groups), 
           all_of(variable)) %>%
    rename("variable" = variable) %>%
    gather("demo", "value", all_of(groups)) %>%
    filter(value == 1) %>%
    group_by(demo, Week) %>%
    summarise(total = n(),
              percent = 100*sum(variable, na.rm=T)) %>%
    mutate(percent = percent/total) %>%
    ungroup() %>% 
    mutate(demo = factor(demo, 
                         levels = groups, 
                         labels = group_labels))
  # create NA rows for days that don't exist
  demo_data = expand_grid(
    demo = unique(demo_data$demo),
    Week = seq(from = min(demo_data$Week, 
                                     na.rm = T),
                          to = max(demo_data$Week, 
                                   na.rm=T),
                          by = 1)) %>% 
    full_join(demo_data) %>%
    #create rolling average
    group_by(demo) %>%
    arrange(Week) %>%
    mutate(percent = rollapplyr(percent, FUN = mean, partial = T, width = 5))
  
  all_data = dataset %>%
    select(CaregiverID, Week, variable) %>%
    rename("variable" = variable) %>%
    group_by(Week) %>%
    summarise(total = n(),
              percent = 100*sum(variable, na.rm=T)/total) %>%
    arrange(Week) %>%
    mutate(percent = rollapplyr(percent, FUN = mean, partial = T, width = 5))
  
  plot = demo_data %>%
    ggplot(aes(x = Week, y = percent)) +
    geom_line(aes(color = demo), alpha = .7) +
    geom_line(data = all_data, linetype = "dashed", color = "black") +
    labs(x = "Week",
         y = "Percent of demographic group",
         title = paste0("__ Percent of caregivers report ", label),
         color = "Key demographics", 
         caption = "Entire sample is represented by the black, dashed line.
       Percent represents rolling 5-day average.") +
    theme_minimal() +
    theme(plot.title.position = "plot")
  
  return.list = list(demo_data = demo_data,
                     all_data = all_data,
                     plot = plot)
}


trend_date_cat = function(variable, dataset, label, groups, group_levels, group_labels = NULL){
  group_levels = group_levels[!is.na(group_levels)]
  if(is.null(group_labels)) group_labels = group_levels
  
  groups_data = dataset %>%
    select(CaregiverID, 
           Week, 
           all_of(groups), 
           all_of(variable)) %>%
    rename("variable" = variable,
           "groups" = groups) %>%
    filter(!is.na(groups)) %>%
    group_by(groups, Week) %>%
    summarise(total = n(),
              percent = 100*sum(variable, na.rm=T)/total) %>%
    ungroup() %>% 
    mutate(groups = factor(groups, 
                           levels = group_levels, 
                           labels = group_labels))
  # create NA rows for days that don't exist
  groups_data = expand_grid(
    groups = unique(groups_data$groups),
    Week = seq(from = min(groups_data$Week, 
                                     na.rm = T),
                          to = max(groups_data$Week, 
                                   na.rm=T),
                          by = 1)) %>% 
    full_join(groups_data) %>%
    #create rolling average
    group_by(groups) %>%
    arrange(Week) %>%
    mutate(percent = rollapplyr(percent, FUN = mean, partial = T, width = 5))
  
  all_data = dataset %>%
    select(CaregiverID, Week, variable) %>%
    rename("variable" = variable) %>%
    group_by(Week) %>%
    summarise(total = n(),
              percent = 100*sum(variable, na.rm=T)/total) %>%
    arrange(Week) %>%
    mutate(percent = rollapplyr(percent, FUN = mean, partial = T, width = 5))
  
  plot = groups_data %>%
    ggplot(aes(x = Week, y = percent)) +
    geom_line(aes(color = groups), alpha = .6) +
    geom_line(data = all_data, linetype = "dashed",  color = "black") +
    labs(x = "Week",
         y = "Percent of group",
         title = paste0("__ Percent of caregivers report ", label),
         color = "Key groups", 
         caption = "Entire sample is represented by the black, dashed line.
       Percent represents rolling 5-day average.") +
    theme_minimal() +
    theme(plot.title.position = "plot")
  
  return.list = list(groups_data = groups_data,
                     all_data = all_data,
                     plot = plot)
}
