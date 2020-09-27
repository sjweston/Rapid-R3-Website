moe.p = function(p, n){
  q = 1-p
  moe = 1.96*sqrt((p*q)/n)
  return(moe)
}

pomp = function(raw){
  min = min(raw, na.rm=T)
  max = max(raw, na.rm=T)
  new = 100*(raw - min)/(max - min)
  return(new)
}

dist_cat = function(variable, data = scored){
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    group_by(CaregiverID) %>%
    filter(Week == max(Week)) %>%
    ungroup() %>%
    group_by({{variable}}) %>%
    summarize(n = n()) %>%
    mutate(percent = 100*n/sum(n)) %>%
    ggplot(aes(x = {{variable}}, 
               y = percent, 
               fill = percent,
               text = paste0(round(percent,1), "%\nCount:",n))) +
    geom_bar(stat = "identity") +
    guides(fill = F) +
    labs(x = NULL, y = NULL) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
}

dist_cat_group = function(variable, group, data = scored){
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{group}})) %>%
    group_by(CaregiverID) %>%
    filter(Week == max(Week)) %>%
    ungroup() %>%
    group_by({{variable}}, {{group}}) %>%
    summarize(n = n()) %>%
    group_by({{group}}) %>%
    mutate(percent = 100*n/sum(n)) %>%
    ggplot(aes(x = {{variable}}, 
               y = percent, 
               fill = {{group}},
               text = paste0(round(percent,1), "%\nCount:",n))) +
    geom_bar(stat = "identity", position = "dodge") +
    guides(fill = F) +
    labs(x = NULL, y = NULL) +
    theme_pubr()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
}

dist_cat_group2 = function(variable, group1, group2, order = FALSE, data = scored){
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{group1}})) %>%
    filter(!is.na({{group2}})) %>%
    group_by(CaregiverID) %>%
    filter(Week == max(Week)) %>%
    ungroup() %>%
    group_by({{variable}}, {{group1}}, {{group2}}) %>%
    summarize(n = n()) %>%
    group_by({{group1}}, {{group2}}) %>%
    mutate(percent = 100*n/sum(n)) %>%
    ungroup()
  
  if(order){
    plot = plot %>%
      arrange({{group1}}, {{group2}}, desc(percent)) %>%
      mutate(order = row_number())
    
    names(plot)[names(plot) == deparse(substitute(variable))] <- "variable"
    
    plot = plot %>% ggplot(aes(x = order, 
                               y = percent, 
                               fill = {{group1}},
                               text = paste0(round(percent,1), "%\nCount:",n))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_continuous(breaks = plot$order, labels = plot$variable) +
      coord_flip() +
      facet_wrap(facets = vars({{group2}}), scales = "free_y")
  } else{
    plot = plot %>% ggplot(aes(x = {{variable}}, 
                               y = percent, 
                               fill = {{group1}},
                               text = paste0(round(percent,1), "%\nCount:",n))) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(facets = vars({{group2}}))
  }
  plot = plot +
    guides(fill = F) +
    labs(x = NULL, y = NULL) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
}

time_cat = function(variable, data = scored){
  data = data %>%
    group_by(Week) %>%
    mutate(Date = case_when(
      Week == 0 ~ as.Date("2020-03-01"),
      TRUE ~ min(Date)
    )) %>%
    ungroup()
  
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    group_by({{variable}}, Date) %>%
    summarize(n = n()) %>%
    group_by(Date) %>%
    mutate(percent = 100*n/sum(n)) %>%
    ggplot(aes(x = Date, 
               y = percent, 
               group = 1,
               color = {{variable}},
               text = paste0(round(percent,1), "%\nCount:",n))) +
    geom_line() +
    geom_point() +
    guides(fill = F) +
    labs(x = NULL, y = NULL) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
}

time_cat_group = function(variable, group, data = scored){
  
  data = data %>%
    group_by(Week) %>%
    mutate(Date = case_when(
      Week == 0 ~ as.Date("2020-03-01"),
      TRUE ~ min(Date)
    )) %>%
    ungroup()
  
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{group}})) %>%
    group_by(Date, {{variable}}, {{group}}) %>%
    summarize(n = n()) %>%
    group_by(Date, {{group}}) %>%
    mutate(percent = 100*n/sum(n)) %>%
    ggplot(aes(x = Date, 
               y = percent, 
               group = 1, 
               color = {{variable}},
               text = paste0(round(percent,1), "%\nCount:",n))) +
    geom_line() + 
    geom_point() +
    guides(color = F) +
    labs(x = NULL, y = NULL) +
    facet_wrap(facets = vars({{group}})) +
    theme_pubr()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
}

time_cat_group2 = function(variable, group1, group2, data = scored){
  
  data = data %>%
    group_by(Week) %>%
    mutate(Date = case_when(
      Week == 0 ~ as.Date("2020-03-01"),
      TRUE ~ min(Date)
    )) %>%
    ungroup()
  
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{group1}})) %>%
    filter(!is.na({{group2}})) %>%
    group_by(Date, {{variable}}, {{group1}}, {{group2}}) %>%
    summarize(n = n()) %>%
    group_by(Date, {{group1}}, {{group2}}) %>%
    mutate(percent = 100*n/sum(n)) %>%
    ggplot(aes(x = Date, 
               y = percent, 
               group = 1, 
               color = {{variable}},
               text = paste0(round(percent,1), "%\nCount:",n))) +
    geom_line() + 
    geom_point() +
    guides(color = F) +
    labs(x = NULL, y = NULL) +
    facet_wrap(facets = vars({{group1}}, {{group2}})) +
    theme_pubr()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
  
}

count_by_week = function(variable, label, data = scored){
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(Week > 0) %>%
    group_by(Week) %>%
    summarize(Date = min(Date),
              n = sum({{variable}})) %>%
    ggplot(aes(x = Date, y = n)) +
    geom_point() +
    geom_line() +
    labs(x = NULL, y = "Number of caregivers",
         title = paste("Number of", str_remove(label, " caregivers"), "caregivers by week")) +
    theme_pubr()
  ggplotly(plot)
}

average_by_group = function(variable, group, label, data = scored){
  plot = data %>%
    filter(!is.na({{group}}) & !is.na({{variable}})) %>%
    ggplot(aes(x = {{group}}, 
               y = {{variable}})) +
    stat_summary(fun = "mean", geom = "point") +
    geom_errorbar(stat="summary", 
                  fun.data="mean_se", 
                  fun.args = list(mult = 1.96),
                  width = .4) +
    labs(x = NULL, y = NULL, title = paste("Average", label, "by group")) +
    theme_pubr()
  ggplotly(plot)
}

average_by_group2 = function(variable, group1, group2, label, data = scored){
  plot = data %>%
    filter(!is.na({{group1}}) & !is.na({{group2}}) & !is.na({{variable}})) %>%
    ggplot(aes(x = {{group1}}, 
               y = {{variable}},
               color = {{group2}})) +
    stat_summary(fun = "mean", 
                 geom = "point", 
                 position = position_dodge(1)) +
    geom_errorbar(stat="summary", 
                  fun.data="mean_se", 
                  fun.args = list(mult = 1.96),
                  width = .4,
                  position = position_dodge(1)) +
    labs(x = NULL, y = NULL, title = paste("Average", label, "by group")) +
    theme_pubr()
  ggplotly(plot)
}

average_by_week = function(variable, group, label, data = scored){
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{group}})) %>%
    filter(Week > 0) %>%
    group_by(Week, {{group}}) %>%
    summarize(Date = min(Date),
              n = n(),
              m = mean({{variable}}),
              s = sd({{variable}})) %>%
    ungroup() %>%
    mutate(moe = s/sqrt(n)*qt(.975,n-1)) %>%
    ggplot(aes(x = Date, 
               y = m, 
               color = {{group}},
               group = 1,
               text = paste0(
                 "N: ", n,
                 "\nMean:", round(m, 2)
               ))) +
    geom_point() +
    geom_errorbar(aes(
      ymin = m-moe, 
      ymax = m+moe), alpha = .5) +
    geom_line(alpha = .5)+
    labs(x = NULL, y = NULL,
         title = paste("Average", label, "by week")) +
    theme_pubr()
  ggplotly(plot)
}

percent_by_group = function(variable, group, data = scored){
  
  plot = data %>%
    filter(!is.na({{group}}) & !is.na({{variable}})) %>%
    group_by({{group}}) %>%
    summarize(n = n(),
              count = sum({{variable}}))  %>%
    ungroup() %>%
    mutate(percent = count/n,
           moe = map2_dbl(percent, n, moe.p)) %>%
    mutate_at(vars(percent, moe), ~100*.x) %>%
    ggplot(aes(x = {{group}},
               y = percent,
               fill = {{group}},
               text = paste0(round(percent,1), "%\nCount:",n))) +
    geom_bar(stat = "identity",
             position = position_dodge(width = .5))   +
    geom_errorbar(aes(
      ymin = percent - moe,
      ymax = percent + moe),
      position = position_dodge(width = .5),
      width = .3) +
    guides(fill = F)+
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
  
}

percent_by_group2 = function(variable, group1, group2, data = scored){
  
  plot = data %>%
    filter(!is.na({{group1}}) & !is.na({{group2}})  & !is.na({{variable}})) %>%
    group_by({{group1}}, {{group2}}) %>%
    summarize(n = n(),
              count = sum({{variable}}))  %>%
    ungroup() %>%
    mutate(percent = count/n,
           moe = map2_dbl(percent, n, moe.p)) %>%
    mutate_at(vars(percent, moe), ~100*.x) %>%
    ggplot(aes(x = {{group1}},
               y = percent,
               fill = {{group2}},
               text = paste0(round(percent,1), "%\nCount:",n))) +
    geom_bar(stat = "identity",
             position = position_dodge(1))   +
    geom_errorbar(aes(
      ymin = percent - moe,
      ymax = percent + moe),
      position = position_dodge(1),
      width = .3) +
    guides(fill = F)+
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
  
}

percent_by_week = function(variable, group, label, data = scored){
  
  plot = data %>%
    filter(!is.na({{group}}) & !is.na({{variable}})) %>%
    group_by({{group}}, Week) %>%
    summarize(
      Date = min(Date),
      n = n(),
      count = sum({{variable}}))  %>%
    ungroup() %>%
    mutate(percent = count/n,
           moe = map2_dbl(percent, n, moe.p)) %>%
    mutate_at(vars(percent, moe), ~100*.x) %>%
    ggplot(aes(x = Date,
               y = percent,
               group = 1,
               color = {{group}},
               text = paste0(round(percent,1), "%\nCount:",n))) +
    geom_point() +
    geom_errorbar(aes(
      ymin = percent - moe,
      ymax = percent + moe),
      width = .3, alpha = .5) +
    geom_line(alpha = .5)+
    labs(x = NULL, y = NULL, color = NULL) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  ggplotly(plot, tooltip = "text")
}
