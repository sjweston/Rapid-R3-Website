policy_binary = function(policy, variable, data = scored){
  
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{policy}})) %>%
    group_by(CaregiverID) %>%
    filter(Week == max(Week)) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    group_by({{policy}}) %>%
    summarize(
      count = n(),
      percent = sum({{variable}}))   %>%
    mutate(
      percent = percent/count,
      moe = 1.96*sqrt((percent*(1-percent))/count),
      moe = 100*moe,
      percent = 100*percent)  %>%
    ggplot(aes(x = {{policy}},
               y = percent,
               #group = 1,
               fill = {{policy}},
               text = paste0(
                 "Count: ", count,
                 "\n Percent: ", round(percent)))) +
    geom_bar(stat = "identity", position = position_dodge(1)) +
    geom_errorbar(aes(ymin = percent-moe, ymax = percent + moe),
                  position = position_dodge(1),
                  width = .1) +
    labs(x = NULL, y = "Percent", fill = NULL) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))

  ggplotly(plot, tooltip = "text")
}

binary_group = function(policy, variable, group, data = scored){
  
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    mutate_at(vars({{group}}), 
              .funs = function(x) str_replace(x, ", ", "\\\n")) %>%
    filter(!is.na({{policy}})) %>%
    filter(!is.na({{group}})) %>%
    group_by(CaregiverID) %>%
    filter(Week == max(Week)) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    group_by({{policy}}, {{group}}) %>%
    summarize(
      count = n(),
      percent = sum({{variable}}))   %>%
    mutate(
      percent = percent/count,
      moe = 1.96*sqrt((percent*(1-percent))/count),
      moe = 100*moe,
      percent = 100*percent)  %>%
    ggplot(aes(x = {{group}},
               y = percent,
               #group = 1,
               fill = {{policy}},
               text = paste0(
                 "Count: ", count,
                 "\n Percent: ", round(percent)))) +
    geom_bar(stat = "identity", position = position_dodge(1)) +
    geom_errorbar(aes(ymin = percent-moe, ymax = percent + moe),
                  position = position_dodge(1),
                  width = .1) +
    labs(x = NULL, y = "Percent", fill = NULL) +
    facet_wrap(facets = vars({{group}}), scales = "free_x") +
    scale_x_discrete(labels = NULL)+
    theme_pubr() 
  
  ggplotly(plot, tooltip = "text")
}
time_policy_binary = function(policy, variable, data = scored){
  
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{policy}})) %>%
    group_by(Week, Date, {{policy}}) %>%
    summarize(count = n(),
              percent = 100*sum({{variable}})/count)  %>%
    ungroup()   %>%
    ggplot(aes(x = Date,
               y = percent,
               group = 1,
               color = {{policy}},
               text = paste0(
                 "Date: ", Date,
                 "\n Count: ", count,
                 "\n Percent: ", round(percent)))) +
    geom_point() +
    geom_line() +
    #geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), color = "red") +
    labs(x = NULL, y = "Percent", color = NULL) +
    theme_pubr()
  
  ggplotly(plot, tooltip = "text")
}

time_binary_group = function(policy, variable, group, data = scored){
  
  plot = data %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{group}})) %>%
    filter(!is.na({{policy}})) %>%
    group_by(Week, Date, {{group}}, {{policy}}) %>%
    summarize(count = n(),
              percent = 100*sum({{variable}})/count)  %>%
    ungroup()   %>%
    ggplot(aes(x = Date,
               y = percent,
               group = 1,
               color = {{policy}},
               text = paste0(
                 "Date: ", Date,
                 "\n Count: ", count,
                 "\n Percent: ", round(percent)))) +
    geom_point() +
    geom_line() +
    #geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), color = "red") +
    labs(x = NULL, y = "Percent", color = NULL) +
    facet_wrap(facets = vars({{group}})) + 
    theme_pubr()
  
  ggplotly(plot, tooltip = "text")
}


policy_cont = function(policy, variable, data = scored){
  
  plot = data %>%
    filter(!is.na({{policy}})) %>%
    filter(!is.na({{variable}}))   %>%
    group_by(CaregiverID) %>%
    filter(Week == max(Week)) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    group_by({{policy}}) %>%
    summarize(
      n = n(),
      mean = mean({{variable}}),
      sd = sd({{variable}})) %>%
    mutate(
      moe = qt(.975, df = n-1)*sd/sqrt(n))  %>%
    ggplot(aes(x = {{policy}},
               y = mean,
               #group = 1,
               fill = {{policy}},
               text = paste0(
                 "Count: ", n,
                 "\n Mean: ", round(mean)))) +
    geom_bar(stat = "identity", position = position_dodge(1)) +
    geom_errorbar(aes(ymin = mean-moe, ymax = mean + moe),
                  position = position_dodge(1),
                  width = .1) +
    labs(x = NULL, y = "Mean", fill = NULL) +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))

  ggplotly(plot, tooltip = "text")
}

cont_group = function(policy, variable, group, data = scored){
  
  plot = data %>%
    mutate_at(vars({{group}}), 
              .funs = function(x) str_replace(x, ", ", "\\\n")) %>%
    filter(!is.na({{policy}})) %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{group}})) %>%
    group_by(CaregiverID) %>%
    filter(Week == max(Week)) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    group_by({{policy}}, {{group}}) %>%
    summarize(
      n = n(),
      mean = mean({{variable}}),
      sd = sd({{variable}})) %>%
    mutate(
      moe = qt(.975, df = n-1)*sd/sqrt(n))  %>%
    ggplot(aes(x = {{group}},
               y = mean,
               #group = 1,
               fill = {{policy}},
               text = paste0(
                 "Count: ", n,
                 "\n Mean: ", round(mean)))) +
    geom_bar(stat = "identity", position = position_dodge(1)) +
    geom_errorbar(aes(ymin = mean-moe, ymax = mean + moe),
                  position = position_dodge(1),
                  width = .1) +
    labs(x = NULL, y = "Mean", fill = NULL) +
    facet_wrap(facets = vars({{group}}), scales = "free_x") +
    scale_x_discrete(labels = NULL)+
    theme_pubr() 
  
  ggplotly(plot, tooltip = "text")
}
time_policy_cont = function(policy, variable, data = scored){
  
  plot = data %>%
    filter(!is.na({{policy}})) %>%
    filter(!is.na({{variable}})) %>%
    group_by(Week, Date, {{policy}}) %>%
    summarize(
      n = n(),
      mean = mean({{variable}}),
      sd = sd({{variable}})) %>%
    mutate(
      moe = qt(.975, df = n-1)*sd/sqrt(n))  %>%
    ungroup()   %>%
    ggplot(aes(x = Date,
               y = mean,
               group = 1,
               color = {{policy}},
               text = paste0(
                 "Date: ", Date,
                 "\n Count: ", n,
                 "\n Mean: ", round(mean)))) +
    geom_point() +
    geom_line() +
    #geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), color = "red") +
    labs(x = NULL, y = "Mean", color = NULL) +
    theme_pubr()
  
  ggplotly(plot, tooltip = "text")
}

time_cont_group = function(policy, variable, group, data = scored){
  
  plot = data %>%
    filter(!is.na({{group}})) %>%
    filter(!is.na({{policy}})) %>%
    filter(!is.na({{variable}})) %>%
    group_by(Week, Date, {{group}}, {{policy}}) %>%
    summarize(
      n = n(),
      mean = mean({{variable}}),
      sd = sd({{variable}})) %>%
    mutate(
      moe = qt(.975, df = n-1)*sd/sqrt(n))  %>%
    ggplot(aes(x = Date,
               y = mean,
               group = 1,
               color = {{policy}},
               text = paste0(
                 "Date: ", Date,
                 "\n Count: ", n,
                 "\n Mean: ", round(mean)))) +
    geom_point() +
    geom_line() +
    #geom_vline(aes(xintercept = as.numeric(as.Date("2020-07-31"))), color = "red") +
    labs(x = NULL, y = "Mean", color = NULL) +
    facet_wrap(facets = vars({{group}})) + 
    theme_pubr()
  
  ggplotly(plot, tooltip = "text")
}
