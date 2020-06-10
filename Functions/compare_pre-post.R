pre_post_overall = function(data, outcome){
  
  newdata = data %>%
    select({{outcome}}, Week) 
  
  outcome.label = deparse(substitute(outcome))
  outcome.label = str_to_title(gsub(".+_", "", outcome.label))
  
  plot = ggbarplot(newdata, 
                   x = "Week", 
                   y = deparse(substitute(outcome)), 
                   fill = "Week", 
                   add = "mean_ci",
                   position = position_dodge(), 
                   label = TRUE, 
                   lab.nb.digits = 2, 
                   lab.vjust = 3,
                   xlab = "", 
                   ylab = outcome.label)
  
  newdata = newdata %>%
    rename(outcome = {{outcome}})
  
  pairwise = pairwise.t.test(newdata$outcome, newdata$Week, p.adjust.method = "holm") %>%
    broom::tidy() %>%
    mutate(p.value = papaja::printp(p.value)) %>%
    kable(digits = 2) %>%
    kable_styling()
  
  
  return(list(plot = plot, 
              pairwise = pairwise))
}

pre_post_group = function(data, group, outcome){
  
  newdata = data %>%
    select({{group}}, {{outcome}}, Week) %>%
    filter(!is.na({{group}}))
  
  outcome.label = deparse(substitute(outcome))
  outcome.label = str_to_title(gsub(".+_", "", outcome.label))
  
  plot = ggbarplot(newdata, 
                   x = deparse(substitute(group)), 
                   y = deparse(substitute(outcome)), 
                   fill = "Week", 
                   add = "mean_ci",
                   position = position_dodge(), 
                   label = TRUE, 
                   lab.nb.digits = 2, 
                   lab.vjust = 3,
                   xlab = "", 
                   ylab = outcome.label)
  
  newdata = newdata %>%
    unite(group, {{group}}, Week) %>%
    rename(outcome = {{outcome}})
  
  pairwise = pairwise.t.test(newdata$outcome, newdata$group, p.adjust.method = "holm") %>%
    broom::tidy() %>%
    mutate(p.value = papaja::printp(p.value)) %>%
    separate(group1, into = c("group1_group", "group1_time"), sep = "_")%>%
    separate(group2, into = c("group2_group", "group2_time"), sep = "_") %>%
    filter(group1_group == group2_group | group1_time == group2_time) %>%
    unite(group1, group1_group, group1_time) %>%
    unite(group2, group2_group, group2_time)  %>%
    kable(digits = 2) %>%
    kable_styling()
  
  
  return(list(plot = plot, 
              pairwise = pairwise))
}

change_overall = function(data, outcome){
  
  newdata = data %>%
    select({{outcome}}) 
  
  outcome.label = deparse(substitute(outcome))
  outcome.label = str_to_title(gsub(".+_", "", outcome.label))
  
  plot = gghistogram(newdata, 
                   x = deparse(substitute(outcome)), 
                   add = "mean",
                   add.params = list(color = "red"),
                   color = "white",
                   fill = "grey",
                   ylab = "Count", 
                   xlab = paste("Change in", outcome.label))
  
  newdata = newdata %>%
    rename(outcome = {{outcome}})
  
  ttest = t.test(newdata$outcome) %>%
    broom::tidy() %>%
    mutate(p.value = papaja::printp(p.value)) %>%
    kable(digits = 2) %>%
    kable_styling()
  
  return(list(plot = plot,
              ttest = ttest))
}

change_group = function(data, group, outcome){
  
  newdata = data %>%
    select({{group}}, {{outcome}}) %>%
    filter(!is.na({{group}}))
  
  outcome.label = deparse(substitute(outcome))
  outcome.label = str_to_title(gsub(".+_", "", outcome.label))
  
  plot = ggbarplot(newdata, 
                   x = deparse(substitute(group)), 
                   y = deparse(substitute(outcome)), 
                   fill = deparse(substitute(group)), 
                   add = "mean_ci",
                   position = position_dodge(), 
                   label = TRUE, 
                   lab.nb.digits = 2, 
                   lab.vjust = 3,
                   xlab = "", 
                   ylab = paste("Change in", outcome.label))
  
  newdata = newdata %>%
    rename(outcome = {{outcome}},
           group = {{group}})
  
  ttest = t.test(outcome~group, data = newdata) %>%
    broom::tidy() %>%
    mutate(p.value = papaja::printp(p.value)) %>%
    kable(digits = 2) %>%
    kable_styling()
  
  return(list(plot = plot,
              ttest = ttest))
}