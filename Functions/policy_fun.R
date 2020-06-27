moe = function(x){
  sd = sd(x, na.rm=T)
  n = length(which(!is.na(x)))
  sem = sd/sqrt(n)
  cv = qt(p = .025, df = n-1)
  moe = sem*cv
  return(moe)
}

overall_figure = function(data, variable){
  data %>%
    group_by(CaregiverID) %>%
    filter(!is.na({{variable}})) %>%
    filter(Week == max(Week)) %>%
    group_by({{variable}}) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    mutate(Percent = 100*Count/sum(Count)) %>%
    ggplot(aes(x = {{variable}}, y = Percent, fill = {{variable}})) +
    geom_bar(stat = "identity") +
    geom_label(aes(y = max(Percent)*1.3, label = Count), fill = "white") +
    geom_text(aes(y = Percent - min(Percent)*.5, label = round(Percent)), color = "white") +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    theme_pubclean()
}

long_plot = function(data, variable){
  
  long_plotd = data %>%
    filter(!is.na({{variable}})) %>%
    group_by(Week, {{variable}}) %>%
    summarize(Count = n()) %>%
    group_by(Week) %>%
    mutate(Percent = 100*Count/sum(Count)) %>%
    ungroup() %>%
    ggplot(aes(x = Week, 
               y = Percent, 
               color = {{variable}},
               group=1,
               text = paste("Week:", Week, 
                            "\nGroup:", {{variable}},
                            "\nPercent:", round(Percent,2), "\nCount:", Count)
    )) +
    geom_point() + 
    geom_line() +
    scale_color_brewer(palette = "Dark2")+
    labs(x = "Week", 
         y = "Percent by week")+
    theme_pubclean()
  
  ggplotly(long_plotd, tooltip = "text")
}

region_plot = function(data, variable, group = region){
  
  long_plotd = data %>%
    filter(!is.na({{variable}}) & !is.na({{group}})) %>%
    group_by(Week, {{group}}, {{variable}}) %>%
    summarize(Count = n()) %>%
    group_by(Week, {{group}}) %>%
    mutate(Percent = 100*Count/sum(Count)) %>%
    ungroup() %>%
    ggplot(aes(x = Week, 
               y = Percent, 
               color = {{group}},
               group=1,
               text = paste("Week:", Week, 
                            "\nGroup:", {{variable}},
                            "\nPercent:", round(Percent,2), "\nCount:", Count)
    )) +
    geom_point() + 
    geom_line() +
    scale_color_brewer(palette = "Dark2")+
    labs(x = "Week", 
         y = "Percent by week")+
    scale_x_continuous(breaks = unique(data$Week)) +
    theme_pubclean()
  
  long_plotd = long_plotd + facet_grid(rows = vars({{variable}}), scales = "free")
  
  ggplotly(long_plotd, tooltip = "text")
}

bygroup_plot = function(data, variable, group, g.levels = NULL, g.labels = NULL){
  

  long_plotd = data %>%
    dplyr::group_by(CaregiverID) %>%
    filter(!is.na({{variable}}) & !is.na({{group}})) %>%
    filter(Week == max(Week)) %>%
    dplyr::group_by({{variable}}, {{group}}) %>%
    summarize(Count = n()) %>%
    dplyr::group_by({{group}}) %>%
    mutate(Percent = 100*Count/sum(Count)) %>%
    ungroup()
  
  if(!is.null(g.levels)){
    long_plotd = 
      mutate_at(long_plotd, vars({{group}}), .f = function(x) factor(x, 
                                            levels = g.levels, 
                                            labels = g.labels)) }
  
  
  
  long_plotd = long_plotd %>%
    ggplot(aes(x = {{variable}},
               y = Percent,
               fill = {{group}},
               group=1,
               text = paste("\nGroup:", {{group}},
                            "\nResponse:", {{variable}},
                            "\nPercent:", round(Percent,2), "\nCount:", Count))) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    facet_grid(cols = vars({{group}})) +
    theme_pubclean()
  
  

  ggplotly(long_plotd, tooltip = "text") 
  
}

bycont_plot = function(data, variable, outcome){
  
  
  long_plotd = data %>%
    dplyr::group_by(CaregiverID) %>%
    filter(!is.na({{variable}}) & !is.na({{outcome}})) %>%
    filter(Week == max(Week)) %>%
    dplyr::group_by({{variable}}) %>%
    summarize(Average = mean({{outcome}}),
              MOE = moe({{outcome}})) %>%
    ggplot(aes(x = {{variable}},
               y = Average,
               fill = {{variable}},
               group=1,
               text = paste("\nGroup:", {{variable}},
                            "\nAverage Response:", round(Average,2),
                            "\n Lower CI:", round(Average-MOE,2),
                            "\n Upper CI:", round(Average+MOE),2))) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = Average-MOE, ymax = Average + MOE)) +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    theme_pubclean()
  ggplotly(long_plotd, tooltip = "text")
  
}

bcont_table = function(data, variable, outcome){
  formula = as.formula(
    paste0(
      deparse(substitute(outcome)), 
      " ~ ",
      deparse(substitute(variable))
      ))
  
  model = lm(formula, data)
  contrast.form = as.formula(
    paste0(
      "pairwise~ ",
      deparse(substitute(variable))
    ))
  modelmeans = emmeans(model, contrast.form)
  contrast.table = modelmeans$contrast %>%
    as.data.frame() %>%
    #mutate_at(vars(estimate, SE), as.numeric) %>%
    mutate(Lower = estimate-1.96*SE, 
           Upper = estimate+1.96*SE) %>%
    select(contrast, estimate, Lower, Upper, p.value) %>%
    mutate(p.value = papaja::printp(p.value)) %>%
    kable(digits = 2) %>% kable_styling() %>% 
    add_header_above(c(" " = 2, "Confidence Interval" =2 , " " = 1))
  return(contrast.table)
}
