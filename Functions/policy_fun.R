moe = function(x){
  sd = sd(x, na.rm=T)
  n = length(which(!is.na(x)))
  sem = sd/sqrt(n)
  cv = qt(p = .025, df = n-1)
  moe = sem*cv
  return(moe)
}

moe.p = function(p,n){
  sqrt(p*(1 - p) / n)
}


overall_figure = function(data, variable){
  
  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
  
  overallp = data %>%
    group_by(CaregiverID) %>%
    filter(!is.na({{variable}})) %>%
    filter(Week == max(Week)) %>%
    group_by({{variable}}) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    mutate(Percent = 100*Count/sum(Count)) %>%
    ggplot(aes(x = {{variable}}, 
               y = Percent, 
               fill = {{variable}},
               group=1,
               text = paste("\nResponse:", {{variable}},
                            "\nPercent:", round(Percent,2), 
                            "\nCount:", Count))) +
    geom_bar(stat = "identity") +
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    theme_pubclean()
  
  if(check_class != "numeric") overallp = overallp + scale_fill_brewer(palette = "Dark2")
  
  ggplotly(overallp, tooltip = "text")
}
overall_c_figure = function(data, variables, labels){
  
  n_caregivers = length(unique(data$CaregiverID))
  
  overallp = data %>%
    group_by(CaregiverID) %>%
    filter(Week == max(Week)) %>%
    ungroup() %>%
    summarize_at(.vars = variables, .funs = list(sum), na.rm=T) %>%
    gather("key", "Count") %>%
    mutate(Percent = 100*Count/n_caregivers,
           key = factor(key, 
                        levels = variables,
                        labels = labels)) %>%
    ggplot(aes(x = reorder(key,Percent), 
               y = Percent, 
               fill = key, 
               group=1,
               text = paste("\nResponse:", key,
                            "\nPercent:", round(Percent,2), 
                            "\nCount:", Count))) +
    geom_bar(stat = "identity") +
    #scale_fill_brewer(palette = "Set2") +
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    theme_pubclean()
  
  ggplotly(overallp, tooltip = "text")
}

overall_m_figure = function(data, variable, moderator, m.val, m.label){
  
  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
  
  overallp = data %>%
    group_by(CaregiverID) %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{moderator}})) %>%
    filter(Week == max(Week)) %>%
    mutate_at(.vars = vars({{moderator}}), 
              .funs = function(x) factor(x, levels = m.val, labels = m.label)) %>%
    group_by({{variable}}, {{moderator}}) %>%
    summarize(Count = n()) %>%
    group_by({{moderator}}) %>%
    mutate(Percent = 100*Count/sum(Count),
           MOE = moe.p(Percent/100, sum(Count))) %>%
    ungroup() %>%
    ggplot(aes(text = paste("Group:", {{moderator}},
                            "\nResponse:", {{variable}},
                            "\nPercent:", round(Percent,2),
                            "\nCount:", Count),
               x = {{variable}},
               y = Percent,
               fill = {{moderator}})) +
    geom_bar(stat = "identity", position=position_dodge(1))   +
    geom_errorbar(aes(ymin = Percent-(100*MOE), ymax = Percent + (100*MOE)),
                  position=position_dodge(1)) +
    labs(x = "")+
    #facet_grid(rows = vars({{variable}})) +
    guides(fill = F) +
    coord_flip()+
    theme_pubclean()

  if(check_class != "numeric") overallp = overallp + scale_fill_brewer(palette = "Dark2")

  ggplotly(overallp, tooltip = "text")
  #return(overallp)
}

long_plot = function(data, variable){
  
  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
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
    scale_x_continuous(breaks = unique(data$Week)) +
    labs(x = "Week", 
         y = "Percent by week")+
    theme_pubclean()
  
  if(check_class != "numeric") long_plotd = long_plotd + scale_fill_brewer(palette = "Dark2")
  
  
  ggplotly(long_plotd, tooltip = "text")
}

long_m_plot = function(data, variable, moderator,m.val, m.label){
  
  check_class = as.data.frame(data[,deparse(substitute(moderator))])
  check_class = class(check_class[,1])
  
  long_plotd = data %>%
    mutate_at(.vars = vars({{moderator}}), 
              .funs = function(x) factor(x, levels = m.val, labels = m.label)) %>%
    filter(!is.na({{variable}})) %>%
    filter(!is.na({{moderator}}))  %>%
    group_by(Week, {{variable}}, {{moderator}}) %>%
    summarize(Count = n()) %>%
    group_by(Week, {{moderator}}) %>%
    mutate(Percent = 100*Count/sum(Count),
           MOE = moe.p(Percent/100, sum(Count))) %>%
    ungroup()  %>%
    ggplot(aes(x = Week,
               y = Percent,
               color = {{moderator}},
               text = paste("Week:", Week,
                            "\nGroup:", {{moderator}},
                            "\nResponse:", {{variable}},
                            "\nPercent:", round(Percent,2), "\nCount:", Count)
    )) +
    geom_point() +
    geom_line(aes(x = Week,
                  y = Percent,
                  color = {{moderator}}), inherit.aes = F) +
    scale_x_continuous(breaks = unique(data$Week)) +
    labs(x = "Week",
         y = "Percent by week")+
    facet_grid(rows = vars({{variable}}))+
    theme_pubclean()

  long_plotd = long_plotd + scale_color_brewer(palette = "Dark2")

  ggplotly(long_plotd, tooltip = "text")
#  return(long_plotd)
}

region_plot = function(data, variable, group = region){
  
  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
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
    labs(x = "Week", 
         y = "Percent by week")+
    scale_x_continuous(breaks = unique(data$Week)) +
    theme_pubclean()
  
  if(check_class != "numeric") long_plotd = long_plotd + scale_color_brewer(palette = "Dark2")
  
  
  long_plotd = long_plotd + facet_grid(rows = vars({{variable}}), scales = "free")
  
  ggplotly(long_plotd, tooltip = "text")
}

region_co_plot = function(data, variable, group = region){
  
  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
  long_plotd = data %>%
    filter(!is.na({{variable}}) & !is.na({{group}})) %>%
    group_by(Week, {{group}}) %>%
    summarize(Average = mean({{variable}}),
              MOE = moe({{variable}}),
              N = n()) %>%
    ungroup() %>%
    ggplot(aes(x = Week, 
               y = Average, 
               color = {{group}},
               group=1,
               text = paste("Week:", Week, 
                            "\nGroup:", {{group}},
                            "\nAverage:", round(Average,2), 
                            "\nCI Lower:", round(Average-MOE,2), 
                            "\nCI Upper:", round(Average+MOE,2), 
                            "\nN:", N)
    )) +
    geom_point() + 
    geom_line() +
    labs(x = "Week", 
         y = "Response by week")+
    scale_x_continuous(breaks = unique(data$Week)) +
    theme_pubclean()
  
  if(check_class != "numeric") long_plotd = long_plotd + scale_color_brewer(palette = "Dark2")
  
  ggplotly(long_plotd, tooltip = "text")
}

bygroup_plot = function(data, variable, group, g.levels = NULL, g.labels = NULL){

  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
  number_outcome = nrow(unique(data[,deparse(substitute(variable))]))
  
  
  long_plotd = data %>%
    dplyr::group_by(CaregiverID) %>%
    filter(!is.na({{variable}}) & !is.na({{group}})) %>%
    filter(Week == max(Week)) %>%
    dplyr::group_by({{variable}}, {{group}}) %>%
    summarize(Count = n()) %>%
    dplyr::group_by({{group}}) %>%
    mutate(Percent = 100*Count/sum(Count),
           p = Count/sum(Count),
           se = sqrt((p*(1-p))/sum(Count)),
           moe = 100*1.96*se) %>%
    ungroup()
  
  if(!is.null(g.levels)){
    long_plotd = 
      mutate_at(long_plotd, vars({{group}}), .f = function(x) factor(x, 
                                            levels = g.levels, 
                                            labels = g.labels)) }
  
  
  
  long_plotd = long_plotd %>%
    ggplot(aes(x = {{group}},
               y = Percent,
               fill = {{variable}},
               group=1,
               text = paste("\nGroup:", {{group}},
                            "\nResponse:", {{variable}},
                            "\nPercent:", round(Percent,2), "\nCount:", Count))) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = Percent-moe, ymax = Percent+moe), width = .5)+
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    theme_pubclean()
  
  if(check_class != "numeric") long_plotd = long_plotd + scale_fill_brewer(palette = "Dark2")
  if(number_outcome < 4) long_plotd = long_plotd + facet_grid(cols = vars({{variable}}), scales = "free") 
  if(number_outcome >= 4) long_plotd = long_plotd + facet_grid(rows = vars({{variable}}), scales = "free") 
  

  ggplotly(long_plotd, tooltip = "text") 
  
}

bygroup_ttest = function(data, variable, group){
  
  long_plotd = data %>%
    dplyr::group_by(CaregiverID) %>%
    filter(!is.na({{variable}}) & !is.na({{group}})) %>%
    filter(Week == max(Week)) %>%
    dplyr::group_by({{variable}}, {{group}}) %>%
    summarize(Count = n()) %>%
    dplyr::group_by({{group}}) %>%
    mutate(Percent = 100*Count/sum(Count),
           Total = sum(Count)) %>%
    group_by({{variable}}) %>%
    nest() %>%
    mutate(prop = map(data, .f = function(x)
      prop.test(x$Count, x$Total))) %>%
    mutate(prop = map(prop, broom::tidy)) %>%
    unnest(prop) %>%
    select({{variable}}, statistic, p.value) %>% 
    mutate(p.value = papaja::printp(p.value)) %>%
    kable(digits = 2) %>%
    kable_styling(full_width = T)
  
  return(long_plotd)
  #test = prop.test(x = long_plotd$Count)
  

 
    
}

bycont_plot = function(data, variable, outcome){
  
  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
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
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    theme_pubclean()
  
  if(check_class != "numeric") long_plotd = long_plotd + scale_fill_brewer(palette = "Dark2")
  
  ggplotly(long_plotd, tooltip = "text")
  
  
}

bycont_m_plot = function(data, variable, outcome, moderator, m.val, m.label){
  
  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
  long_plotd = data %>%
    dplyr::group_by(CaregiverID) %>%
    mutate_at(.vars = vars({{moderator}}), 
              .funs = function(x) factor(x, levels = m.val, labels = m.label)) %>%
    filter(!is.na({{variable}}) & !is.na({{outcome}} & !is.na({{moderator}}))) %>%
    filter(Week == max(Week)) %>%
    dplyr::group_by({{variable}}, {{moderator}}) %>%
    summarize(Average = mean({{outcome}}),
              MOE = moe({{outcome}})) %>%
    ggplot(aes(x = 1,
               y = Average,
               fill = {{moderator}},
               text = paste("\nGroup:", {{variable}},
                            "\nModerator:", {{moderator}},
                            "\nAverage Response:", round(Average,2),
                            "\n Lower CI:", round(Average-MOE,2),
                            "\n Upper CI:", round(Average+MOE),2))) +
    geom_bar(stat = "identity", position = position_dodge(1)) +
    geom_errorbar(aes(ymin = Average-MOE, ymax = Average + MOE), position = position_dodge(1)) +
    scale_x_continuous(labels = NULL)+
    labs(x = "")+
    guides(fill = F) +
    facet_grid(rows = vars({{variable}}), scales = "free")+
    coord_flip()+
    theme_pubclean()
  
  if(check_class != "numeric") long_plotd = long_plotd + scale_fill_brewer(palette = "Dark2")
  
  ggplotly(long_plotd, tooltip = "text")
  
  
}

bcont_table = function(data, variable, outcome){
  
  check_class = as.data.frame(data[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
  formula = as.formula(
    paste0(
      deparse(substitute(outcome)), 
      " ~ ",
      deparse(substitute(variable))
      ))
  
  model = lm(formula, data)
  
  if(check_class != "numeric"){
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
      filter(!is.nan(p.value)) %>%
      filter(!is.na(p.value)) %>%
      mutate(p.value = papaja::printp(p.value)) %>%
      kable(digits = 2) %>% kable_styling() %>%
      add_header_above(c(" " = 2, "Confidence Interval" =2 , " " = 1))
  }
  if(check_class == "numeric"){
    contrast.table = broom::tidy(model) %>%
      mutate(Lower = estimate-1.96*std.error,
             Upper = estimate+1.96*std.error) %>%
      filter(term != "(Intercept)") %>%
      select(estimate, Lower, Upper, p.value) %>%
      mutate(p.value = papaja::printp(p.value)) %>%
      kable(digits = 2) %>% kable_styling() %>%
      add_header_above(c(" " = 1, "Confidence Interval" =2 , " " = 1))
  }
  return(contrast.table)
}

bcont_m_table = function(ndata, variable, outcome, moderator, m.val, m.label){
  
  check_class = as.data.frame(ndata[,deparse(substitute(variable))])
  check_class = class(check_class[,1])
  
  formula = as.formula(
    paste0(
      deparse(substitute(outcome)), 
      " ~ ",
      deparse(substitute(moderator))
    ))
  
  contrast.table = ndata %>%
    mutate_at(.vars = vars({{moderator}}), 
              .funs = function(x) factor(x, levels = m.val, labels = m.label)) %>%
    group_by({{variable}}) %>%
    nest() %>%
    mutate(model = map(data, .f = function(x) lm(formula, data = x))) %>%
    mutate(model = map(model, broom::tidy)) %>%
    unnest(model) %>%
    select(-data) %>%
    filter(term != "(Intercept)") %>%
    mutate(Lower = estimate-1.96*std.error,
           Upper = estimate+1.96*std.error) %>%
    ungroup() %>%
    mutate(p.value = p.adjust(p.value, method = "holm")) %>%
    mutate(p.value = papaja::printp(p.value)) 

  
  if(length(m.val) == 2){
    contrast.table = contrast.table %>%
      select({{variable}}, estimate, Lower, Upper, p.value) %>%
      kable(digits = 2) %>% kable_styling() %>%
      add_header_above(c(" " = 2, "Confidence Interval" =2 , " " = 1))
  } else {
    contrast.table = contrast.table %>%
      mutate(term = gsub(paste0("^", deparse(substitute(moderator))), "", term)) %>%
      select({{variable}}, term, estimate, Lower, Upper, p.value) %>%
      kable(digits = 2) %>% kable_styling() %>%
       add_header_above(c(" " = 3, "Confidence Interval" =2 , " " = 1))
  }

  return(contrast.table)
}


long_c_plot = function(data, variables, labels){
  
  n_caregivers_week = data %>%
    filter(Week > 0) %>%
    group_by(Week) %>%
    summarize(N = n())
  
  longp = data %>%
    group_by(Week) %>%
    summarize_at(.vars = variables, .funs = list(sum), na.rm=T) %>%
    gather(key, Count, -Week) %>%
    right_join(n_caregivers_week) %>%
    mutate(Percent = 100*Count/N,
           key = factor(key,
                        levels = variables, 
                        labels = labels)) %>%
    ungroup() %>%
    ggplot(aes(x = Week, 
               y = Percent, 
               color = key,
               group=1,
               text = paste("Week:", Week, 
                            "\nResponse:", key,
                            "\nPercent:", round(Percent,2), "\nCount:", Count)
    )) +
    geom_point() + 
    geom_line() +
    labs(x = "Week", 
         y = "Percent by week")+
    scale_x_continuous(breaks = unique(n_caregivers_week$Week)) +
    theme_pubclean()
  
  ggplotly(longp, tooltip = "text")
}

region_c_plot = function(data, variables, labels, group = region){
  
  n_caregivers_region = data %>%
    filter(Week > 0) %>%
    filter(!is.na({{group}})) %>%
    group_by(Week, {{group}}) %>%
    summarize(N = n())
  
  long_plotd = data %>%
    filter(!is.na({{group}})) %>%
    group_by(Week, {{group}}) %>%
    summarize_at(.vars = variables, .funs = list(sum), na.rm=T) %>%
    gather(key, Count, -Week, -{{group}}) %>%
    right_join(n_caregivers_region) %>%
    mutate(Percent = 100*Count/N,
           key = factor(key, 
                        levels = variables, 
                        labels = labels)) %>%
    ungroup() %>%
    ggplot(aes(x = Week, 
               y = Percent, 
               color ={{group}},
               group=1,
               text = paste("Week:", Week, 
                            "\nGroup:", key,
                            "\nPercent:", round(Percent,2), "\nCount:", Count)
    )) +
    geom_point() + 
    geom_line() +
    labs(x = "Week", 
         y = "Percent by week")+
    scale_x_continuous(breaks = unique(data$Week)) +
    facet_wrap(~key, scales = "free")+
    theme_pubclean()
  
  
  ggplotly(long_plotd, tooltip = "text")
}


bygroup_c_plot = function(data, variables, labels, group, g.levels = NULL, g.labels = NULL){
  
  ncaregivers_group = data %>%
    group_by(CaregiverID) %>%
    filter(!is.na({{group}})) %>%
    filter(Week == max(Week)) %>%
    group_by({{group}}) %>%
    summarize(N = n())
  
  long_plotd = data %>%
    dplyr::group_by(CaregiverID) %>%
    filter(!is.na({{group}})) %>%
    filter(Week == max(Week)) %>%
    group_by({{group}}) %>%
    summarize_at(.vars = variables, .funs = list(sum), na.rm=T) %>%
    gather(key, Count, -{{group}}) %>%
    right_join(ncaregivers_group) %>%
    mutate(Percent = 100*Count/N,
           key = factor(key, 
                        levels = variables, 
                        labels = labels)) %>%
    ungroup()
  
  if(!is.null(g.levels)){
    long_plotd = 
      mutate_at(long_plotd, vars({{group}}), .f = function(x) factor(x, 
                                                                     levels = g.levels, 
                                                                     labels = g.labels)) }
  
  
  
  long_plotd = long_plotd %>%
    ggplot(aes(x = reorder(key, Percent),
               y = Percent,
               fill = {{group}},
               group=1,
               text = paste("\nGroup:", {{group}},
                            "\nResponse:", key,
                            "\nPercent:", round(Percent,2), "\nCount:", Count))) +
    geom_bar(stat = "identity") +
    #scale_fill_brewer(palette = "Dark2") +
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    facet_grid(cols = vars({{group}})) +
    theme_pubclean()
  
  
  
  ggplotly(long_plotd, tooltip = "text") 
  
}

bycont_c_plot = function(data, variables, labels, outcome){
  
  
  long_plotd = data %>%
    dplyr::group_by(CaregiverID) %>%
    filter(!is.na({{outcome}})) %>%
    filter(Week == max(Week)) %>%
    gather("key", "value", all_of(variables)) %>%
    filter(value == 1) %>%
    dplyr::group_by(key) %>%
    summarize(Average = mean({{outcome}}),
              MOE = moe({{outcome}})) %>%
    mutate(key = factor(key, 
                        levels = variables, 
                        labels = labels)) %>%
    ggplot(aes(x = reorder(key, Average),
               y = Average,
               fill = key,
               group=1,
               text = paste("\nGroup:", key,
                            "\nAverage Response:", round(Average,2),
                            "\n Lower CI:", round(Average-MOE,2),
                            "\n Upper CI:", round(Average+MOE),2))) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = Average-MOE, ymax = Average + MOE)) +
    labs(x = "")+
    guides(fill = F) +
    coord_flip()+
    theme_pubclean()
  ggplotly(long_plotd, tooltip = "text")
  
}

bygroup_c_ttest = function(data, variables, labels, group){

  
  ncaregivers_group = data %>%
    group_by(CaregiverID) %>%
    filter(!is.na({{group}})) %>%
    filter(Week == max(Week)) %>%
    group_by({{group}}) %>%
    summarize(N = n())
  
  long_plotd = data %>%
    dplyr::group_by(CaregiverID) %>%
    filter(!is.na({{group}})) %>%
    filter(Week == max(Week)) %>%
    group_by({{group}}) %>%
    summarize_at(.vars = variables, .funs = list(sum), na.rm=T) %>%
    gather(key, Count, -{{group}}) %>%
    right_join(ncaregivers_group) %>%
    mutate(Percent = 100*Count/N,
           key = factor(key, 
                        levels = variables, 
                        labels = labels)) %>%
    group_by(key) %>%
    nest() %>%
    mutate(prop = map(data, .f = function(x)
      prop.test(x$Count, x$N))) %>%
    mutate(prop = map(prop, broom::tidy)) %>%
    unnest(prop) %>%
    select(key, statistic, p.value) %>%
    filter(!is.nan(p.value)) %>%
    mutate(p.value = papaja::printp(p.value)) %>%
    kable(digits = 2) %>%
    kable_styling(full_width = T)
  
  return(long_plotd)
  #test = prop.test(x = long_plotd$Count)
  
  
  
  
}

