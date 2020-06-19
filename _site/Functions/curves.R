train_fun = function(train.data, poly){
  if(poly > 1) reg.formula = as.formula(paste0("value ~ poly(Week,",poly,")"))
  if(poly == 1) reg.formula = as.formula(paste0("value ~ Week"))
  train.out = train(
    reg.formula, 
    data = train.data,
    trControl = trainControl(method="repeatedcv", number=10, repeats = 10),
    method = "lm"
  )
  return(train.out)
}


overall_model = function(data, outcome, poly){
  
  if(poly > 1){
    reg.formula = as.formula(paste0(deparse(substitute(outcome)),
                                    " ~ poly(Week,",
                                    poly,
                                    ") +  (1|CaregiverID)"))}
  if(poly == 1){
    reg.formula = as.formula(paste0(deparse(substitute(outcome)),
                                    " ~ Week + (1|CaregiverID)"))
  }
  
  model = lmer(reg.formula, data)
  predicted = plot_model(model, type = "pred")
  pred.data = predicted$Week$data
  mod.summary = broom::tidy(model)
  
  plot = data %>%
    filter(!is.na({{outcome}})) %>%
    group_by(Week) %>%
    summarize(m = mean({{outcome}}),
              sd = sd({{outcome}}),
              n = n(),
              se = sd/sqrt(n),
              moe = 1.96*se) %>%
    ggplot(aes(x = Week, y = m)) +
    geom_point(color = "darkgrey")+
    geom_errorbar(aes(ymin = m-moe, ymax = m+moe), 
                  width = .5, 
                  color = "darkgrey") +
    geom_line(aes(x = x, y = predicted), data = pred.data, inherit.aes = F) +
    scale_x_continuous(breaks = c(1:10))+
    scale_y_continuous(str_to_title(deparse(substitute(outcome))))+
    theme_pubclean()
  
  return.list = list(model = predicted,
                     pdata = pred.data,
                     plot = plot)
  return(return.list)
}


group_model = function(data, outcome, group, poly){
  
  if(poly > 1){
    reg.formula = as.formula(paste0(deparse(substitute(outcome)),
                                    " ~ ",
                                    deparse(substitute(group)),
                                    "*poly(Week,",
                                    poly,
                                    ") +  (1|CaregiverID)"))}
  if(poly == 1){
    reg.formula = as.formula(paste0(deparse(substitute(outcome)),
                                    " ~ ",
                                    deparse(substitute(group)),
                                    "*Week + (1|CaregiverID)"))
  }
  
  model = lmer(reg.formula, data)
  predicted = plot_model(model, type = "pred", terms = c("Week [all]", deparse(substitute(group))))
  pred.data = as.data.frame(predicted$data)
  mod.summary = broom::tidy(model)
  
  
  color.pal = c("red", "darkgrey")
  
  plot = data %>%
    filter(!is.na({{outcome}})) %>%
    group_by(Week, {{group}}) %>%
    summarize(m = mean({{outcome}}),
              sd = sd({{outcome}}),
              n = n(),
              se = sd/sqrt(n),
              moe = 1.96*se) %>%
    ungroup() %>%
    ggplot(aes(x = Week, y = m, color = as.factor({{group}}))) +
    geom_smooth(aes(x = x, 
                    y = predicted,
                    ymin = conf.low, 
                    ymax = conf.high, fill = as.factor(group),
                    color = as.factor(group)), 
                stat = "identity",
                data = pred.data, inherit.aes = F, alpha = .3) +
    geom_point(alpha = .5)+
    # geom_errorbar(aes(ymin = m-moe, ymax = m+moe),
    #               width = .5,
    #               alpha = .5) +
    
    #geom_line(aes(x = x, y = predicted, color = as.factor(group)), data = pred.data, inherit.aes = F) +
    scale_color_manual(str_to_title(deparse(substitute(group))),
                       values = color.pal, 
                       labels = c("Group", "Everyone else"))+
    scale_fill_manual(str_to_title(deparse(substitute(group))),
                       values = color.pal, 
                       labels = c("Group", "Everyone else"))+
    scale_x_continuous(breaks = c(1:10))+
    scale_y_continuous(str_to_title(deparse(substitute(outcome))))+
    theme_pubclean()
  
  group.text = deparse(substitute(group))
  
  mod.summary = mod.summary %>%
    mutate(pvalue = pt(abs(statistic), df = nrow(data)-nrow(mod.summary), lower.tail = F)*2) %>%
    filter(grepl(group.text, term)) %>%
    select(-group) %>%
    kable(., digits = 2) %>%
    kable_styling()
  
  return.list = list(model = model,
                     pdata = pred.data,
                     plot = plot,
                     summary = mod.summary)
  return(return.list)
}

splines.overall = function(data, outcome, point){
  
  data = data %>%
    mutate(SL1 = ifelse(Week <= point, Week-point, 0),
           SL2 = ifelse(Week > point, Week-point, 0))
  
  reg.formula = as.formula(paste0(deparse(substitute(outcome)),
                                  " ~ SL1 + SL2 + (1|CaregiverID)")) 
  model = lmer(reg.formula, data)
  
  mod.summary = broom::tidy(model) 
  mod.summary = mod.summary %>%
    mutate(pvalue = pt(abs(statistic), 
                       df = nrow(data)-nrow(mod.summary), 
                       lower.tail = F)*2) %>%
    filter(!grepl("^sd_", term)) %>%
    select(-group) %>%
    kable(., digits = 2) %>%
    kable_styling()
  
  predicted = data.frame(Week = 1:10,
                         CaregiverID = 0000) %>%
    mutate(SL1 = ifelse(Week <= point, Week-point, 0),
           SL2 = ifelse(Week > point, Week-point, 0))
  
  predicted$fit = predict(model, 
                          newdata = predicted, 
                          allow.new.levels = T)
  
  plot = data %>%
    filter(!is.na({{outcome}})) %>%
    group_by(Week) %>%
    summarize(m = mean({{outcome}}),
              sd = sd({{outcome}}),
              n = n(),
              se = sd/sqrt(n),
              moe = 1.96*se) %>%
    ggplot(aes(x = Week, y = m)) +
    geom_point(color = "darkgrey") +
    geom_line(aes(x = Week, y = fit), data = predicted, inherit.aes = F) +
    scale_x_continuous(breaks = c(1:10))+
    scale_y_continuous(str_to_title(deparse(substitute(outcome))))+
    theme_pubclean()
  
  return.list = list(model = model,
                     summary = mod.summary,
                     pdata = predicted,
                     plot = plot)
  return(return.list)
}

splines.groups = function(data, outcome, group, point){
  
  group.name = deparse(substitute(group))
  
  color.pal = c("red", "darkgrey")
  
  data = data %>%
    filter(!is.na({{group}})) %>%
    mutate(SL1 = ifelse(Week <= point, Week-point, 0),
           SL2 = ifelse(Week > point, Week-point, 0))
  
  reg.formula = as.formula(paste0(deparse(substitute(outcome)),
                                  " ~ SL1*",
                                  deparse(substitute(group)),
                                  "+ SL2*",
                                  deparse(substitute(group)),
                                  "+ (1|CaregiverID)")) 
  model = lmer(reg.formula, data)
  
  mod.summary = broom::tidy(model) 
  mod.summary = mod.summary %>%
    mutate(pvalue = pt(abs(statistic), 
                       df = nrow(data)-nrow(mod.summary), 
                       lower.tail = F)*2) %>%
    filter(grepl(group.name,term)) %>%
    select(-group) %>%
    kable(., digits = 2) %>%
    kable_styling()
  
  predicted = data %>% 
    group_by(Week, {{group}}) %>% 
    summarize(n=n()) %>%
    mutate(CaregiverID = 0000,
           SL1 = ifelse(Week <= point, Week-point, 0),
           SL2 = ifelse(Week > point, Week-point, 0))

  predicted$fit = predict(model, 
                          newdata = predicted, 
                          allow.new.levels = T)
  
  plot = data %>%
    filter(!is.na({{outcome}})) %>%
    group_by(Week, {{group}}) %>%
    summarize(m = mean({{outcome}}),
              sd = sd({{outcome}}),
              n = n(),
              se = sd/sqrt(n),
              moe = 1.96*se) %>%
    ggplot(aes(x = Week, y = m, color = as.factor({{group}}))) +
    geom_point(alpha = .5) +
    geom_line(aes(x = Week, y = fit, color = as.factor({{group}})), 
                  data = predicted, 
                  inherit.aes = F) +
    scale_color_manual(str_to_title(deparse(substitute(group))),
                                   values = color.pal, 
                                   labels = c("Group", "Everyone else")) +            
    scale_x_continuous(breaks = c(1:10))+
    scale_y_continuous(str_to_title(deparse(substitute(outcome))))+
    theme_pubclean()
  
  return.list = list(model = model,
                     summary = mod.summary,
                     pdata = predicted,
                     plot = plot)
  return(return.list)
}
