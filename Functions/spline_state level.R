state_splines.groups = function(data, outcome, group, point){
  
  group.name = deparse(substitute(group))
  group.label = gsub("_", " ", group.name)
  group.label = stringr::str_to_sentence(group.label)
  
  outcome.label = deparse(substitute(outcome))
  outcome.label = gsub("_", " ", outcome.label)
  outcome.label = stringr::str_to_sentence(outcome.label)
  
  group.levs = unique(as.data.frame(data[,group.name]))
  ngroups = nrow(group.levs)
  contrast = sum(group.levs[,1] %in% c(-1,1)) == 2
  
  data = data %>%
    filter(!is.na({{group}})) %>%
    filter(Week > 0) %>%
    group_by(state, Week, {{group}}) %>%
    summarise_at(vars({{outcome}}), mean, na.rm=T) %>%
    ungroup() %>%
    mutate(SL1 = ifelse(Week <= point, Week-point, 0),
           SL2 = ifelse(Week > point, Week-point, 0))
  
  reg.formula = as.formula(paste0(deparse(substitute(outcome)),
                                  " ~ SL1*",
                                  deparse(substitute(group)),
                                  "+ SL2*",
                                  deparse(substitute(group))))
  model = lm(reg.formula, data)

  mod.summary = broom::tidy(model)
  mod.summary = mod.summary %>%
    mutate(section = case_when(
      grepl("SL1", term) ~ "SL1",
      grepl("SL2", term) ~ "SL2",
      TRUE ~ "Intercept"
    )) %>%
    arrange(section, term) %>%
    select(-section) %>%
    kable(., digits = 2) %>%
    kable_styling() 
    

  data$pred = predict(model)

  plot = data %>%
    filter(!is.na({{outcome}})) %>%
    ggplot(aes(x = Week, color = as.factor({{group}}))) +
    geom_point(aes(y = {{outcome}}), alpha = .5) +
    geom_line(aes(y = pred)) +
    scale_x_continuous(breaks = c(1:max(data$Week)))+
    labs(y = outcome.label) +
    theme_pubclean()

  if(contrast){
    plot = plot +
      scale_color_manual(group.label,
                         values = c("red", "darkgrey"),
                         labels = c("Group", "Sample Average"))
  } else{
    plot = plot +
      scale_color_brewer(group.label, palette = "Set2")
  }
  
   plotdata = plot$data
  
   plot = ggplotly(plot)

   return.list = list(model = model,
                      summary = mod.summary,
                      plot = plot,
                      plotdata = plotdata)
  
  return(return.list)
}
