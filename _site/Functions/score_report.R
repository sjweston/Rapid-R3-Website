combine.cat = function(x, cols, id, newvar.name){
  subset = x[,c(id, cols)]
  names(subset) = c("id", paste0("X",1:length(cols)))
  subset = suppressWarnings(gather(subset, "key", "value", -id))
  subset = filter(subset, !is.na(value))
  subset$key = gsub("X", "", subset$key)
  subset = group_by(subset, id)
  subset = summarize(subset, newvar = paste(key, collapse = ","))
  names(subset) = c(id,newvar.name)
  x = suppressMessages(full_join(x, subset))
  return(x)
}

find_items = function(string, data2){
  items = names(data2)
  locations = which(grepl(string, items))
  final = items[locations]
  return(final)
}

contains_items = function(string, data2){
  items = names(data2)
  locations = which(grepl(string, items))
  output = length(locations) > 0
}

identify_state = function(ZC, zipcode.dataset = zipcode){
  if(!is.na(ZC)) {
    loc = which(zipcode.dataset$zip == ZC)
    state = zipcode.dataset$state[loc]
    if(length(loc) == 0) state = NA
  }
  if(is.na(ZC)) state = NA
  return(state)
}


score_report = function(data = NULL, week = NULL, zipcode = zipcode, master = FALSE){
  
  items = names(data)
  if(!master){
    newdata = data.frame(CaregiverID = data$CaregiverID, Week = week)
    newdata$CaregiverID = as.character(newdata$CaregiverID)
    #fill in random caregiver IDs
    missing = which(newdata$CaregiverID == "")
    random_ID = paste0("X", week, "ID", sample(11111:99999, size = length(missing), replace = F))
    newdata$CaregiverID[missing] = random_ID
    names(data) = gsub("R3.R..", "", names(data))
  }
  if(master){
    newdata = data[,c("CaregiverID", "Week", "BaselineWeek")]
    newdata$Date = as.Date(data$StartDate)

  }
  
  
  
  if(contains_items("POLICY.001_[1-7]$", data)){
    newdata$access_telehealth = ifelse(rowSums(data[,find_items("POLICY.001_[1-2]", data)], na.rm=T) > 0, 1, 0)
    newdata$access_social = ifelse(rowSums(data[,find_items("POLICY.001_[3-4]", data)], na.rm=T) > 0, 1, 0)
    newdata$access_online = ifelse(rowSums(data[,find_items("POLICY.001_[1-7]$", data)], na.rm=T) > 0, 1, 0)  
  }
  
  if(contains_items("POLICY.002_[1-7]$", data)){
    newdata$utility_telehealth = rowMeans(data[,find_items("POLICY.002_[1-2]$", data)], na.rm=T)
    newdata$utility_social = rowMeans(data[,find_items("POLICY.002_[3-4]$", data)], na.rm=T)
    newdata$utility_online = rowMeans(data[,find_items("POLICY.002_[1-7]$", data)], na.rm=T)
  }
  
  if(contains_items("POLICY.004_[a-k]$", data)){
    hours_familyCC_current = rowSums(data[,find_items("POLICY.004.[a-d]$", data)],na.rm=T)
    hours_nonfamilyCC_current = rowSums(data[,find_items("POLICY.004.[e-k]$", data)],na.rm=T)
    newdata$familyCC_current = hours_familyCC_current
    newdata$nonfamilyCC_current = hours_nonfamilyCC_current
  }
  
  if(contains_items("POLICY.003.[a-k]$", data)){
    hours_familyCC_pre = rowSums(data[,find_items("POLICY.003.[a-d]$", data)],na.rm=T)
    newdata$hours_familyCC_pre = hours_familyCC_pre
    
    hours_nonfamilyCC_pre = rowSums(data[,find_items("POLICY.003.[e-k]$", data)],na.rm=T)
    newdata$hours_nonfamilyCC_pre = hours_nonfamilyCC_pre
    
    hours_familyCC_current = rowSums(data[,find_items("POLICY.004.[a-d]$", data)],na.rm=T)
    newdata$hours_familyCC_current = hours_familyCC_current
    
    hours_nonfamilyCC_current = rowSums(data[,find_items("POLICY.004.[e-k]$", data)],na.rm=T)
    newdata$hours_nonfamilyCC_current = hours_nonfamilyCC_current
    
    change_familyCC = hours_familyCC_current-hours_familyCC_pre
    newdata$increase_familyCC = ifelse(change_familyCC > 0, 1, 0)
    newdata$decrease_familyCC = ifelse(change_familyCC < 0, 1, 0)
    
    change_nonfamilyCC = hours_nonfamilyCC_current-hours_nonfamilyCC_pre
    newdata$increase_nonfamilyCC = ifelse(change_nonfamilyCC > 0, 1, 0)
    newdata$decrease_nonfamilyCC = ifelse(change_nonfamilyCC < 0, 1, 0)
  }
  
  if(contains_items("POLICY.006_[1-7]$", data)){
    data = combine.cat(x = data, 
                       cols = find_items("POLICY.006_[1-7]$", data), 
                       id = "CaregiverID",
                       newvar.name = "POLICY.006_cat")
    newdata$parent_edu_interrupt = ifelse(grepl("[1,3,5]",data$POLICY.006_cat), 1, 0)
  }
  
  if(contains_items("POLICY.008_[1-7]$", data)){
    data = combine.cat(x = data, 
                       cols = find_items("POLICY.008_[1-7]$", data), 
                       id = "CaregiverID",
                       newvar.name = "POLICY.008_cat")
    newdata$child_edu_interrupt = ifelse(grepl("[1,3,5]",data$POLICY.008_cat), 1, 0)
  }
  
  if(contains_items("HEALTH.001", data)) newdata$insurance = ifelse(data$HEALTH.001 == 1, 1, 0)
  if(contains_items("HEALTH.002", data)) newdata$child_insurance = ifelse(data$HEALTH.002 == 1, 1, 0)
  if(contains_items("HEALTH.003", data)) {
    num_delay_healthcare = rowSums(data[,find_items("HEALTH.003.[a-f]$", data)],na.rm=T)
    newdata$delay_healthcare = ifelse(num_delay_healthcare > 0, 1, 0)
  }
  
  if(contains_items("COVID.001", data)){
    newdata$suspected_covid = ifelse(data$COVID.001 %in% c(1,2,3), 1, 0)
    newdata$diagnosed_covid = ifelse(data$COVID.001 %in% c(2,3), 1, 0)
  }
  
  if(contains_items("COVID.002", data)){
    newdata$hospital = ifelse(data$COVID.002 %in% c(1,2), 1, 0)
    newdata$hospital_covid  = ifelse(data$COVID.002 == 2, 1, 0)
  }
  
  if(contains_items("GAD2.002", data)){
    anxiety_current = rowMeans(data[,find_items("GAD2.002.[a-b]$", data)], na.rm=T)
    newdata$anxiety_current_some = ifelse(anxiety_current > 0, 1, 0)
    newdata$anxiety_current_lots = ifelse(anxiety_current > 1, 1, 0)
    newdata$anxiety = anxiety_current
  }
  if(contains_items("GAD2.001", data)){
    anxiety_pre = rowMeans(data[,find_items("GAD2.001.[a-b]$", data)], na.rm=T)
    anxiety_change = anxiety_current = anxiety_pre
    newdata$anxiety_pre_some = ifelse(anxiety_pre > 0, 1, 0)
    newdata$anxiety_pre_lots = ifelse(anxiety_pre > 1, 1, 0)
    newdata$anxiety_increase = ifelse(anxiety_change > 0, 1, 0)
    newdata$anxiety_pre = anxiety_pre
  }
  
  if(contains_items("PHQ.002", data)){
    depress_current = rowMeans(data[,find_items("PHQ.002.[a-b]$", data)], na.rm=T)
    newdata$depress_current_some = ifelse(depress_current > 0, 1, 0)
    newdata$depress_current_lots = ifelse(depress_current > 1, 1, 0)
    newdata$depress = depress_current
  }
  
  if(contains_items("PHQ.001", data)){
    depress_pre = rowMeans(data[,find_items("PHQ.001.[a-b]$", data)], na.rm=T)
    newdata$depress_pre_some = ifelse(depress_pre > 0, 1, 0)
    newdata$depress_pre_lots = ifelse(depress_pre > 1, 1, 0)
    depress_change = depress_current - depress_pre
    newdata$depress_pre = depress_pre
    newdata$depress_increase  = ifelse(depress_change > 0, 1, 0)
    newdata$depress_pre = depress_pre
  }
  
  if(contains_items("STRESS.002", data)){
    newdata$stress_current_some = ifelse(data$STRESS.002 > 0, 1, 0)
    newdata$stress_current_lots = ifelse(data$STRESS.002 > 2, 1, 0)
    newdata$stress = data$STRESS.002
  }
  if(contains_items("STRESS.001", data)){
    stress_change = data$STRESS.002 - data$STRESS.001
    newdata$stress_pre_some = ifelse(data$STRESS.001 > 0, 1, 0)
    newdata$stress_pre_lots = ifelse(data$STRESS.001 > 2, 1, 0)
    newdata$stress_increase  = ifelse(stress_change > 0, 1, 0)
    newdata$stress_pre = data$STRESS.001
  }
  
  
  if(contains_items("PSIIV.001.a", data)){
    newdata$handle_pre = ifelse(data$PSIIV.001.a > 3, 1, 0)
    handle_change = data$PSIIV.001.b - data$PSIIV.001.a
    newdata$handle_decrease = ifelse(handle_change < 0, 1, 0)
  }
  
  if(contains_items("PSIIV.001.b", data)){
    newdata$handle_current = ifelse(data$PSIIV.001.b > 3, 1, 0)
  }
  
  if(contains_items("PSIIV.002", data)){
    newdata$support_pre = ifelse(data$PSIIV.002 %in% c(1,2), 1, 0)
    support_change = data$PSIIV.003 - data$PSIIV.002
    newdata$support_decrease = ifelse(support_change < 0, 1, 0)
  }
  
  if(contains_items("PSIIV.003", data)){
    newdata$support_pre = ifelse(data$PSIIV.003 %in% c(1,2), 1, 0)
  }
  
  if(contains_items("LONE.001.b", data)){
    newdata$lonely_current_some = ifelse(data$LONE.001.b > 1, 1, 0)
    newdata$lonely_current_lots = ifelse(data$LONE.001.b > 2, 1, 0)
  }
  if(contains_items("LONE.001.a", data)){
    newdata$lonely_pre_some = ifelse(data$LONE.001.a > 1, 1, 0)
    newdata$lonely_pre_lots = ifelse(data$LONE.001.a > 2, 1, 0)
    lone_change = data$LONE.001.b - data$LONE.001.a
    newdata$lone_increase = ifelse(lone_change > 0, 1, 0)
  }
  
  
  if(contains_items("JOB.001_[1,2,3]_TEXT", data)){
    income_weeklybased = suppressWarnings(as.numeric(data$JOB.001_1_TEXT)*52)
    income_monthlybased = suppressWarnings(as.numeric(data$JOB.001_2_TEXT)*12)
    income_yearlybased = suppressWarnings(as.numeric(data$JOB.001_3_TEXT))
    income.mat = cbind(income_weeklybased, income_monthlybased, income_yearlybased)
    inc.missing = apply(income.mat, 1, function(x) length(which(is.na(x))))
    income = rowSums(income.mat, na.rm=T)
    income[which(inc.missing == 3)] = NA
    newdata$income = income
    income = income/1000
    newdata$income.cat = cut(income, breaks =  quantile(income, probs = seq(.1, .9, by = .1), na.rm=T) )
  }
  
  
  if(contains_items("JOB.005", data)){
    data = combine.cat(x = data, 
                       cols = find_items("JOB.005", data), 
                       id = "CaregiverID",
                       newvar.name = "Job.005_cat")
    newdata$free_food = ifelse(grepl("1", data$Job.005_cat), 1, 0)
  }
  
  
  if(contains_items("JOB.007", data)){
    data = combine.cat(x = data, 
                       cols = find_items("JOB.007", data), 
                       id = "CaregiverID",
                       newvar.name = "Job.007_cat")
    free_lunch_current = ifelse(grepl("2",data$Job.007_cat), 1, 0)}
  
  if(contains_items("JOB.006", data)){
    data = combine.cat(x = data, 
                       cols = find_items("JOB.006", data), 
                       id = "CaregiverID",
                       newvar.name = "Job.006_cat")
    free_lunch_pre = ifelse(grepl("2",data$Job.006_cat), 1, 0)
    
    newdata$lost_free_lunch = case_when(free_lunch_pre == 1 & free_lunch_current == 1 ~ 0,
                                        free_lunch_pre == 1 & free_lunch_current == 0 ~ 1,
                                        TRUE ~ NA_real_)
    newdata$gained_free_lunch = case_when(free_lunch_pre == 0 & free_lunch_current == 1 ~ 1,
                                          free_lunch_pre == 0 & free_lunch_current == 0 ~ 0,
                                          TRUE ~ NA_real_)
  }
  
  if(contains_items("JOB.011", data)) newdata$employment_decreased = ifelse(data$JOB.011 == 2, NA, data$JOB.011)
  
  if(contains_items("JOB.013", data)) newdata$sick_leave = ifelse(data$JOB.013 == 2, NA, data$JOB.013)
  
  if(contains_items("JOB.014", data)){
    losejob_sickleave = ifelse(data$JOB.014 == 5, NA, data$JOB.014)
    newdata$losejob_sickleave = ifelse(losejob_sickleave > 2, 1, 0)
  } 
  
  
  if(contains_items("JOB.015", data)) newdata$unemployment = ifelse(data$JOB.015 == 1, 1, 0)
  
  if(contains_items("EHQ.001", data)) newdata$income_decreaed = ifelse(data$EHQ.001 == 1, 1, 0)
  
  if(contains_items("EHQ.002", data)){
    newdata$financial_prob  = ifelse(data$EHQ.002 == 0, 0, 1)
    newdata$major_financial  = ifelse(data$EHQ.002 %in% c(0,1), 0, 1)
  }
  
  if(contains_items("FSTR.001", data))newdata$difficulty_basics  = ifelse(data$FSTR.001 %in% c(3,2), 1, 0)
  
  if(contains_items("CBCL.002.a", data)){
    newdata$fussy_current_some = ifelse(data$CBCL.002.a > 0, 1, 0)
    newdata$fussy_current_lots = ifelse(data$CBCL.002.a > 1, 1, 0)
    newdata$fussy = data$CBCL.002.a 
  }
  if(contains_items("CBCL.001.a", data)){
    newdata$fussy_pre = data$CBCL.001.a
    newdata$fussy_pre_some = ifelse(data$CBCL.001.a > 0, 1, 0)
    newdata$fussy_pre_lots = ifelse(data$CBCL.001.a > 1, 1, 0)
    fussy_difference = data$CBCL.002.a-data$CBCL.001.a
    newdata$fussy_more = ifelse(fussy_difference > 0, 1, 0)
  }
  
  if(contains_items("CBCL.002.b", data)){
    newdata$fear_current_some = ifelse(data$CBCL.002.b > 0, 1, 0)
    newdata$fear_current_lots = ifelse(data$CBCL.002.b > 1, 1, 0)
    newdata$fear = data$CBCL.002.b
  }
  if(contains_items("CBCL.002.a", data)){
    newdata$fear_pre = data$CBCL.002.a
    newdata$fear_pre_some = ifelse(data$CBCL.002.a > 0, 1, 0)
    newdata$fear_pre_lots = ifelse(data$CBCL.002.a > 1, 1, 0)
    fear_difference = data$CBCL.002.a-data$CBCL.002.a
    newdata$fear_more = ifelse(fear_difference > 0, 1, 0)
  }
  
  
  if(contains_items("HEALTH.005", data)){
    data = combine.cat(x = data, 
                       cols = find_items("HEALTH.005", data), 
                       id = "CaregiverID",
                       newvar.name = "HEALTH.005_cat")
    newdata$disability = ifelse(grepl("[1-4]", data$HEALTH.005_cat), 1, 0)
  }
  
  if(contains_items("JOB.010", data)){
    data = combine.cat(x = data, 
                       cols = find_items("JOB.010_.{1,2}$", data),
                       id = "CaregiverID",
                       newvar.name = "JOB.010_cat")
    data$JOB.010_cat = gsub("10", "0", data$JOB.010_cat)
    newdata$working_pre = ifelse(grepl("[1,2,6]", data$JOB.010_cat), 1, 0)
  }
  
  if(contains_items("JOB.008", data)){
    data = combine.cat(x = data, 
                       cols = find_items("JOB.008_.{1,2}$", data),
                       id = "CaregiverID",
                       newvar.name = "JOB.008_cat")
    data$JOB.008_cat = gsub("10", "0", data$JOB.008_cat)
    newdata$working_current = ifelse(grepl("[1,2,6]", data$JOB.008_cat), 1, 0)
  }
  
  
  if(contains_items("JOB.012", data)) newdata$essential = ifelse(data$JOB.012 == 1, 1, 0)
  
  if(contains_items("DEMO.002", data))newdata$single = ifelse(data$DEMO.002 %in% c(3,4,5,7,8), 1, 0)
  
  if(contains_items("DEMO.003", data)){
      newdata$num_children = case_when(
      data$DEMO.003 == 1 ~ "1 Child",
      data$DEMO.003 == 2 ~ "2 Children",
      data$DEMO.003 == 3 ~ "3 Children",
      data$DEMO.003 >= 4 ~ "4+ Children")
      newdata$num_children_raw = data$DEMO.003
      newdata$household_size = data$DEMO.003 + (2-newdata$single)
    }
  
  if(contains_items("DEMO.007_.{1}$", data)){
    data = combine.cat(x = data, 
                       cols = find_items("DEMO.007_.{1}$", data), 
                       id = "CaregiverID",
                       newvar.name = "DEMO.007_cat")
    newdata$minority = ifelse(grepl("[^5]", data$DEMO.007_cat), 1, 0)
    newdata$black = ifelse(grepl("3", data$DEMO.007_cat), 1, 0)
    }
  
  if(contains_items("DEMO.008", data)) newdata$latinx = as.numeric(data$DEMO.008)
  
  if(contains_items("DEMO.001", data)){
    state = unlist(sapply(data$DEMO.001, identify_state))
    newdata$zip = data$DEMO.001
    newdata$state = state
    newdata$region = case_when(
      state %in% c("ME", "NH", "VT", "MA", "RI", "CT",
                   "NY", "NJ", "PA") ~ "Northeast",
      state %in% c("OH", "MI", "IL", "IN", "WI", "MN", "IA",
                   "MO", "ND", "SD", "NE", "KS") ~ "Midwest",
      state %in% c("DE", "MD", "VA", "WV", "KY", "NC", "SC",
                   "TN", "GA", "FL", "AL", "MS", "AR", "LA",
                   "TX", "OK") ~ "South",
      state %in% c("MT", "ID", "WY", "CO", "NM", "AZ",
                   "UT", "NV", "CA", "OR", "WA", "AK",
                   "HI") ~ "West")
    }
  
  return(newdata)
}
