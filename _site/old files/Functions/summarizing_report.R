percent.report = function(x){
  total_valid = length(which(!is.na(x)))
  total_yes = sum(x, na.rm=T)
  percent = total_yes/total_valid*100
  percent = round(percent,1)
  return(percent)
}

first_instance = function(pattern, string){
  min(which(grepl(pattern,string)))
}

last_instance = function(pattern, string){
  max(which(grepl(pattern,string)))
}