pomp_ad = function(raw, min = 0, max = 3){
  100*(raw - min)/(max - min)
}

pomp_stress = function(raw, min = 0, max = 4){
  100*(raw - min)/(max - min)
}

pomp_lonely = function(raw, min = 0, max = 4){
  100*(raw - min)/(max - min)
}

pomp_child = function(raw, min = 0, max = 2){
  100*(raw - min)/(max - min)
}

pomp = function(raw){
  min = min(raw, na.rm=T)
  max = max(raw, na.rm=T)
  new = 100*(raw - min)/(max - min)
  return(new)
}
