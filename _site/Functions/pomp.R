pomp_ad = function(raw, min = 0, max = 3){
  100*(raw - min)/(max - min)
}

pomp_stress = function(raw, min = 1, max = 5){
  100*(raw - min)/(max - min)
}