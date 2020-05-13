pomp_ad = function(raw, min = 0, max = 3){
  100*(raw - min)/(max - min)
}

pomp_stress = function(raw, min = 1, max = 5){
  100*(raw - min)/(max - min)
}

pomp_lonely = function(raw, min = 0, max = 4){
  100*(raw - min)/(max - min)
}

pomp_child = function(raw, min = 0, max = 2){
  100*(raw - min)/(max - min)
}