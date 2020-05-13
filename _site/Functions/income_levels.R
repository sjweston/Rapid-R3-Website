income_levels = function(x, num_levels){
  x = x/1000
  cut(x, breaks = c(seq(from=0, by =20, length.out = num_levels-1), max(x, na.rm=T)), include.lowest = T)
}
