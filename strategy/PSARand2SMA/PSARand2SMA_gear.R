PSARand2SMA_gear <- function(data, slow.sma, fast.sma, accel.start = 0.02, accel.max = 0.2) {
  require(quantmod) 
  # описание psar.2sma стратегии 
  data$sma <- SMA(Cl(data), slow.sma)
  data$fma <- SMA(Cl(data), fast.sma)
  data$sar <- SAR(data, accel = c(accel.start, accel.max))
  data$sig.sma <- ifelse(data$fma > data$sma, 1, 
               ifelse(data$fma < data$sma, -1, 0))
  data$sig.sar <- ifelse(data$Close > data$sar, 1, 
               ifelse(data$Close < data$sar, -1, 0))
  data$pos <- sign(data$sig.sma + data$sig.sar)
  data$pos <- lag(data$pos)
  data <- na.omit(data)
  return(data)
}    