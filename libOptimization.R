 
TransformMetric <- function(metric, name){
  # функция трансформации данных метрик к одному виду
  print (paste("Calculating Performance Metric:", name))
  metricData <- as.matrix(metric)
  # трансформация в нужный вид:
  if(nrow(metricData) == 1){
    metricData <- t(metricData)
  }
  colnames(metricData) <- name
  rownames(metricData) <- ""
  return (metricData)
}
 
 RatioTable <- function(returns){
  require(PerformanceAnalytics)
  # функция вычисления стандартных наборов метрик продуктивности для результатов работы стратегий
  # расчет коэффициентов
  # SharpeRatio
  Sharp.calc <-  SharpeRatio.annualized(returns, scale=1)
  # SortinoRatio
  Sortino.calc <- SortinoRatio(returns)
  pMetric <- cbind(TransformMetric(Sharp.calc, name="SharpRatio"), TransformMetric(Sortino.calc, name="SortinoRatio"))
  # CalmarRatio
  Calmar.calc <- CalmarRatio(returns, scale=1)
  pMetric <- cbind(pMetric, TransformMetric(Calmar.calc, name="CalmarRatio"))
  # SterlingRatio
  Sterling.calc <- SterlingRatio(returns, scale=1)
  pMetric <- cbind(pMetric, TransformMetric(Sterling.calc, name="SterlingRatio"))
  pMetric[,1] <- 
  return(pMetric)
} 
DrawdownTable <- function(returns, plot=FALSE) {
  # подготовка данных
  print (paste("Calculating Drawdown Metric:", "Drawdown Data Set"))
  drawdowns <- table.Drawdowns(returns[,1])
  for (i in seq(1:nrow(drawdowns))) {
    drawdowns$IntDays[i] <- trunc(as.numeric(drawdowns$To[i] - drawdowns$From[i]))
  }
  # max просадка
  print (paste("Calculating Performance Metric:", "MaxDrawdown"))
  MaxDrawdown.calc <- drawdowns$Depth[1]
  # средняя просадка
  print (paste("Calculating Performance Metric:", "MeanDrawdown"))
  MeanDrawdown.calc <- trunc(mean(drawdowns$Depth))
  pMetric <- cbind(MaxDrawdown.calc, MeanDrawdown.calc)
  # max длина просадки в днях
  print (paste("Calculating Performance Metric:", "MaxDrawdownDays"))
  MaxDrawdownDays.calc <- drawdowns$IntDays[which.max(drawdowns$IntDays)]
  pMetric <- cbind(pMetric, MaxDrawdownDays.calc)
  # среднее число дней в просадке
  print (paste("Calculating Performance Metric:", "MeanDrawdownDays"))
  MeanDrawdown.calc <- trunc(mean(drawdowns$IntDays))
  pMetric <- cbind(pMetric, MeanDrawdownDays.calc)
  # текущее число дней в просадке
  print (paste("Calculating Performance Metric:", "NowDrawdownDays"))
  NowDrawdownDays.calc <- drawdowns$IntDays[which.max(drawdowns$To)]
  pMetric <- cbind(pMetric, NowDrawdownDays.calc)
  # текущая просадка 
  print (paste("Calculating Performance Metric:", "NowDrawdown"))
  NowDrawdown.calc <- DrawdownPeak(returns)
  pMetric <- cbind(pMetric, NowDrawdown.calc)
  # графики
  if (plot==TRUE) {
    drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
    drawdowns.dates[is.na(drawdowns.dates)] <- format(index(returns)[NROW(returns)])
    drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])  
  }
 return(pMetric)
}

  # просадки
  # максимальная просадка
  maxDrawdown.calc <- maxDrawdown
  pMetric <- cbind(pMetric,CalculatePerformanceMetric(returns,"maxDrawdown"))
  # средняя просадка
  pMetric <- cbind(pMetric,CalculatePerformanceMetric(returns,"AverageDrawdown"))
  # средняя длина просадки
  pMetric <- cbind(pMetric,CalculatePerformanceMetric(returns,"AverageLength"))
  # this function calculates the average length of the recovery period of the drawdowns observed
  pMetric <- cbind(pMetric,CalculatePerformanceMetric(returns,"AverageRecovery"))


  colnames(pMetric) <- c("Profit","SharpeRatio","MaxDrawDown")
 
  print("Performance Table")
  print(pMetric)
  return (pMetric)
}