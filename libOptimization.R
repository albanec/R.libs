 
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
 
 PerformanceTable <- function(returns){
  # функция вычисления стандартных наборов метрик продуктивности для результатов работы стратегий
  # расчет коэффициентов
  pMetric <- NA
  Sharp.calc <-  SharpeRatio.annualized(returns, scale=1)
  pMetric <- cbind(pMetric, TransformMetric(Sharp.calc, name="SharpRatio"))
  Sortino.calc <- SortinoRatio(returns)
  pMetric <- cbind(pMetric, TransformMetric(Sortino.calc, name="SortinoRatio"))
  Calmar.calc <- CalmarRatio(returns, scale=1)
  pMetric <- cbind(pMetric, TransformMetric(Calmar.calc, name="CalmarRatio"))
  Sterling.calc <-  SterlingRatio(returns, scale=1)
  pMetric <- cbind(pMetric, TransformMetric(Sterling.calc, name="SterlingRatio"))
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