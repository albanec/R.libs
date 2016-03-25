 
TransformMetric <- function(metric, name){
  # функция трансформации данных метрик к одному виду (нужна для RatioTable)
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
#
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
# 
DrawdownDataSet <- function(returns, days=TRUE) {
  # функция возращает таблицу просадок + кол-во дней в просадке 
  # на вход - SR
  require(PerformanceAnalytics)
  drawdowns <- table.Drawdowns(returns[,1], top=1000000)
  if (days==TRUE) {
    for (i in seq(1:nrow(drawdowns))) {
      drawdowns$Days <- as.numeric(-floor(difftime(drawdowns$From, drawdowns$To, units = "days")))
      
    }  
  }
  return(drawdowns)
}
#
DrawdownTable <- function(returns, plot=FALSE, period="15") {
  # вычисляет параметры по просадкам
  # период - в минутах 
  require(PerformanceAnalytics)
  # подготовка данных
  print (paste("Calculating Drawdown Metric:", "Drawdown Data Set"))
  drawdowns <- DrawdownDataSet(returns, days=TRUE)
  # max просадка
  print (paste("Calculating Performance Metric:", "MaxDrawdown"))
  MaxDrawdown.calc <- as.numeric(drawdowns$Depth[1])
  # средняя просадка
  print (paste("Calculating Performance Metric:", "MeanDrawdown"))
  MeanDrawdown.calc <- as.numeric(mean(drawdowns$Depth))
  pMetric <- cbind(MaxDrawdown.calc, MeanDrawdown.calc)
  # max длина просадки в днях
  print (paste("Calculating Performance Metric:", "MaxDrawdownDays"))
  MaxDrawdownDays.calc <- as.numeric(drawdowns$Days[which.max(drawdowns$Days)])
  pMetric <- cbind(pMetric, MaxDrawdownDays.calc)
  # среднее число дней в просадке
  print (paste("Calculating Performance Metric:", "MeanDrawdownDays"))
  MeanDrawdownDays.calc <- as.numeric(trunc( mean( na.omit(drawdowns$Days) )))
  pMetric <- cbind(pMetric, MeanDrawdownDays.calc)
  # текущее число дней в просадке
  print (paste("Calculating Performance Metric:", "NowDrawdownDays"))
  NowDrawdownDays.calc <- as.numeric(-floor(-(drawdowns$Length[which(is.na(drawdowns$To))] * as.numeric(period)/60/24)))
  pMetric <- cbind(pMetric, NowDrawdownDays.calc)
  # текущая просадка 
  print (paste("Calculating Performance Metric:", "NowDrawdown"))
  NowDrawdown.calc <- as.numeric(DrawdownPeak(last(returns)))
  pMetric <- cbind(pMetric, NowDrawdown.calc)
  pMetric <- data.frame(pMetric)
  colnames(pMetric) <- c("MaxDrawdown", "MeanDrawdown" , "MaxDrawdownDays", "MeanDrawdownDays", "NowDrawdownDays", "NowDrawdown")  
  return(pMetric)
}
#
BigPlot <- function(returns, MarginPlot=FALSE, ReturnsPlot=FALSE, DrawdownShadowPlot=FALSE, DrawdownPlot=FALSE, CandlePlot=FALSE, period="15 min") {
  require(PerformanceAnalytics)
  require(plotly)
  print (paste("Calculating Metric:", "Drawdown Data Set"))
  n <- 0
  mPlot <- NA
  rPlot <- NA
  dsPlot <- NA
  dPlot <- NA
  cPlot <- NA
  mPlot.par <- NA
  rPlot.par <- NA
  dsPlot.par <- NA
  dPlot.par <- NA
  cPlot.par <- NA
  if (MarginPlot==TRUE) {
    n <- n + 1
    if (drawdownShadow.plot==TRUE) {
      print(paste("Calculating Charts:", "Margin & DrawdownShadow Chart"))
      drawdowns <- DrawdownDataSet(returns, days=FALSE)
      drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
      drawdowns.dates[is.na(drawdowns.dates)] <- format(index(returns)[NROW(returns)])
      drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])  
      mPlot.par <- par(mar=c(1,4,4,2))
      mPlot <- chart.CumReturns(  returns, main = "PerformanceCharts", colorset=c(2,3,4), 
        period.areas = drawdowns.dates, period.color = rgb[1],
        legend.loc = "topleft", geometric=TRUE, ylab="Cumulative Simple Returns"
        )
      if (DrawdownShadowPlot==FALSE) {
        remove(drawdowns)
        remove(drawdowns.dates)
      }
    } else {
      print(paste("Calculating Charts:", "Margin Chart"))
      mPlot.par <- par(mar=c(1,4,4,2))
      mPlot <- chart.CumReturns(returns, main = "PerformanceCharts", colorset=c(2,3,4), 
        legend.loc = "topleft", geometric=TRUE, ylab="Cumulative Simple Returns"
        )
    }
  }
  if (DrawdownPlot==TRUE) {
    print(paste("Calculating Charts:", "Drawdown Chart"))
    n <- n+1
    dPlot.par <- par(mar=c(5,4,0,2))
    dPlot <- chart.Drawdown(returns, main = "", colorset=c(2,3,4), legend.loc = "topleft", geometric=TRUE, ylab = "Drawdowns")     
  }  
  if (ReturnsPlot==TRUE) {
    print(paste("Calculating Charts:", "Simple Returns Chart"))
    n <- n + 1
    rPlot.par <- par(mar=c(1,4,0,2))
    rPlot <- chart.BarVaR(returns, main = "", ylab = paste(date.label,"Returns"))
  }
  if (CandlePlot==TRUE) {
    print(paste("Calculating Charts:", "Drawdown Length in Candles Chart"))
    if (drawdownShadow.plot==FALSE) {
      drawdowns <- DrawdownDataSet(returns, days=FALSE)
    } 
    drawdowns <- drawdowns[order(drawdowns$From), ]
    rownames(drawdowns) <- 1:nrow(drawdowns)
    firstRun <- TRUE
    for (i in seq(1:nrow(drawdowns))) {
      time <- seq(drawdowns$From[i], drawdowns$To[i], by=period )
      candles <- seq(from=0, to=drawdowns$Length[18], by=drawdowns$Length[18]/length(time))
      candles <- candles[-1]
      candles <- data.frame(Time=time, Length=candles2)
      cxts <- xts(candles[,-1], order.by=candles[,1])
      if(firstRun){
        firstRun <- FALSE
        final.candles <- candles 
      } else {
        final.candles <- rbind(final.candles, candles)
      }
    }
    remove(drawdowns)
    remove(drawdowns.dates)
    n <- n + 1
    cPlot.par <- par(mar=c(1,4,0,2))
    cPlot <- plot(final.candles)
  }
  print(paste("Calculating Charts:", "Build Form"))
  charts.matrix <- seq(1:n)
  layout(matrix(charts.matrix), heights = c(2, 1, 1.3), widths = 1)
  if (exists("mPlot")) {
    print(paste("Painting Chart:", "Margin Chart"))
    mPlot.par
    mPlot
  }
  if (exists("rPlot")) {
    print(paste("Painting Chart:", "Simple Returns Chart"))
    rPlot.par
    rPlot
  }
  if (exists("dsPlot")) {
    print(paste("Painting Chart:", "DrawdownShadow Chart"))
    dsPlot.par
    dsPlot
  }
  if (exists("dPlot")) {
    print(paste("Painting Chart:", "Dropdown Chart"))
    dPlot.par
    dPlot
  }
  if (exists("cPlot")) {
    print(paste("Painting Chart:", "Candles Chart"))
    cPlot.par
    cPlot
  } 
}
