# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для анализа параметров бектеста из Lab'ов
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
EVA_BotReitLabsFile <- function (data, bot.num.table) {
  # ----------
  # Общее описание:
  #   функция вычисления рейтингов ботов
  # Входные данные:
  #   data: данные с рядом id-ботов (var0)
  #   bot.num.table: таблица с номерами ботов
  # Выходные данные:
  #   table: таблица номерами ботов и их рейтингами
  # ----------
  FirstTime <- TRUE
  for (i in 1:nrow(bot.num.table)) {
    bot.reit <- 0
    bot.sum <- 0
    for (n in 1:nrow(data)){
      if (data$var0[n] == bot.num.table[i,1]) {
        bot.reit <- bot.reit + 1
        bot.sum <- bot.sum + data$profit.norm[n]
      }   
    }
    x <- c(i, bot.reit, bot.sum)
    if (FirstTime == TRUE) {
      table <- x 
      FirstTime <- FALSE
    } else {
      table <- cbind(table, x)
    }        
  }
  table <- t(table)
  return (table)
}
EVA_PerReitLabsFile <- function (data) {
  # ----------
  # Общее описание:
  #   функция вычисления рейтингов периодов
  # Входные данные:
  #   data: данные 
  # Выходные данные:
  #   table: таблица номерами ботов и их рейтингами
  # ----------
  FirstTime <- TRUE
  for (i in 1:300) {
    bot.reit <- 0
    bot.sum <- 0
    for (n in 1:nrow(data)) {
      if (i == data$var3[n]) {
        bot.reit <- bot.reit + 1
        bot.sum <- bot.sum + data$profit.norm[n]
      }
    }        
    x <- c(i, bot.reit, bot.sum)
    if (FirstTime == TRUE) {
      table <- x 
      FirstTime <- FALSE
    } else {
      table <- cbind(table, x)
    }
  }       
  table <- t(table)
  return (table)
}
#
EVA_3DChartLabsFile <- function (data, var1, var2, var3, color) {
  # ----------
  # Общее описание:
  #   функция построения 3D графиков для LabsFile
  # Входные данные:
  #   data: данные 
  #   var1, var2, var3, color: номера столбцов переменных и цвета
  # Выходные данные:
  #   p: график
  # Зависимости:
  require(plotly)
  # ----------
  mycolors <-  rainbow(30, start = 0.3, end = 0.95)
  p <- plot_ly(data, x = var1, y = var2, z = var3, 
               type = "scatter3d", mode = "markers", color = color, 
               colors = mycolors, marker = list(size = 4))  
  return(p)
}
#
EVA_ReitChartLabsFile <- function (data, bot = FALSE, per = FALSE) {
  # ----------
  # Общее описание:
  #   функция графиков рейтингов
  # Входные данные:
  #   data: таблица с рейтингами (per ил bot)
  # per/bot: какой график строить
  # Выходные данные:
  #   p: график
  # Зависимости:
  require(plotly)
  # ----------
  if (per == TRUE) {
    p <- plot_ly(x = data[, 2], y = data[, 1], mode = "markers", color = data[,3], colors=mycolors)  
  }
  if (bot == TRUE) {
    p <- plot_ly(x = data[,1], y = data[,2], mode = "markers", color = data[,3], colors=mycolors)  
  }
  return(p)
}
#
EVA_3Dto2DChartsMatrixLabsFile <- function (data, plot.num) {
  # ----------
  # Общее описание:
  #   функция раскладывает 3D график на набор 2D графиков
  # Входные данные:
  #   data: данные
  # plot.num: рандомное число для загрузки графиков на сервер
  # Выходные данные:
  #   p: матрица гарфиков
  # Зависимости:
  require(plotly)
  # ----------
  data <- data[order(-data$var3), ]
  new.begining <- TRUE
  plot.vector <- c()
  mycolors <-  rainbow(30, start=0.3, end=0.95)
  FirstTime <- TRUE
  for ( i in 1:(nrow(data)-1)) {
    if (data$var3[i] == data$var3[i+1]) {
      per.num <- data$var3[i]
      if (FirstTime == TRUE) {
       temp.data <- data[i, ]
       FirstTime <- FALSE
      } else {
        temp.data <- rbind(temp.data, data[i, ])
      }
    } else {
       per.num <- data$var3[i]
      FirstTime <- TRUE
      new.begining <- TRUE
      if (i == 1) {
        temp.data <- data[i, ]
      } else {
         temp.data <- rbind(temp.data, data[i, ])    
        }
      temp.plot.name <- paste("plot", per.num, sep=".")
      temp.data.name <- paste("temp.data", per.num, sep=".")
      temp.yaxis.name <- paste("yaxis", per.num, sep=".")
      temp.xaxis.name <- paste("xaxis", per.num, sep=".")
      plot.vector <- c(plot.vector, temp.plot.name)
      assign(temp.data.name, temp.data)
      assign(temp.yaxis.name, list (title = paste("PER: ", per.num)))
      assign(temp.xaxis.name, list (title = paste(" ")))
      temp.text <- paste(temp.plot.name, "<- plot_ly(", temp.data.name, ", x = var1, y = var2, 
                         mode = \"markers\", color = profit.norm, colors = mycolors) %>% layout(yaxis =", temp.yaxis.name , 
                         ", xaxis =", temp.xaxis.name, ", showlegend = FALSE)", sep = "")
      eval(parse(text = temp.text)) 
    }
  }
  plot.count <- length(plot.vector)
  cat("numbers of plot:", plot.count, "\n")
  FirstTime <- TRUE
  cat("printing plot:")
  for (i in 1:plot.count) {
    cat(i, " ")
    if (FirstTime == TRUE) {
      FirstTime <- FALSE
      temp.plot.name <- paste(plot.vector[i])
    } else {
      temp.plot.name <- paste(temp.plot.name, plot.vector[i], sep=",")
    }
  }
  cat("\n")
  nrow.plot.matrix <- round(plot.count/6)+1
  cat("building plot matrix with", nrow.plot.matrix, "rows", "\n")
  temp.text <- paste("subplot(", 
                     temp.plot.name, ", nrows = ", nrow.plot.matrix, ")", sep="")
  p <- eval(parse(text = temp.text))
  cloud.filename <- paste("r-docs/public-graph/", plot.num, ".bigplot", sep="")
  plotly_POST(p, filename = cloud.filename)
  return (p)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для анализа параметров бектеста
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Расчет временных метрик:
EVA_DateTable <- function (states.data) {
  # ----------
  # Общее описание:
  #   функция свода временных параметров работы стратегии
  # Входные данные:
  #   states.data: ряд сделок
  # Выходные данные:
  #   metric.table: таблица метрик
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  
}
# 
# Расчет коэффициентов продуктивности:
EVA_RatioTable <- function (returns.data) {
  # ----------
  # Общее описание:
  #   функция вычисления стандартного наборов метрик продуктивности результатов работы стратегий
  # Входные данные:
  #   returns.data: ряд доходностей
  # Выходные данные:
  #   metric.table: таблица метрик
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  # расчет коэффициентов:
  # SharpeRatio
  sharp.data <-  SharpeRatio.annualized(returns.data, scale = 1)
  # SortinoRatio
  sortino.data <- SortinoRatio(returns.data)
  # CalmarRatio
  calmar.data <- CalmarRatio(returns.data, scale = 1)
  # SterlingRatio
  sterling.data <- SterlingRatio(returns.data, scale = 1)
  # формирование таблицы
  metric.table <- cbind(EVA_TransformMetric(sharp.data, name = "SharpRatio"), 
                        EVA_TransformMetric(sortino.data, name = "SortinoRatio"), 
                        EVA_TransformMetric(calmar.data, name = "CalmarRatio"),
                        EVA_TransformMetric(sterling.data, name = "SterlingRatio") )
  #pMetric[,1] <- 
  return(metric.table)
}
# Вспомогательные фунции:
EVA_TransformMetric <- function (metric.data, metric.name) {
  # ----------
  # Общее описание:
  #   функция трансформации данных метрик к одному виду (нужна для RatioTable)
  # Входные данные:
  #   metric.data: данные метрики
  #   metric.name: название метрики
  # Выходные данные:
  #   metric.data: транформированная в столбец метрика
  # ----------
  #
  cat("Calculating Performance Metric:", metric.name, "\n")
  metric.data <- as.matrix(metric.data)
  # трансформация в нужный вид:
  if (nrow(metric.data) == 1) {
    metric.data <- t(metric.data)
  }
  colnames(metric.data) <- metric.name
  rownames(metric.data) <- ""
  return (metric.data)
}
# Расчет Drawdown метрик:
EVA_DrawdownDataSet <- function (returns.data, days = TRUE) {
  # ----------
  # Общее описание:
  # функция возращает таблицу просадок + кол-во дней в просадке (формирует ряд для анализа)
  # Входные данные:
  #   returns.data: ряд доходностей
  # Выходные данные:
  #   drawdowns: таблица просадок
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  drawdowns <- table.Drawdowns(returns.data[, 1], top = 1000000)
  if (days == TRUE) {
    for (i in seq(1:nrow(drawdowns))) {
      drawdowns$Days <- as.numeric(-floor(difftime(drawdowns$From, drawdowns$To, units = "days"))) 
    }  
  }
  return (drawdowns)
}
#
EVA_DrawdownTable <- function (returns.data, plot = FALSE, period = "15") {
  # ----------
  # Общее описание:
  # вычисляет параметры по просадкам (выводит итоговые данные)
  # Входные данные:
  #   returns.data: ряд доходностей
  #   plot: TRUE/FALSE график
  #   period: период свечей (в минутах)  
  # Выходные данные:
  #   drawdown.table: фрейм с данными по просадке
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  # подготовка данных
  cat("Calculating Drawdown Metric:", "Drawdown Data Set", "\n", sep = "  ")
  drawdowns <- EVA_DrawdownDataSet(returns.data, days = TRUE)
  # max просадка
  cat("Calculating Performance Metric:", "MaxDrawdown", "\n", sep = "  ")
  max.drawdown <- as.numeric(drawdowns$Depth[1])
  # средняя просадка
  cat("Calculating Performance Metric:", "MeanDrawdown", "\n", sep = "  ")
  mean.drawdown <- as.numeric(mean(drawdowns$Depth))
  # max длина просадки в днях
  cat("Calculating Performance Metric:", "MaxDrawdownDays", "\n", sep = "  ")
  max.drawdown.days <- as.numeric(drawdowns$Days[which.max(drawdowns$Days)])
  # среднее число дней в просадке
  cat("Calculating Performance Metric:", "MeanDrawdownDays", "\n", sep = "  ")
  mean.drawdown.days <- as.numeric(trunc(mean(na.omit(drawdowns$Days))))
  # текущее число дней в просадке
  cat("Calculating Performance Metric:", "NowDrawdownDays", "\n", sep = "  ")
  now.drawdown.days <- as.numeric(-floor(-(drawdowns$Length[which(is.na(drawdowns$To))] * as.numeric(period) / 60 / 24)))
  # текущее число свечей в просадке
  cat("Calculating Performance Metric:", "NowDrawdownPeriods", "\n", sep = "  ")
  now.drawdown.periods <- as.numeric(-floor(-(drawdowns$Length[which(is.na(drawdowns$To))] )))
  # текущая просадка 
  cat("Calculating Performance Metric:", "NowDrawdown", "\n", sep = "  ")
  now.drawdown <- as.numeric(DrawdownPeak(last(returns.data)))
  # формирование таблицы
  drawdown.table <- cbind(max.drawdown, mean.drawdown, max.drawdown.days, 
                          mean.drawdown.days, now.drawdown.days, now.drawdown.periods, now.drawdown)
  drawdown.table <- data.frame(drawdown.table)
  colnames(drawdown.table) <- c("MaxDrawdown", "MeanDrawdown" , "MaxDrawdownDays", 
                                "MeanDrawdownDays", "NowDrawdownDays", "NowDrawdownPeriods", "NowDrawdown")  
  return(drawdown.table)
}
#
# Вывод графиков:
EVA_BigPlot <- function(returns, MarginPlot = FALSE, ReturnsPlot = FALSE, DrawdownShadowPlot = FALSE, DrawdownPlot = FALSE, CandlePlot = FALSE, period = "15 min") {
  # ----------
  # Общее описание:
  # 
  # Входные данные:
  # 
  # Выходные данные:
  # 
  # Зависимости:
  require(PerformanceAnalytics)
  require(plotly)
  # ----------
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
  if (MarginPlot == TRUE) {
    n <- n + 1
    if (drawdownShadow.plot == TRUE) {
      print(paste("Calculating Charts:", "Margin & DrawdownShadow Chart"))
      drawdowns <- EVA_DrawdownDataSet(returns, days = FALSE)
      drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
      drawdowns.dates[is.na(drawdowns.dates)] <- format(index(returns)[NROW(returns)])
      drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])  
      mPlot.par <- par(mar = c(1,4,4,2))
      mPlot <- chart.CumReturns(  returns, main = "PerformanceCharts", colorset = c(2,3,4), 
        period.areas = drawdowns.dates, period.color = rgb[1],
        legend.loc = "topleft", geometric = TRUE, ylab = "Cumulative Simple Returns"
        )
      if (DrawdownShadowPlot == FALSE) {
        remove(drawdowns)
        remove(drawdowns.dates)
      }
    } else {
      print(paste("Calculating Charts:", "Margin Chart"))
      mPlot.par <- par(mar = c(1,4,4,2))
      mPlot <- chart.CumReturns(returns, main = "PerformanceCharts", colorset = c(2,3,4), 
        legend.loc = "topleft", geometric = TRUE, ylab = "Cumulative Simple Returns"
        )
    }
  }
  if (DrawdownPlot == TRUE) {
    print(paste("Calculating Charts:", "Drawdown Chart"))
    n <- n+1
    dPlot.par <- par(mar = c(5,4,0,2))
    dPlot <- chart.Drawdown(returns, main = "", colorset = c(2,3,4), legend.loc = "topleft", geometric = TRUE, ylab = "Drawdowns")     
  }  
  if (ReturnsPlot == TRUE) {
    print(paste("Calculating Charts:", "Simple Returns Chart"))
    n <- n + 1
    rPlot.par <- par(mar = c(1,4,0,2))
    rPlot <- chart.BarVaR(returns, main = "", ylab = paste(date.label, "Returns"))
  }
  if (CandlePlot == TRUE) {
    print(paste("Calculating Charts:", "Drawdown Length in Candles Chart"))
    if (drawdownShadow.plot == FALSE) {
      drawdowns <- EVA_DrawdownDataSet(returns, days = FALSE)
    } 
    drawdowns <- drawdowns[order(drawdowns$From), ]
    rownames(drawdowns) <- 1:nrow(drawdowns)
    firstRun <- TRUE
    for (i in seq(1:nrow(drawdowns))) {
      time <- seq(drawdowns$From[i], drawdowns$To[i], by = period )
      candles <- seq(from = 0, to = drawdowns$Length[18], by = drawdowns$Length[18]/length(time))
      candles <- candles[-1]
      candles <- data.frame(Time = time, Length = candles2)
      cxts <- xts(candles[,-1], order.by = candles[,1])
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
    cPlot.par <- par(mar = c(1,4,0,2))
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
#

