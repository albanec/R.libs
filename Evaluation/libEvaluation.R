source("libEva_Ratio.R")
source("libEva_Drawdown.R")
source("libEva_DealsTable.R")
source("libEva_Labs.R")
#
CalcPerfomanceTable <- function(data, returns, 
                                from.date, to.date, period) {
  # простые временные параметры
  datesTable <- DateTable(data, from.date, to.date, period)
  # расчёт коэффициентов
  ratioTable <- RatioTable(returns, ret.type)
  # расчёт drawdown'ов
  drawdownTable <- DrawdownTable(returns, ret.type, period = period)
  #
  # итоговая таблица
  perfomanceTable <- cbind.data.frame(datesTable, ratioTable, drawdownTable)
  return(perfomanceTable)
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для анализа параметров бектеста
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Таблица временных параметров работы стратегии
#'
#' Формирует таблицу временных параметров работы стратегии
#' 
#' @param data xts с данными отработки стратегии
#' @param from.date Дата начала торговли
#' @param to.date Дата окончания торговли
#' @param period Период свечей
#'  
#' @return datesTable Таблица с временными параметрами
#'
#' @export
DateTable <- function(data, from.date, to.date, period) {
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  cat("INFO(DateTable):  Build Date Metrics", "\n", sep = "  ")
  trading.days <- CalcTradingDays(data)
  datesTable <- cbind.data.frame(from.date, to.date, period, trading.days)
  colnames(datesTable) <- c("Начальная дата", "Конечная дата", "Период", "Число торговых дней")
  return(datesTable)
}
#
###
#' Вычисление торговых дней
#'
#' Возвращает количество торговых дней за период  
#' 
#' @param data xts с данными отработки стратегии
#'  
#' @return tradingDays Число торговых дней
#'
#' @export
CalcTradingDays <- function(data) {
  #
  tradingDays <- 
    index(data) %>%
    ndays(.)
  return(tradingDays)
}
#



# #
# # Вывод графиков:
# BigPlot <- function(returns, MarginPlot = FALSE, ReturnsPlot = FALSE, DrawdownShadowPlot = FALSE, DrawdownPlot = FALSE, CandlePlot = FALSE, period = "15 min") {
#   # ----------
#   # Общее описание:
#   # 
#   # Входные данные:
#   # 
#   # Выходные данные:
#   # 
#   # Зависимости:
#   require(PerformanceAnalytics)
#   require(plotly)
#   # ----------
#   print (paste("Calculating Metric:", "Drawdown Data Set"))
#   n <- 0
#   mPlot <- NA
#   rPlot <- NA
#   dsPlot <- NA
#   dPlot <- NA
#   cPlot <- NA
#   mPlot.par <- NA
#   rPlot.par <- NA
#   dsPlot.par <- NA
#   dPlot.par <- NA
#   cPlot.par <- NA
#   if (MarginPlot == TRUE) {
#     n <- n + 1
#     if (drawdownShadow.plot == TRUE) {
#       print(paste("Calculating Charts:", "Margin & DrawdownShadow Chart"))
#       drawdowns <- CalcDrawdownDataSet(returns, days = FALSE)
#       drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
#       drawdowns.dates[is.na(drawdowns.dates)] <- format(index(returns)[NROW(returns)])
#       drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])  
#       mPlot.par <- par(mar = c(1,4,4,2))
#       mPlot <- chart.CumReturns(  returns, main = "PerformanceCharts", colorset = c(2,3,4), 
#         period.areas = drawdowns.dates, period.color = rgb[1],
#         legend.loc = "topleft", geometric = TRUE, ylab = "Cumulative Simple Returns"
#         )
#       if (DrawdownShadowPlot == FALSE) {
#         remove(drawdowns)
#         remove(drawdowns.dates)
#       }
#     } else {
#       print(paste("Calculating Charts:", "Margin Chart"))
#       mPlot.par <- par(mar = c(1,4,4,2))
#       mPlot <- chart.CumReturns(returns, main = "PerformanceCharts", colorset = c(2,3,4), 
#         legend.loc = "topleft", geometric = TRUE, ylab = "Cumulative Simple Returns"
#         )
#     }
#   }
#   if (DrawdownPlot == TRUE) {
#     print(paste("Calculating Charts:", "Drawdown Chart"))
#     n <- n+1
#     dPlot.par <- par(mar = c(5,4,0,2))
#     dPlot <- chart.Drawdown(returns, main = "", colorset = c(2,3,4), legend.loc = "topleft", geometric = TRUE, ylab = "Drawdowns")     
#   }  
#   if (ReturnsPlot == TRUE) {
#     print(paste("Calculating Charts:", "Simple Returns Chart"))
#     n <- n + 1
#     rPlot.par <- par(mar = c(1,4,0,2))
#     rPlot <- chart.BarVaR(returns, main = "", ylab = paste(date.label, "Returns"))
#   }
#   if (CandlePlot == TRUE) {
#     print(paste("Calculating Charts:", "Drawdown Length in Candles Chart"))
#     if (drawdownShadow.plot == FALSE) {
#       drawdowns <- CalcDrawdownDataSet(returns, days = FALSE)
#     } 
#     drawdowns <- drawdowns[order(drawdowns$From), ]
#     rownames(drawdowns) <- 1:nrow(drawdowns)
#     firstRun <- TRUE
#     for (i in seq(1:nrow(drawdowns))) {
#       time <- seq(drawdowns$From[i], drawdowns$To[i], by = period )
#       candles <- seq(from = 0, to = drawdowns$Length[18], by = drawdowns$Length[18]/length(time))
#       candles <- candles[-1]
#       candles <- data.frame(Time = time, Length = candles2)
#       cxts <- xts(candles[,-1], order.by = candles[,1])
#       if(firstRun){
#         firstRun <- FALSE
#         final.candles <- candles 
#       } else {
#         final.candles <- rbind(final.candles, candles)
#       }
#     }
#     remove(drawdowns)
#     remove(drawdowns.dates)
#     n <- n + 1
#     cPlot.par <- par(mar = c(1,4,0,2))
#     cPlot <- plot(final.candles)
#   }
#   print(paste("Calculating Charts:", "Build Form"))
#   charts.matrix <- seq(1:n)
#   layout(matrix(charts.matrix), heights = c(2, 1, 1.3), widths = 1)
#   if (exists("mPlot")) {
#     print(paste("Painting Chart:", "Margin Chart"))
#     mPlot.par
#     mPlot
#   }
#   if (exists("rPlot")) {
#     print(paste("Painting Chart:", "Simple Returns Chart"))
#     rPlot.par
#     rPlot
#   }
#   if (exists("dsPlot")) {
#     print(paste("Painting Chart:", "DrawdownShadow Chart"))
#     dsPlot.par
#     dsPlot
#   }
#   if (exists("dPlot")) {
#     print(paste("Painting Chart:", "Dropdown Chart"))
#     dPlot.par
#     dPlot
#   }
#   if (exists("cPlot")) {
#     print(paste("Painting Chart:", "Candles Chart"))
#     cPlot.par
#     cPlot
#   } 
# }
# #

