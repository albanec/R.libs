# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции работы с серверами данных:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция загрузки списка котировок за период from/to.date + сохранения в файлы
#' 
#' @param tickers CSV или вектор с тикерами
#' @param from.date Дата начала (даты в формате "2015-01-01")
#' @param to.date Дата конца (даты в формате "2015-01-01")
#' @param period Вектор периодов (или единичное значение; из вектора будет выбрано min значение)
#' @param maxattempts Количество попыток загрузки (для каждого тикера)
#'
#' @return data.list Лист с XTS по котировкам (+ все котировки сохраняются в csv файлы)
#'
#' @export
GetData_Ticker_Set <- function(tickers = "TickerList.csv", from.date, to.date, period, 
                               maxattempts = 5, rename = FALSE, dir) {
  # Зависимости:
  require(rusquant)
  # ----------
  #
  old.dir <- getwd()
  setwd(dir)
  cat("INFO(GetData_Ticker_Set):  Current work.dir:", getwd(), "\n")  
  if (all(grepl(".csv", tickers)) == TRUE) {
  cat("INFO(GetData_Ticker_Set):  Loading Tickers: ", tickers, "\n")
  tickers <- 
    read.csv(tickers, header = F, stringsAsFactors = F) %>%
    .[, 1]   
  cat("INFO(GetData_Ticker_Set):  Loading Tickers: OK", "\n") 
  } 
  n.ticker <- length(tickers)
  n.period <- length(period)
  # если фреймы - вектор, то 
  period.min <- period[1]
  FirstTime <- TRUE 
  for (i in 1:n.ticker) {
  # цикл загрузки с max количеством попыток
  for (t in 1:maxattempts) {
    cat("INFO(GetData_Ticker_Set):  (", i ,"/", n.ticker, ")", 
      "Downloading: ", tickers[i], "  Attempt: ", t ,"/", maxattempts, "\n")
    data <- GetData_Ticker_One(ticker = tickers[i], from.date = from.date, to.date = to.date, 
                 period = period.min, rename)
    if (exists("data")) {
     cat("INFO(GetData_Ticker_Set):  (", i ,"/", n.ticker, ")", 
       "Downloading ", tickers[i], "  complete", "\n")
     break
    }
  }
  data <- na.omit(data)
  data.name <- as.character(tickers[i])
  Save_XTStoCSV(data = data, filename = data.name, period = period.min)
  assign(paste(data.name, period.min, sep="."), data)
  remove(data); remove(data.name)
  }
  tickers <- sapply(tickers, function(x) { paste(x, period.min, sep = ".") })
  tickers.temp <- paste(tickers, collapse = ",")
  temp.text <- paste("data.list <- list(", tickers.temp, ") ;",
       "names(data.list) <-  tickers",  
       sep = "")
  eval(parse(text = temp.text))
  setwd(old.dir)
  return(data.list)
}
#
###
#' Функция загрузки тикера с Финам + (если нужно) переименовывает столбцы
#' 
#' @param ticker Нужный тикер
#' @param from.date Дата-старт (даты в формате "2015-01-01")
#' @param to.date Дата-стоп (даты в формате "2015-01-01")
#' @param period Период свечей
#' @param rename Нужно ли переименовывать (T/F)
#'  
#' @return data XTS массив
#'
#' @export
GetData_Ticker_One <- function(ticker, period = "15min", 
                 from.date, to.date = Sys.Date(), rename = FALSE) {
  # Зависимости:
  require(rusquant)   
  # ----------
  #
  cat("INFO(GetData_Ticker_One):  ", "Download Source Data...", "\n")
  data <- getSymbols(ticker, from = from.date, to = to.date, period = period, src = "Finam", auto.assign = FALSE)
  if (is.xts(data) !=  TRUE) {
  stop(paste("ERROR(GetData_Ticker_One):  ticker ", ticker, " not present!!!", sep = ""))
  }
  if (rename == TRUE) {
  names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")  
  }
  return(data)
}
#
