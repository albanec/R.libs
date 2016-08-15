# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generic функции для работы с котировками: 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция умного удаления NA из XTS
#'
#' @param data Исходный XTS
#' @param type способ удаления NA (full/locf/approx)
#' @param filename Если нужэно сохранить, то определить название файла
#'
#' @return data XTS ряд, очищенный от NA (по всем тикерам)
#'
#' @export
NormData_NA_inXTS <- function(data, type="full", filename = FALSE) {
  # 
  if (is.xts(data) != TRUE) {
  stop("INFO(NormData_NA): Error in source data: DataType != .xts !!!")
  }
  if (any(is.na(data)) != TRUE) {
  cat("INFO(NormData_NA): No NA rows in data", "\n")
  } else {
  # нормализация NA-значений
  if (type == "locf") {
    # нормализация с пом-ю na.locf
    data <- na.locf(data)
  } 
  if (type == "full") {
    # нормализация по уровням свечей 
    data.names <- 
    names(data)[grep("Close", names(data))] %>%
    sub(".Close", "", .)
    for (i in 1:length(data.names)) {
    temp.text <- paste(
      "if (any(is.na(data$",data.names[i],".Close))!= TRUE) {",
      "cat(\"INFO(NormData_NA): No NA in\"",",data.names[i]", ",\"\\n\")",
      "} else {",
      "data$",data.names[i],".temp <- data$",data.names[i],".Open ; ",    
      "data$",data.names[i],".Open[is.na(data$",data.names[i],".Open)] <- ",
      "na.locf(coredata(data$",data.names[i],".Close))[is.na(data$",data.names[i],".Open)] ; ",
      "",
      "data$",data.names[i],".Close[is.na(data$",data.names[i],".Close)] <- ",
      "rev(na.locf(rev(coredata(data$",data.names[i],".temp))))[is.na(data$",data.names[i],".Close)] ; ",
      "",
      "data$",data.names[i],".High[is.na(data$",data.names[i],".High)] <- ",
        "ifelse(data$",data.names[i],".Close[is.na(data$",data.names[i],".High)] > ",
        "data$",data.names[i],".Open[is.na(data$",data.names[i],".High)],",
        "data$",data.names[i],".Close[is.na(data$",data.names[i],".High)],",
        "data$",data.names[i],".Open[is.na(data$",data.names[i],".High)]) ; ",
      "",
      "data$",data.names[i],".Low[is.na(data$",data.names[i],".Low)] <- ",
        "ifelse(data$",data.names[i],".Close[is.na(data$",data.names[i],".Low)] >",
        "data$",data.names[i],".Open[is.na(data$",data.names[i],".Low)],",
        "data$",data.names[i],".Open[is.na(data$",data.names[i],".Low)],",
        "data$",data.names[i],".Close[is.na(data$",data.names[i],".Low)]) ; ",
      "",
      "data$",data.names[i],".Volume[is.na(data$",data.names[i],".Volume)] <- 0 ; ",
      "",
      "data$",data.names[i],".temp <- NULL ; ",
      "cat(\"INFO(NormData_NA): All NA remove in\"",",data.names[i]", ",\"\\n\")", 
      "}",
      sep = "")
    eval(parse(text = temp.text))
    }
  }
  if (type == "approx") {
    # аппроксимация NA
    data <- na.approx(data)
  }
  data <- na.omit(data)
  }
  if (filename != FALSE) {
  Save_XTStoCSV(data = merged.data, filename = filename)  
  }
  #
  return(data)
}
#
###
#' Функция выделения данных по tf и временному интервалу
#'
#' @param data.list Лист с котировками в XTS
#' @param frames Файл (или вектор), сожержащий список нужных временных интервалов (в виде '2014-12-01/2014-12-31')
#' @param period Вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
#' 
#' @return 
#'
#' @export
ExpandData_toPeriod <- function(data.list, frames, period) {
  # 
  if (any(grepl(".csv", frames)) == TRUE) {
    cat("INFO(ExpandData_toPeriod):  Loading FrameList: ", frames, "\n")
    frames <- read.csv(frames, header = F, stringsAsFactors = F)
    frames <- frames[, 1]    
    cat("(ExpandData_toPeriod):  Loading FrameList: OK", "\n")
  } 
  n.frame <- length(frames)
  n.ticker <- length(data.list) 
  n.period <- length(period)
  period.min <- period[1]
  for (i in 1:n.ticker) {
    data <- data.list[[i]]
    data.name <- names(data)[grep("Close", names(data))]
    data.name <- sub(".Close", "", data.name)
    cat( "INFO(ExpandData_toPeriod):  Processing Data:  ", data.name, "\n")
    for (n in 1:n.frame) {
      # цикл time.frame'а
      cat ("INFO(ExpandData_toPeriod):  Expand...  ", data.name, "for TimeFrame ", frames[n], "\n")
      window <- frames[n] 
      for (t in 1:n.period) {
        # цикл периода
        p <- period[t]
        cat ("INFO(ExpandData_toPeriod):  Expand...  ", data.name, "for Period ", p, "\n")
        if (p == "5min") { 
          p1 <- "mins"
          k <- 5
        }
        if (p == "10min") {
          p1 <- "mins"
          k <- 10
        }
        if (p == "15min") {
          p1 <- "mins"
          k <- 15
        }
        if (p == "30min") {
          p1 <- "mins"
          k <- 30
        }
        if (p == "1hour") {
          p1 <- "hours"
          k <- 1
        }
        if (p == "1day") {
          p1 <- "days"
          k <- 1
        }
        data.temp <- data[window]
        ends <- endpoints(data.temp, p1, k)
        data.temp <- data.temp[ends]
        Save_XTStoCSV(data = data.temp, filename = data.name, period = p, tframe = n)
      }
      remove(data.temp); remove(data)
    }
  }
}
#
###
#' Функция для расчёта стоимости тиков
#' 
#' @param data Данные котировок
#' @param norm.data Данные USDRUB_TOM
#'
#' @return data Основной xts
#'
#' @export
NormData_Price_byCol <- function(data, norm.data, convert.to) {
  # 
  if (convert.to == "RUB") {
    data <- data * norm.data
  }
  if (convert.to == "USD") {
    data <- data / norm.data  
  }
  #
  return(data)
}
#
###
#' Функция добавляет параметры инструментов 
#' (для фьючерсов: размеры ГО и курс USDRUB для пересчёта к RUB)
#' 
#' @param data XTS, сожержащий нужные данные 
#' @param from.date Дата начала
#' @param to.date Дата конца
#' @param im.wd Папка с данными по ГО
#'
#' @return data XTS ряд, с добавленными параметрами
#'
#' @export
AddData_FuturesSpecs_inXTS <- function(data, from.date, to.date, im.wd) {
  # 
  # загрузка ГО
  data.names <- names(data)[grep("Close", names(data))]
  data.names <- sub(".Close", "", data.names)
  setwd(im.wd) 
  temp.data <- xts()
  for (i in 1:length(data.names)) {
    temp.text <- paste("temp.data <- Read_CSVtoXTS(filename = \"",data.names[i],".IM\") ; ",
               "data$",data.names[i],".IM <- temp.data ; ",
               "remove(temp.data) ; ",
               "data$",data.names[i],".IM <- na.locf(data$",data.names[i],".IM) ; ",
               sep="")
    eval(parse(text = temp.text))
  }
  remove(temp.text); remove(data.names); 
  # загрузка котировок USDRUB_TOM
  data.USDRUB <- GetData_Ticker_One(ticker="USD000UTSTOM", from.date, to.date, period = "day", rename = TRUE)
  data$USDRUB <- data.USDRUB$Close
  remove(data.USDRUB)
  data$USDRUB <- na.locf(data$USDRUB)
  # очистка от NA (на данном этапе na.omit полезным данным не навредит)
  data <- na.omit(data)
  #
  return(data)
}
#
###
#' Функция вычисляет return'ы 
#' 
#' @param data $ряд 
#' @param type Тип return'a (ret/sret/lret)
#'
#' @return data $ряд с return'ами 
#'
#' @export
CalcReturn <- function(data, type = "sret") {
  # ----------
  require(quantmod)
  # ----------
  if (type == "ret") {
  data <- data - lag(data)
  }
  if (type == "sret") {
  data <- Delt(data, type = "arithmetic")
  } 
  if (type == "lret") {
  data <- Delt(data, type = "log")  
  }
  data[1] <- 0
  #
  return(data)
}