# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# описания стратегий и вспомагательных функций
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
STR_Convert_SigToState <- function(x) {
  # ----------
  # Общее описание:
  #  функция для перехода к состояниям (фильтрация сигналов)
  # Входные данные:
  #  x - ряд сигналов стратегии
  # Выходные данные:
  #  ряд сделок (отфильтрованный ряд сигналов)
  # ----------
  x$a <- 
    na.locf(x) %>%
    ifelse(is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a)
  ind <- which(x$a != lag(x$a))
  x$y <- rep(NA, length(x$a))
  x$y[ind] = x$a[ind]
  x$y[1] <- x$a[1]
  return (x$y)
}
#
STR_CrossLine_ForVector <- function(x1,x2) {
    # ----------
    # Общее описание:
    #  функция вычисляет пересечения графиков векторов
    # Входные данные:
    #  x1 - вектор1
    #  x2 - вектор2
    # Выходные данные:
    #  вектор пересечений
    # ----------
    #
    x <- diff(x1 > x2)
    x[1] <- 0
    x[x < 0] <- 0
    x <- as.logical(c(0, x))
    return(x)
}
#
STR_CrossLine_inXTS <- function(x1,x2) {
    # ----------
    # Общее описание:
    #  функция вычисляет пересечения графиков рядов
    # Входные данные:
    #  x1 - xts1
    #  x2 - xts2
    # Выходные данные:
    #  ряд пересечений
    # ----------
    # 
    x <- diff(x1 > x2)
    x[1] <- 0
    x[x < 0] <- 0
    x <- sign(x)
    return(x)
}
#
STR_CalcState_Data <- function(data) {
  # ----------
  # Общее описание:
  #  функция для перехода к состояниям (фильтрация сигналов)
  # Входные данные:
  #  data: ряд позиций (data$pos)
  # Выходные данные:
  #  data$state: ряд состояний  
  # Зависимости:
  require(quantmod) 
  # ----------
  data$pos <-
    na.locf(data$pos) %>%
    ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)
  ind <- which(data$pos != lag(data$pos))
  data$state <- rep(NA, length(data$pos))
  data$state[ind] <- data$pos[ind]
  data$state[1] <- data$pos[1]
  return(data$state)
}
#
STR_CalcState_Table <- function(data) {
  # ----------
  # Общее описание:
  # генерирует таблицу сделок
  # Входные данные:
  # data: данные
  # Выходные данные:
  # state.data: данные с рядом состояний  
  # Зависимости:
  require(quantmod) 
  # ----------
  state.data <- 
    data %>% STR_CalcState_Data(.) %>%
    merge(data, .) %>%
    na.omit(.)
  return(state.data)
}
#
STR_CalcReturn_inXTS <- function(data, price = "Close", type = "sret") {
  # ----------
  # Общее описание:
  #  функция вычисляет return'ы по всему портфелю внутри XTS
  # Входные данные:
  #  data: XTS 
  #  type: тип return'a (ret/sret/lret)
  # Выходные данные:
  #  data: XTS + return'ы по каждому инструменту 
  # Зависимости:
  require(quantmod)
  # ----------
  data.names <- 
    names(data)[grep("Close", names(data))] %>%
    sub(".Close", "", .)
  for (i in 1:length(data.names)) {
    temp.text <- paste("data$",data.names[i],".",type," <- ",
                       "CalcReturn(data$",data.names[i],".",price,", type = \"",type,"\")", 
                       sep="")
    eval(parse(text = temp.text))
  }
  return(data)
}
# 
STR_CalcEquity <- function(data, s0 = 0, abs = FALSE, SR = FALSE, LR = FALSE, reinvest = TRUE, state = FALSE) {
    # ----------
      # Общее описание:
      #   функция расчета equity
     # Входные данные:
     #   data: данные с доходностями и позициями
      # Выходные данные:
      #   data: данные + объем(w), относительной прибылью(margin), equity 
      # Зависимости:
      require(quantmod) 
      # ----------
    # расчет 
    if (state == FALSE) {
        data$state <- data$pos
    }
    if (abs == TRUE) {     
        if (reinvest == TRUE) {
            data$w <- data$state[[1]] * s0/data$Open[[1]]
            data$w <- trunc(data$w)
            data$equity <- s0
            data$margin <- 0
            for (i in 2:nrow(data)) { 
                data$margin[i] <- data$w[[i-1]] * ( data$Open[[i]] - data$Open[[i-1]] )
                data$equity[i] <- (data$equity[[i-1]] + data$margin[[i]])
                data$w[i] <- data$state[[i]] * data$equity[[i]] / data$Open[[i]]
                data$w[i] <- trunc(data$w[i])
            } 
        } else {
            data$w <- 1 
            data$margin <- lag(data$state) * ( data$Open-lag(data$Open) )
            data$margin[1] <- 0
            data$equity <- cumsum(data$margin)
        }
    }
    if (SR == TRUE) {
        if (reinvest == TRUE) {
            data$SR <- lag(data$state) * data$SR
            data$SR[1] <- 0
            data$margin <- cumprod(data$SR + 1) 
            data$margin[1] <- 0
            data$equity <- s0*data$margin
        } else {
            data$SR <- lag(data$state) * data$SR
            data$SR[1] <- 0
            data$margin <- cumprod(data$SR + 1) - 1
            data$equity <- data$Open[[1]] * as.numeric(data$margin)
        }
    }
    if (LR == TRUE) {
        #if (reinvest==TRUE) {
            #
        #} else {
            data$LR <- lag(data$state) * data$LR
            data$LR[1] <- 0
            data$margin <- cumsum(data$LR)
            data$equity <- data$Open[[1]] * (exp(as.numeric(last(data$margin))) - 1)
        #}
    }
    return(data)
}
#
STR_CalcProfit <- function(data, s0 = 0, pip, reinvest = TRUE) {
    require(quantmod) 
    # расчет итогового профита
    if (reinvest == TRUE) {
        profit <- as.numeric(last(data$equity / pip) - s0)        
    } else {
        profit <- as.numeric(last(data$equity / pip))    
    }
    return (profit)
}
#
STR_NormData_Price_inXTS <- function(data, names, norm.data, outnames, convert.to, tick.val, tick.price) {
  # ----------
  # Общее описание:
  # Функция для расчёта стоимости тиков внутри основного листа данных
  # Входные данные:
  # data: XTS данные котировок (основной лист данных)
  # names: список тикеров для конвертирования 
  # Выходные данные:
  # data: основной XTS (нужные данные конвертированы к нужной валюте)
  # ----------
  x <- norm.data
  for (i in 1:length(names)) {
    temp.text <- paste("data$",outnames[i]," <- ",
                       "STR_NormData_Price_byCol(data = data$",names[i],",",
                                                 "norm.data = x, convert.to = \"",convert.to,"\",",
                                                 "tick.val = ",tick.val[i],",",
                                                 "tick.price = ", tick.price[i],")",
                       sep = "")
    eval(parse(text = temp.text))  
  }
  return(data)  
}
#
STR_CalcSum_Basket_TargetPar_inXTS <- function(data, basket.weights, target) {
  #require()
  # расчёт суммарного параметра (согласно весам инструмента в портфеле)
  temp.text <- 
    names(data)[grep(target, names(data))] %>% 
    paste("data$", ., sep = "") %>%
    paste(., basket.weights, sep = " * ", collapse = " + ") %>%
    paste("data <- ", ., sep = "") 
  eval(parse(text = temp.text))
  return(data)
}
#
STR_AddData_FuturesSpecs_inXTS <- function(data, from.date, to.date, dir) {
  # ----------
  # Общее описание:
  # функция добавляет параметры инструментов (для фьючерсов: размеры ГО и курс USDRUB для пересчёта к RUB)
  # Входные данные:
  # data: XTS, сожержащий нужные данные 
  # from.date / to.date
  # Выходные данные:
  #  data: XTS ряд, с добавленными параметрами
  # ----------
  # 
  old.dir <- getwd()
  setwd(dir) 
  # загрузка ГО
  data.names <- names(data)[grep("Close", names(data))]
  data.names <- sub(".Close", "", data.names)
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
  data.USDRUB <- GetData_Ticker_One(ticker = "USD000UTSTOM", from.date, to.date, period = "day", rename = TRUE)
  data$USDRUB <- data.USDRUB$Close
  remove(data.USDRUB)
  data$USDRUB <- na.locf(data$USDRUB)
  # очистка от NA (на данном этапе na.omit полезным данным не навредит)
  data <- na.omit(data)
  setwd(old.dir)
  return(data)
}
#
STR_NormData_Price_byCol <- function(data, norm.data, convert.to, tick.val, tick.price) {
  # ----------
  # Общее описание:
  # Функция для расчёта стоимости тиков
  # Входные данные:
  # data: данные котировок
  # norm.data: данные USDRUB_TOM 
  # Выходные данные:
  # data: основной xts 
  # ----------
  if (convert.to == "RUB") {
    data <- (data * tick.price / tick.val) * norm.data
  }
  if (convert.to == "USD") {
    data <- (data * tick.price / tick.val) / norm.data  
  }
  return(data)
}
#
