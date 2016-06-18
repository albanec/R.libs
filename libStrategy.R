# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# описания стратегий и вспомагательных функций
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
STR_Convert_SigToState <- function (x) {
	# ----------
	# Общее описание:
	# 	функция для перехода к состояниям (фильтрация сигналов)
	# Входные данные:
	# 	x - ряд сигналов стратегии
	# Выходные данные:
	#	ряд сделок (отфильтрованный ряд сигналов)
	# ----------
	#
	x$a <- na.locf(x)
	x$a <- ifelse(is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a)
	ind <- which(x$a != lag(x$a))
	x$y <- rep(NA, length(x$a))
	x$y[ind] = x$a[ind]
	x$y[1] <- x$a[1]
	return (x$y)
}
#
STR_CrossLine_ForVector <- function (x1,x2) {
	# ----------
	# Общее описание:
	# 	функция вычисляет пересечения графиков векторов
	# Входные данные:
	# 	x1 - вектор1
	#	x2 - вектор2
	# Выходные данные:
	#	вектор пересечений
	# ----------
	#
	x <- diff(x1>x2)
	x[1] <- 0
	x[x<0] <- 0
	x <- as.logical(c(0,x))
	return (x)
}
#
STR_CrossLine_ForXTS <- function (x1,x2) {
	# ----------
	# Общее описание:
	# 	функция вычисляет пересечения графиков рядов
	# Входные данные:
	# 	x1 - xts1
	#	x2 - xts2
	# Выходные данные:
	#	ряд пересечений
	# ----------
	# 
	x <- diff(x1>x2)
	x[1] <- 0
	x[x<0] <- 0
	x <- sign(x)
	return (x)
}
#
STR_CalcState_Data <- function(data) {
	# ----------
  	# Общее описание:
  	# функция для перехода к состояниям (фильтрация сигналов)
  	# Входные данные:
 	#   data: ряд позиций (data$pos)
  	# Выходные данные:
  	# data$state: ряд состояний  
  	# Зависимости:
  	require(quantmod) 
	# ----------
	data$pos <- na.locf(data$pos)
	data$pos <- ifelse(is.na(data$pos) | is.nan(data$pos) | is.infinite(data$pos), 0, data$pos)
	ind <- which(data$pos != lag(data$pos))
	data$state <- rep(NA, length(data$pos))
	data$state[ind] <- data$pos[ind]
	data$state[1] <- data$pos[1]
	return (data$state)
}
#
STR_CalcState_Table <- function (data) {
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
	data$state <- STR_CalcState_Data(data)
	state.data <- na.omit(data)
	return (state.data)
}
#
STR_CalcReturn_inXTS <- function(data, type = "simret") {
	# ----------
	# Общее описание:
	# 	функция вычисляет return'ы по всему портфелю внутри XTS
	# Входные данные:
	# data: XTS 
	# type: тип return'a (ret/simret/logret)
	# Выходные данные:
	#	data: XTS + return'ы по каждому инструменту 
	# Зависимости:
	require(quantmod)
	# ----------
	data.names <- names(data)[grep("Close", names(data))]
	data.names <- sub(".Close", "", data.names)
	for (i in 1:length(data.names)) {
		temp.text <- paste("data$",data.names[i],".",type," <- ",
						   "CalcReturn(data$",data.names[i],".Close, type = \"",type,"\")", 
						   sep="")
		eval(parse(text = temp.text))
	}
	return(data)
}
# 
STR_CalcEquity <- function (data, s0 = 0, abs = FALSE, SR = FALSE, LR = FALSE, reinvest = TRUE, state = FALSE) {
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
			for ( i in 2:nrow(data) ) { 
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
	return (data)
}
#
STR_CalcProfit <- function (data, s0 = 0, pip, reinvest = TRUE) {
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
STR_PSARand2SMA <- function (data, slow.sma, fast.sma, accel.start=0.02, accel.max=0.2) {
 	require(quantmod) 
	# описание psar.2sma стратегии 
	data$sma <- SMA(Cl(data), slow.sma)
	data$fma <- SMA(Cl(data), fast.sma)
	data$sar <- SAR(data, accel = c(accel.start, accel.max))
	data$sig.sma <- ifelse( 
		data$fma > data$sma, 1, 
		ifelse(data$fma < data$sma, -1, 0)
	)
	data$sig.sar <- ifelse(
		data$Close > data$sar, 1, 
		ifelse(data$Close < data$sar, -1, 0)
	)
	data$pos <- sign(data$sig.sma + data$sig.sar)
	data$pos <- lag(data$pos)
	data <- na.omit(data)
	return(data)
}		
#
STR_NormData_EquityPrice_Col <- function(data, usdrub, tick.type, tick.price, convert.to) {
	# ----------
    # Общее описание:
    # Функция для расчёта стоимости тиков
    # Входные данные:
    # data: данные котировок
    # norm.data: данные USDRUB_TOM
    # tick.type: USD/RUB 
    # tick.price: стоимость одного тика
    # Выходные данные:
    # data: основной xts 
	# ----------
	if (tick.type == "USD") {
		if (convert.to == "USD") {
			data <- data * tick.price	
		} else {
			data <- data * tick.price * usdrub 
		}
	} else {
		if (convert.to == "USD") {
			data <- data / usdrub
		} else {
			data <- data * tick.price	
		}	
	}
	return(data)
}
#
STR_TestStrategy <- function(data.source.list, tickers = c("SPFB.SI", "SPFB.RTS", "SPFB.BR",
							 sma.per, add.per)) {
	require(quantmod)
	# тикер-индикатор: SI
	# основа для данных стратегии
	data <- xts()
	# добавляем индикаторы
	cat("Calculate SMA with period:  ", sma.per, "\n")
	data$sma <- SMA(data.source.list[[1]]$SPFB.SI.Close, sma.per)
	cat("Calculate $sig and $pos...", "\n")
	data$sig <- ifelse((data$sma < data.source.list[[1]]$SPFB.SI.Close), 1, 
					   ifelse(data$sma > data.source.list[[1]]$SPFB.SI.Close, -1, 0))
	data <- na.omit(data)
	# позиции зависят только от SMA
	data$pos <- lag(data$sig)
	data$pos[1] <- 0
	# сигналы на сброс позиций (1)
	cat("Calculate $sig.drop...", "\n")
	data$sig.drop <- ifelse((((data$sma > data.source.list[[1]]$SPFB.SI.Low) & (data$sig == 1)) | 
							 ((data$sma < data.source.list[[1]]$SPFB.SI.High) & (data$sig == -1))
							) & (data$sig == data$pos), 
							1, 0)
	data$diff.sig <- diff(data$sig)
	data$diff.sig[1] <- data$sig[1]
	data$temp.num <- cumsum(abs(sign(data$diff.sig)))
	temp.num.vector <- seq(1:max(data$temp.num))
	# расчет сигналов на добор позиций (1)
		# нумерация тиков внутри позиций/сделок
	data.temp <- list()
	data.temp <- sapply(temp.num.vector, 
						function(x, y) {
							xts(cumsum(abs(sign(which(data$temp.num == x)))), 
							order.by = index(data$temp.num[data$temp.num == x]))
						})
	data$sig.ticks <- NA
	data$sig.ticks <- MergeData_inList_byRow(data.temp)
	data$pos.num <- lag(data$temp.num)
		data$pos.num[1] <- 0
	data$pos.ticks <- lag(data$sig.ticks)
		data$pos.ticks[1] <- 0
	 
	# удаляем мусор
	remove(temp.num.vector); remove(data.temp); data$temp.num <- NULL
	# нумерация сигналов на расширение позиций 
	data$temp.num <- data$sig.ticks %/% add.per
	# выделение сигналов
	data$sig.add <- sign(data$temp.num) * abs(sign(diff(data$temp.num)))
		data$sig.add[1] <- 0
	data$sig.add.num <- data$temp.num * abs(sign(diff(data$temp.num)))
		data$sig.add.num[1] <- 0
	data$pos.add <- lag(data$sig.add)
		data$pos.add[1] <- 0
	data$pos.add.num <- lag(data$sig.add.num)
		data$pos.add.num[1] <- 0
data$pos.drop <- lag(data$sig.drop)

1	##
	data$pos.add[data$pos.add == data$pos.drop] <- 0
	data$pos.drop[data$pos.add == data$pos.drop] <- 0
	##
	# вывод транзакций 
	data$diff.pos <- data$pos - lag(data$pos)
	# расчет состояний 
	data$state <- STR_Convert_SigToState(x = data$pos)
	#data$test <- data$pos - lag(data$pos)
	
	
}