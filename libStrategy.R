# описания стратегий и вспомагательных функций

#####################
				####################
#
STR_SigToState <- function(x) {
	# ----------
	# Общее описание:
	# 	функция для перехода к состояниям (фильтрация сигналов)
	# Входные данные:
	# 	x - ряд сигналов стратегии
	# Выходные данные:
	#	ряд сделок (отфильтрованный ряд сигналов)
	# ----------
	#
	x$a <- na.locf( x )
	x$a <- ifelse( is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a )
	ind <- which( x$a != lag(x$a) )
	x$y <- rep( NA, length(x$a) )
	x$y[ind] = x$a[ind]
	x$y[1] <- x$a[1]
	return(x$y)
}
#
STR_CrossForVector <- function(x1,x2) {
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
	return(as.logical(c(0,x)))
}
#
STR_CrossForXTS <- function(x1,x2) {
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
	return(sign(x))
}
#
STR_CalcState <- function(data) {
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
STR_StateData <- function (data) {
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
	data$state <- STR_CalcState(data)
	state.data <- na.omit(data)
	return (state.data)
}
#
STR_CalcReturns <- function (data, SR=FALSE, LR=FALSE) {
	# ----------
  	# Общее описание:
  	#   функция расчета доходностей
 	# Входные данные:
 	#   data: котировки
  	# Выходные данные:
  	#   data: таблица котировок с рассчитанными доходностями
  	# Зависимости:
  	require(quantmod) 
  	# ----------
	cat("Calculating Returns: ")
	if (SR == TRUE) {
		cat(" SR")
		data$SR <- Delt(data$Close, type="arithmetic")		
	}
	if (LR == TRUE) {
		cat(" LR")
		data$LR <- Delt(data$Close, type="log")
	}
	cat("\n")
	data <- na.omit(data)
	return (data)
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
	return(data)
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
Strategy_PSARand2SMA <- function (data, slow.sma, fast.sma, accel.start=0.02, accel.max=0.2) {
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
