# описания стратегий и вспомагательных функций

#####################
				####################

exrem <- function(x) {
	# функция для перехода к состояниям (фильтрация сигналов)
	x$a <- na.locf( x )
	x$a <- ifelse( is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a )
	ind <- which( x$a != lag(x$a) )
	x$y <- rep( NA, length(x$a) )
	x$y[ind] = x$a[ind]
	x$y[1] <- x$a[1]
	return(x$y)
}

calc.returns <- function(data, pip, s0=0, abs=FALSE, SR=FALSE, LR=FALSE, reinvest=TRUE) {
	# расчет доходностей
	if (abs==TRUE) { 	
		if (reinvest==TRUE) {
			data$w <- data$state[[1]] * s0/data$Open[[1]]
			data$w <- round(data$w, 0)
			data$equity <- s0
			data$margin <- 0
			for ( i in 2:nrow(data) ) { 
				data$margin[i] <- data$w[[i-1]] * ( data$Open[[i]] - data$Open[[i-1]] )
				data$equity[i] <- (data$equity[[i-1]] + data$margin[[i]]) / pip
				data$w[i] <- data$state[[i]] * data$equity[[i]] / data$Open[[i]]
				data$w[i] <- round(data$w[i], 0)
			} 
		} else {
			data$w <- 1 
			data$margin <- lag(data$state) * ( data$Open-lag(data$Open) ) / pip
			data$margin[1] <- 0
			data$equity <- cumsum(data$margin)
		}
	}
	if (SR==TRUE) {
		if (reinvest==TRUE) {
			data$SR <- lag(data$state)*Delt(data$Open, type="arithmetic")
			data$SR[1] <- 0
			data$margin <- cumprod(data$SR + 1) 
			data$margin[1] <- 0
			data$equity <- s0*data$margin
		} else {
			data$SR <- lag(data$state)*Delt(data$Open, type="arithmetic")
			data$SR[1] <- 0
			data$margin <- cumprod(data$SR + 1) - 1
			data$equity <- data$Open[[1]] * as.numeric(data$margin)
		}
	}
	if (LR==TRUE) {
		#if (reinvest==TRUE) {
			#
		#} else {
			data$LR <- lag(data$state)*Delt(data$x, type="log")
			data$LR[1] <- 0
			data$margin <- cumsum(data$LR)
			data$equity <- data$Open[[1]] * (exp(as.numeric(last(data$margin))) - 1)
		#}
	}
	return(data)
}

calc.profit <- function(data, s0=0, reinvest=TRUE) {
	# расчет итогового профита
	if (reinvest==TRUE) {
		profit <- as.numeric(last(data$equity) - s0)		
	} else {
		profit <- as.numeric(last(data$equity))	
	}
	return (profit)
}

strategy.psar.2sma <- function (data, slow.sma, fast.sma, accel.start=0.02, accel.max=0.2, state=TRUE) {
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
	if (state==TRUE) {
		data$state <- exrem(data$pos)
	} else {
		colnames(data)[colnames(data)=="pos"] <- "state"
	}
	data <- na.omit(data)
	return(data)
}		
