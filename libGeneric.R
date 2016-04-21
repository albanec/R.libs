#
GEN_SigToState <- function(x) {
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
GEN_RepeatRow <- function(x,n){
	# ----------
	# Общее описание:
	# 	функция создает матрицу из n строк вектора x
	# Входные данные:
	# 	x - вектор, который надо повторить 
	#	n - количество нужных строк
	# Выходные данные:
	#	матрица m, состоящая их n сток вектора x
	# ----------
	#
	m <- matrix(rep(x,each = n),nrow = n)
 	return(m)
}
#
GEN_RepeatCol<-function(x,n){
	# ----------
	# Общее описание:
	# 	функция создает матрицу из n столбцов вектора x
	# Входные данные:
	# 	x - вектор, который надо повторить 
	#	n - количество нужных столбцов
	# Выходные данные:
	#	матрица m, состоящая их n столбцов вектора x
	# ----------
	# 
	m <- matrix(rep(x,each = n), ncol = n, byrow = TRUE)
	return(m)
}
#
GEN_CrossForVector <- function(x1,x2) {
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
GEN_CrossForXTS <- function(x1,x2) {
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
GEN_GetData <- function (ticker, from.date, to.date = Sys.Date(), period = "15min") {
	# ----------
	# Общее описание:
	# 	функция загрузки тикера с Финам + переименовывает столбцы
	# Входные данные:
	# 	ticker - нужный тикер
	#	from.date - дата-старт
	#	to.date - дата-стоп
	#	period - период свечей
	# Выходные данные:
	#	xts массив "data"
	# Зависимости:
		require(rusquant) 	
	# ----------
	#
	# загрузка данных
	# дата в формате "2015-01-01"
	cat("\t", "Download Source Data...", "\n")
	data <- getSymbols(ticker, from = from.date, to = to.date, period = period, src = "Finam", auto.assign = FALSE)
	names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")
	return(data)
}
#
GEN_SaveXTStoCSV <- function (data, name, period = FALSE, tframe = FALSE) {
	# ----------
	# Общее описание:
	# 	функция записи XTS рядов в .csv файл 	
	# Входные данные:
	# 	data - нужный xts
	#	name - название файла
	#	period - указать в название период свечей
	#	tframe - указать в названии номер тайм-фрейма во Framelist'e
	# Выходные данные:
	#	сохраненный .csv файл
	# Зависимости:
		require(zoo)
	# ----------	
	#
	filename <- name
	if (period !=  FALSE) {
		filename <- paste(filename, period, sep = ".")
	}
	if (tframe !=  FALSE) {
		filename <- paste(filename, tframe, sep = ".")
	}
	filename <- paste(filename, "csv", sep = ".")
	write.zoo(data, file = filename, sep = ",")
	cat("Save OK : ", "\t", filename, "\n")
}
#
GEN_ReadCSVtoXTS <- function (name, period = FALSE, tframe = FALSE) {
	# ----------
	# Общее описание:
	# 	функция чтения XTS рядов из .csv файлов
	# Входные данные:
	#	name - название файла
	#	period - указать в название период свечей
	#	tframe - указать в названии номер тайм-фрейма во FrameList'е
	# Выходные данные:
	#	xts ряд, полученный из файла
	# Зависимости:
		require(xts)
	# ----------
	# 
	filename <- name
	if (period !=  FALSE) {
		filename <- paste(name, period, sep = ".")
	}
	if (tframe !=  FALSE) {
		filename <- paste(filename, tframe, sep = ".")
	}
	filename <- paste(filename, "csv", sep = ".")
	data <- read.csv(file = filename)
	data <- xts(data[,-1], order.by = as.POSIXct(data$Index))
	cat("Read OK : ", "\t", filename, "\n")
	return(data)
}
#
GEN_GetDataTickerListCSV <- function (ticker.list = "TickerList.csv", from.date, to.date, period, maxretryattempts = 5, description = FALSE) {
	# ----------
	# Общее описание:
	# 	функция загрузки листа котировок за период from/to.date 
	# Входные данные:
	#	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период (время вида "5 min"))
	#	from.date - дата-старт
	#	to.date - дата-стоп
	#	ticker.list - файл, сожержащий список нужных тикеров
	# 	description - добавить описание к названию файла 
	# 	maxretryattempts - количество попыток загрузки каждого тикера
	# Выходные данные:
	#	.csv файлы
	# Зависимости:
		require(rusquant)
	# ----------
	#
	cat("Info: Current work.dir:", getwd())	
	stocks.list <- read.csv(ticker.list, header = F, stringsAsFactors = F) 
	stocks.data <- new.env() #Make a new environment for quantmod to store data in
	data.name.list <- GEN_StocksNameList(ticker.list = ticker.list, description)
	nstocks <- nrow(data.name.list) #The number of stocks to download
	nperiod <- length(period)
	# если фреймы - вектор, то 
	period.min <- period[1]
	FirstTime <- TRUE 
	for (i in 1:nstocks){
		# цикл загрузки с max количеством попыток
		for(t in 1:maxretryattempts){
			cat( "(", i , "/" , nstocks, ")", "Downloading: ", stocks.list[i,1] , "\t Attempt: ", t , "/", maxretryattempts,"\n")
			data <- GEN_GetData(ticker = stocks.list[i,1], from.date = from.date, to.date = to.date, period = period.min)
			if (exists("data")) {
				cat( "(", i , "/" , nstocks, ")", "Downloading ", stocks.list[i,1] , "\t complete", "\n")
  				break
			}
		}
		data <- na.omit(data)
   		data$SR <- Delt(data$Close, type = "arithmetic")
		data$SR[1] <- 0
		data$LR <- Delt(data$Close, type = "log")
		data$LR[1] <- 0
   		data.name <- as.character(data.name.list[i])
   		GEN_SaveXTStoCSV(data = data, name = data.name, period = period.min)
	}
}
#
 GEN_StocksNameList <- function (ticker.list, description = FALSE) {
	# ----------
	# Общее описание:
	# 	вспомогательная для GEN_GetTickerListData()
	# 	функция генерации листа имен тикеров (data.name.list)
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров
	# 	description - добавить описание к названию файла 
	# Выходные данные:
	#	data.name.list - frame c данными тикеров
	# ----------
	# 
 	stocks.list <- read.csv(ticker.list, header = F, stringsAsFactors = F) 
	nstocks <- nrow(stocks.list)
	FirstTime <- TRUE
	for (i in 1:nstocks){
		if (description!= FALSE) {
			data.name <- paste(stocks.list[i,1], description, sep = ".")	
		} else {
			data.name <- paste(stocks.list[i,1])
		}
   		if (FirstTime == TRUE) {
   			data.name.list <- data.name
   			data.name.list <- data.name.list
   			FirstTime <- FALSE
   		} else {
   			data.name.list <- rbind(data.name.list, data.name)
   		}
	}
	return(data.name.list)
}
#
GEN_TimeExpandData <- function(ticker.list, frame.list, period, description = FALSE) {
	# ----------
	# Общее описание:
	# 	функция выделения данных по tf и временному интервалу
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	frame.list - файл, сожержащий список нужных временных интервалов	
	# 		даты должны быть записаны в виде '2014-12-01/2014-12-31'	
	# 	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# 	description - добавить описание к названию файла 
	# Выходные данные:
	#	выдает .csv
	# ----------
	# 
	cat( "Start DataExpand by FrameList :", "\n")
	cat( "Generate DataNameList...", "\n")
	data.name.list <- GEN_StocksNameList(ticker.list, description)
	frame.list <- read.csv(frame.list, header = F, stringsAsFactors = F)
	nstocks <- nrow(data.name.list)
	nframe <- nrow(frame.list)
	nperiod <- length(period)
	period.min <- period[1]
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		cat( "Processing StocksData:", "\t", data.name, "\n")
		data.source <- GEN_ReadCSVtoXTS(name = data.name, period = period[1], tframe = FALSE)
		cat ("Expand...", "\t", data.name, "\n")
		for (n in 1:nframe) {
			# цикл time.frame'а
			cat ("Expand...", "\t", data.name, "for TimeFrame ", frame.list[n, 1], "\n")
			window <- frame.list[n, 1] 
			for (t in 1:nperiod) {
				# цикл периода
				p <- period[t]
				cat ("Expand...", "\t", data.name, "for Period ", p, "\n")
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
				data <- data.source[window]
				ends <- endpoints(data, p1, k)
				data <- data[ends]
				GEN_SaveXTStoCSV(data = data, name = data.name, period = p, tframe = n)
			}
		}
	}
	cat( "Expand StocksData...", "\t", "complete", "\n")
}
#

	# выгрузка данных
	#filename <- paste("MergedData", ticker.list, sep = ".")
	#data <- GEN_ReadCSVtoXTS (name = filename, period, tframe)
