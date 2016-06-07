#
PrepareForWork_Packages <- function (package.list.csv, package.list = FALSE, download = FALSE, 
									 update = FALSE) {
	# ----------
	# Общее описание:
	# 	функция загружает, устанавливает и подключает необходимые пакеты 
	# Входные данные:
	# 	package.list.csv или package.list: путь к .csv или вектор с пакетами 
	#	download, update или load: загрузить, обновить или подколючить пакеты
	# ----------
	#
	if (package.list == FALSE) {
		cat("Package List Path:", package.list.csv, "\n")	
		cat("Reading Package Lists...", "\n")
		package.list <- read.csv(package.list.csv, header = F, stringsAsFactors = F)	
		package.list <- unlist(package.list, use.names = FALSE)	
	} 
	cat("Loading Installed Package List...", "\n")
	if (update == TRUE) {
		cat ("Start Upgrading packages", "\n")
		update.packages(ask = FALSE)
		cat ("Upgrade Packages: OK", "\n")
		download <- FALSE
	} 
	if (download == TRUE) {
		package.list.temp1 <- setdiff(package.list, rownames(installed.packages()))
		if (length(package.list.temp1) > 0) {
			install.packages(package.list.temp1, dependencies = TRUE)		
			package.list.temp2 <- setdiff(package.list.temp1, rownames(installed.packages()))
			if (length(package.list.temp2) > 0) {
				warning("Installation Error!!!")
			} else {
				cat (length(diff(package.list, package.list.temp1)), " packages newly installed!", "\n")
				cat("Installation Complete", "\n")
			}
		} else {
			cat ("All Packages Already Installed!", "\n")
		}	
	}
	if (load == TRUE) {
		cat("Load Libraries to Workspace")
		lapply(package.list, library, character.only = TRUE)
		cat("Libraries Prepare to Work")
	}
}
#
Repeat_Row <- function(x,n) {
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
Repeat_Col<-function(x,n) {
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
FindMax_DistancePoint <- function (y, x=1:len(y)) {
	# ----------
	# Общее описание:
	# Входные данные:
	# Выходные данные:	
	# ----------
	# 
	all.coord <- rbind(vec(y), vec(x))
	first.point <- all.coord[, 1]
	line.vec <- all.coord[, len(y)] - first.point
	line.vec.n <- line.vec / sqrt(sum(line.vec^2))
	#	
	vec.from.first <- all.coord - first.point
	scalar.product <- line.vec.n %*% vec.from.first
	#	
	vec.from.first.parallel <- t(scalar.product) %*% line.vec.n
	vec.to.line <- t(vec.from.first) - vec.from.first.parallel
	dist.to.line <- sqrt(rowSums(vec.to.line^2, 2))
	dist <- which.max(dist.to.line)
	return(dist)
}	
#
GetData_Ticker <- function (ticker, from.date, to.date = Sys.Date(), period = "15min", rename = FALSE) {
	# ----------
	# Общее описание:
	# 	функция загрузки тикера с Финам + переименовывает столбцы
	# Входные данные:
	# 	ticker - нужный тикер
	#	from.date - дата-старт
	#	to.date - дата-стоп
	#	period - период свечей
	# rename - нужно ли переименовывать
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
	if (is.xts(data) !=  TRUE) {
		stop(paste("Downloading error: ticker ", ticker, " not present!!!", sep = ""))
	}
	if (rename ==TRUE) {
		names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")	
	}
	return(data)
}
#
Save_XTStoCSV <- function (data, name, period = FALSE, tframe = FALSE) {
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
Read_CSVtoXTS <- function (name, period = FALSE, tframe = FALSE) {
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
Read_CSVtoDF <- function (file.path, sep = ";") {
	# ----------
	# Общее описание:
	# функция считывания простых .csv
	# Входные данные:
	# file.path: путь к файлу
	# sep: тип разделителя
	# Выходные данные:
	# file: считанный файл
	# ----------
	#
	file <- read.table(file=file.path, header=F, sep = ";", as.is=T) 
	return (file)
}
#
GetData_TickerList <- function(ticker.list = "TickerList.csv", from.date, to.date, period, maxretryattempts = 5, 
							   description = FALSE, merge = FALSE) {
	# ----------
	# Общее описание:
	# 	функция загрузки листа котировок за период from/to.date 
	# Входные данные:
	# Выходные данные:
	#	.csv файлы
	# Зависимости:
		require(rusquant)
	# ----------
	#
	cat("Info: Current work.dir:", getwd())	
	n.ticker <- length(ticker.list)
	n.period <- length(period)
	if (merge == TRUE) {
		rename <- FALSE
	}
	# если фреймы - вектор, то 
	period.min <- period[1]
	FirstTime <- TRUE 
	for (i in 1:n.ticker) {
		# цикл загрузки с max количеством попыток
		for (t in 1:maxretryattempts) {
			cat("(", i , "/" , n.ticker, ")", "Downloading: ", ticker.list[i], "\t Attempt: ", t , "/", 
				maxretryattempts, "\n")
			data <- GetData_Ticker(ticker = ticker.list[i], from.date = from.date, to.date = to.date, 
								   period = period.min, rename = rename)
			if (exists("data")) {
				cat( "(", i , "/" , n.ticker, ")", "Downloading ", ticker.list[i] , "\t complete", "\n")
  				break
			}
		}
		data <- na.omit(data)
   		data.name <- as.character(ticker.list[i])
		Save_XTStoCSV(data = data, name = data.name, period = period.min)
   		assign(paste(data.name, period.min, sep="."), data)
   		remove(data); remove(data.name)
	}
	ticker.list <- sapply(ticker.list, function(x) { paste(x, period.min, sep = ".") })
	ticker.list <- paste(ticker.list, collapse = ",")
	if (merge == TRUE) {
		temp.text <- paste("full.data <- ", "merge.xts(", ticker.list,")", sep = "")
		eval(parse(text = temp.text))
		full.data <- list(full.data)
	} else {
		temp.text <- paste("full.data <- ", "list(", ticker.list,")", sep = "")
	}
	eval(parse(text = temp.text))
	return(full.data)
}

'data$SR <- Delt(data$Close, type = "arithmetic")
		data$SR[1] <- 0
		data$LR <- Delt(data$Close, type = "log")
		data$LR[1] <- 0
ticker.list <- read.csv(ticker.list, header = F, stringsAsFactors = F)
		ticker.list <- ticker.list[, 1] 
		n.ticker <- length(ticker.list) #The number of stocks to download		'
#
StocksNameList <- function(ticker.list, description = FALSE) {
	# ----------
	# Общее описание:
	# 	вспомогательная для GetTickerListData()
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
	for (i in 1:nstocks) {
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
ExpandData_Period <- function(ticker.list, frame.list, period, description = FALSE) {
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
	data.name.list <- StocksNameList(ticker.list, description)
	frame.list <- read.csv(frame.list, header = F, stringsAsFactors = F)
	nstocks <- nrow(data.name.list)
	nframe <- nrow(frame.list)
	nperiod <- length(period)
	period.min <- period[1]
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		cat( "Processing StocksData:", "\t", data.name, "\n")
		data.source <- Read_CSVtoXTS(name = data.name, period = period[1], tframe = FALSE)
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
				Save_XTStoCSV(data = data, name = data.name, period = p, tframe = n)
			}
		}
	}
	cat( "Expand StocksData...", "\t", "complete", "\n")
}
#
MergeData <- function (price = "SR", ticker.list, description = FALSE, period, tframe, approx = FALSE) {
	# ----------
	# Общее описание:
	# 	вспомогательная для PCA_DataPreparation()
	# 	функция объединения данных в один XTS и устранение NA значений
		# NA можно убрать простым na.locf и аппроксимацией
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	price - тип исследуемых данных 
	# 	tframe - тайм фрейм
	# 	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# 	description - добавить описание к названию файла 
	# 	approx - необходимость аппроксимации NA-данных
	# Выходные данные:
	#	merged.data - xts ряд объединенных значений (по всему портфелю)
	# ----------
	# 
	cat( "Generate DataNameList...", "\n")
	data.name.list <- StocksNameList(ticker.list = ticker.list, description = description) 
	nstocks <- nrow(data.name.list)
	FirstTime <- TRUE
	#  чтение и объединение данных
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		cat( "Processing StocksData:", "\t", data.name, "\n")
		data <- Read_CSVtoXTS(name = data.name, period = period, tframe = tframe) 
		if (price == "Open") {
			data <- data$Open
			col.name <- paste(data.name, "Open")
			names(data) <- c(col.name)
		}
		if (price == "Close") {
			data <- data$Close
			col.name <- paste(data.name, "Close")
			names(data) <- c(col.name)	
		}
		if (price == "SR") {
			data <- data$SR
			col.name <- paste(data.name, "SR")
			names(data) <- c(col.name)
		}
		if (price == "LR") {
			data <- data$LR
			col.name <- paste(data.name, "LR")
			names(data) <- c(col.name)
		}
		if (FirstTime == TRUE) {
			FirstTime <- FALSE
			merged.data <- data
		} else {
			merged.data <- merge(merged.data, data)
		}
	}	
	cat("Merging StocksData:", "\t", "complete", "\n")
	# нормализация NA-значений
	if (approx == FALSE) {
		# нормализация без аппроксимации (с пом-ю na.locf)
		cat( "Normalize StocksData...", "\t", "without approx", "\n") 
		merged.data <- na.locf(merged.data)
	} else	{
		# аппроксимация NA
		cat( "Normalize StocksData...", "\t", "with approx", "\n") 
		merged.data <- na.approx(merged.data)
	}
	merged.data <- na.omit(merged.data)
	cat( "Save Data...", "\n") 
	filename <- paste("MergedData", ticker.list, price, sep = ".")
	Save_XTStoCSV(data = merged.data, name = filename, period = period, tframe = tframe)
	return (merged.data)
}
	
	# выгрузка данных
	#filename <- paste("MergedData", ticker.list, sep = ".")
	#data <- Read_CSVtoXTS (name = filename, period, tframe)