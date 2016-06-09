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
GetData_TickerList <- function(ticker.list = "TickerList.csv", from.date, to.date, period, maxattempts = 5) {
	# ----------
	# Общее описание:
	# 	функция загрузки листа котировок за период from/to.date и сохранения в файлы
	# Входные данные:
	# ticker.list: .csv или вектор с тикерами
	# from.date/to.date: даты начала/конца
	# period: вектор периодов (или единичное значение)
	# maxattempts: количество попыток загрузки 
	# Выходные данные:
	#	.csv файлы
	# листы: либо склееные данные по всем котировкам, либо с отдельными котировками
	# Зависимости:
	require(rusquant)
	# ----------
	#
	cat("Info: Current work.dir:", getwd())	
	if (grepl(".csv", ticker.list) == TRUE) {
		cat("Loading TickerList: ", ticker.list, "\n")
		ticker.list <- read.csv(ticker.list, header = F, stringsAsFactors = F)
		ticker.list <- ticker.list[, 1]	 
		cat("Loading TickerList: OK", "\n") 
	} 
	n.ticker <- length(ticker.list)
	n.period <- length(period)
	# если фреймы - вектор, то 
	period.min <- period[1]
	FirstTime <- TRUE 
	for (i in 1:n.ticker) {
		# цикл загрузки с max количеством попыток
		for (t in 1:maxattempts) {
			cat("(", i , "/" , n.ticker, ")", "Downloading: ", ticker.list[i], "\t Attempt: ", t , "/", 
				maxattempts, "\n")
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

	temp.text <- paste("return(list(", ticker.list,"))", sep = "")
	eval(parse(text = temp.text))
	#return(full.data)
}
#
CalcReturn <- function(data, type = "SR") {
	if (type == "SR") {
		data <- Delt(data, type = "arithmetic")
		data[1] <- 0
	} 
	if (type = "LR") {
		data <- Delt(data, type = "log")
		data[1] <- 0		
	}
	return(data)
}
#
ExpandData_Period <- function(data.list, frame.list, period) {
	# ----------
	# Общее описание:
	# 	функция выделения данных по tf и временному интервалу
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	frame.list - файл, сожержащий список нужных временных интервалов	
	# 		даты должны быть записаны в виде '2014-12-01/2014-12-31'	
	# 	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# Выходные данные:
	#	выдает .csv
	# ----------
	# 
	#cat( "Start DataExpand by FrameList & Period :", "\n")
	#n.period <- length(period)
	#period.min <- period[1]
	#cat("Loading TickerList: ", ticker.list, "\n")
	#ticker.list <- names(data)
	#ticker.list <- sub(paste(".", period.min, sep = ""), "", ticker.list) 
	#cat("Loading TickerList: OK", "\n")
	#n.ticker <- length(ticker.list)
	#
	if (grepl(".csv", frame.list) == TRUE) {
		cat("Loading FrameList: ", frame.list, "\n")
		frame.list <- read.csv(frame.list, header = F, stringsAsFactors = F)
		frame.list <- frame.list[, 1]	  
		cat("Loading FrameList: OK", "\n")
	} 
	n.frame <- length(frame.list)
	n.ticker <- length(data.list) 
	n.period <- length(period)
	period.min <- period[1]
	for (i in 1:n.ticker) {
		#'data.name <- as.character(ticker.list[i])
	#		cat( "Processing StocksData:", "\t", data.name, "\n")
	#		data.source <- Read_CSVtoXTS(name = data.name, period = period[1], tframe = FALSE)
	#		cat ("Expand...", "\t", data.name, "\n")'
		data <- data.list[[i]]
		data.name <- names(data)[grep("Close", names(data))]
		data.name <- sub(".Close", "", data.name)
		cat( "Processing StocksData:", "\t", data.name, "\n")
		for (n in 1:n.frame) {
			# цикл time.frame'а
			cat ("Expand...", "\t", data.name, "for TimeFrame ", frame.list[n], "\n")
			window <- frame.list[n] 
			for (t in 1:n.period) {
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
				data.temp <- data[window]
				ends <- endpoints(data.temp, p1, k)
				data.temp <- data.temp[ends]
				Save_XTStoCSV(data = data.temp, name = data.name, period = p, tframe = n)
			}
			remove(data.temp); remove(data)
		}
	}
	cat( "Expand StocksData...", "\t", "complete", "\n")
}
#
MergeData_InList <- function(data.list) {
	# ----------
	# Общее описание:
	# 	вспомогательная для PCA_DataPreparation()
	# 	функция объединения данных в один XTS и устранение NA значений
		# NA можно убрать простым na.locf и аппроксимацией
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	price - тип исследуемых данных 
	# 	tframe - номер тайм фрейма
	# 	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# 	approx - необходимость аппроксимации NA-данных
	# Выходные данные:
	#	merged.data - xts ряд объединенных значений (по всему портфелю)
	# ----------
	# 
	n.ticker <- length(data) 
	FirstTime <- TRUE
	#  чтение и объединение данных
	for (i in 1:n.ticker) {
		data <- data.list[[i]]
		data.name <- names(data)[grep("Close", names(data))]
		data.name <- sub(".Close", "", data.name)
		cat( "Processing StocksData:", "\t", data.name, "\n")
		#data <- Read_CSVtoXTS(name = data.name, period = period, tframe = tframe) 
		if (col.name != FALSE) {
			col.name <- paste(data.name, col.name, sep = ".")
			temp.text <- paste("data <- data$", col.name, sep = "")
			eval(parse(text = temp.text))
		}
		if (FirstTime == TRUE) {
			FirstTime <- FALSE
			merged.data <- data
		} else {
			merged.data <- merge(merged.data, data)
		}
	}	
	return(list(merged.data))
}
#
NormData_NA <- function(data, type) {
	# 
	if (is.list(data) == TRUE) {
		data <- data[[1]]
		if (is.xts(data) != TRUE) {
			stop("Error: Error in source data!!!")
		}
	}
	# нормализация NA-значений
	if (type == "locf") {
		# нормализация с пом-ю na.locf
		cat( "Normalize StocksData...", "\t", "without approx", "\n") 
		data <- na.locf(data)
	} 
	if (type == "full") {
		# нормализация по уровням свечей 
		cat( "Normalize StocksData...", "\t", "full StocksData", "\n")
		data.names <- names(data)[grep("Close", names(data))]
		data.names <- sub(".Close", "", data.names)
		for (i in 1:length(data.names)) {
			temp.text <- paste("data$",data.names[i],".temp <- data$",data.names[i],".Open ; ",
				"data$",data.names[i],".Open[is.na(data$",data.names[i],".Open)] <- ",
				"na.locf(coredata(data$",data.names[i],".Close))[is.na(data$",data.names[i],".Open)] ; ",
				"data$",data.names[i],".Close[is.na(data$",data.names[i],".Close)] <- ",
				"rev(na.locf(rev(coredata(data$",data.names[i],".temp))))[is.na(data$",data.names[i],".Close)] ; ",
				"data$",data.names[i],".High[is.na(data$",data.names[i],".High)] <- ",
					"ifelse(data$",data.names[i],".Close[is.na(data$",data.names[i],".High)] > ",
						"data$",data.names[i],".Open[is.na(data$",data.names[i],".High)],",
						"data$",data.names[i],".Close[is.na(data$",data.names[i],".High)],",
						"data$",data.names[i],".Open[is.na(data$",data.names[i],".High)]) ; ",
				"data$",data.names[i],".Low[is.na(data$",data.names[i],".Low)] <- ",
					"ifelse(data$",data.names[i],".Close[is.na(data$",data.names[i],".Low)] >",
						"data$",data.names[i],".Open[is.na(data$",data.names[i],".Low)],",
						"data$",data.names[i],".Open[is.na(data$",data.names[i],".Low)],",
						"data$",data.names[i],".Close[is.na(data$",data.names[i],".Low)]) ; ",
				"data$",data.names[i],".Volume[is.na(data$",data.names[i],".Volume)] <- 0 ; ",
				"data$",data.names[i],".temp <- NULL", 
				sep = "")
			eval(parse(text = temp.text))
		}
	}
	if (type == "appr") {
		# аппроксимация NA
		cat( "Normalize StocksData...", "\t", "with approx", "\n") 
		data <- na.approx(data)
	}
	data <- na.omit(data)
	cat( "Save Data...", "\n") 
	filename <- paste("MergedData", ticker.list, price, sep = ".")
	if (save == TRUE) {
		Save_XTStoCSV(data = merged.data, name = filename, period = period, tframe = tframe)	
	}
	return(data)
}
	
	# выгрузка данных
	#filename <- paste("MergedData", ticker.list, sep = ".")
	#data <- Read_CSVtoXTS (name = filename, period, tframe)