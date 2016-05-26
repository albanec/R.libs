#
GEN_RepeatRow <- function(x,n) {
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
GEN_RepeatCol<-function(x,n) {
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
GEN_FindMaxDistancePoint <- function (y, x=1:len(y)) {
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
GEN_ReadSimpleCSV <- function (file.path, sep = ";") {
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
	for (i in 1:nstocks) {
		# цикл загрузки с max количеством попыток
		for (t in 1:maxretryattempts) {
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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Фукции для парсинга готовых данных бэктеста (из TSlab & WealthLab) и подготовки для анализа:
#
GEN_ParseLabsCSV <- function (file.path = file.path, var.list, profit=profit, 
							draw = draw, sort = FALSE, var.names = TRUE, sep = ";") {
    # ----------
    # Общее описание:
    #   функция для парсинга .csv файлов (заточена под выгрузку данных из WealthLab & TSlab)
    # Входные данные:
    #   file.path: путь к файлу (абсолютный путь)
    #   var.list: лист с номерами столбцов нужных переменных (любое количество, функция подстраивается под нужное число)
    #   profit: номер столбца с профитом
    #   draw: номер столбца с данными по просадкам (нужно для нормирования доходностей)
    #   sort: нужна ли сортировка занчений по доходности
    #   var.names: выгрузить из исходного файла имена переменных
    # Выходные данные:
    #   data: выгруженные из .csv данные
    # ----------
    #
    # считывание файла 
    file <- GEN_ReadSimpleCSV(file.path, sep)   
    # выделяем нужные параметры
    # profit/draw
    profit.name <- file[[1, profit]] 
    draw.name <- file[[1, draw]]
    # всего пременных
    n.vars <- length(var.list)
    cat( "############", "\n",
         "Парсинг файла:", ".......... ", file.path, "\n")
    cat(sep = "\n", "############",
        "Выбраны переменные & тепловой параметр:",
        "" )
    var.name.list <- c()
    name.raw <- file[1, ]
    file <- file[-1, ]
    file <- file[, colSums(is.na(file)) == 0]
    # temp дата-фрейм 
    temp.frame <- rep(NA, nrow(file))
    temp.frame <- data.frame(temp.frame)
    for (i in 1:n.vars) {
        temp.var.name <- name.raw[[var.list[i]]]
        cat("var", i, ": ", ".......... ", temp.var.name, "\n")
		var.name.list <- c(var.name.list, temp.var.name)
     	temp.frame[, paste("var", i, sep = "")] <- as.numeric(gsub("\\,", ".", file[[var.list[i]]])) 
        temp.frame[, paste("var", i, sep = "")] <- as.numeric(gsub("\\s", "", temp.frame[, paste("var", i, sep = "")]) )
    }
    cat("profit: ", ".......... ", profit.name, "\n")
    cat("draw:   ", ".......... ", draw.name, "\n")
    temp.frame$profit <- as.numeric( gsub("\\,", ".", file[[profit]]) )
    temp.frame$profit <- as.numeric( gsub("\\s", "", temp.frame$profit) )
    temp.frame$draw <- as.numeric( gsub("\\,", ".", file[[draw]]) )
    temp.frame$draw <- as.numeric( gsub("\\s", "", temp.frame$draw) )
    temp.frame$temp.frame <- NULL
    # чистим от лишнего 
    remove(file)  
    # сортировка по профиту
    if (sort == TRUE) {
        temp.frame <- temp.frame[order(-temp.frame$profit),]
        cat( "############", "\n",
             "Сортировка по данных по profit'у", "\n")	
    }
    if (var.names == TRUE) {
    #    colnames(temp.frame) <- c(var1.name, var2.name, var3.name, profit.name)	
        colnames(temp.frame) <- c(var.name.list, profit.name, draw.name)
        cat( "############", "\n",
             "Столбцы проименованы", "\n")
    }
    cat(sep = "\n", "############",
        "Готово.", 
        "############" )
    return (temp.frame)
}
#
GEN_CompareLabsFile <- function (file1, file2, rec=FALSE, p.diff=TRUE) {
	# ----------
    # Общее описание:
    # функция для сравнения двух распарсенных данных бэктеста и вывода только 
    # повторяющихся строк
    # Входные данные:
    # file1, file2: считанные из .csv данные
    # rec: наличие/отсутствие рековери
    # p.diff: нужен ли расчёт сглаженности equity (abs разности профитов)
    # Выходные данные:
    #   file1: файл содер. в себе только повторяющие параметры роботов (+|- рековери и разница профитов)
    # ----------
    #
	# добавление идентификатора строк и сортировка 
		temp.file1 <- file1
		temp.file2 <- file2
		file1$profit <- NULL
		file1$draw <- NULL
		file2$profit <- NULL
		file2$draw <- NULL
		if (rec == TRUE) {
			file1$recovery <- NULL
			file2$recovery <- NULL
		}
		file1$var0 <- apply(file1, 1, paste, collapse='')
		file2$var0 <- apply(file2, 1, paste, collapse='')
		file1$var0  <- as.numeric(file1$var0)
		file2$var0  <- as.numeric(file2$var0)
		file1$profit <- temp.file1$profit  
		file1$draw <- temp.file1$draw   
		file2$profit <- temp.file2$profit
		file2$draw <- temp.file2$draw
		if (rec == TRUE) {
			file1$recovery <- temp.file1$recovery  
			file2$recovery <- temp.file2$recovery 
		}
		file1 <- file1[order(-file1$var0),]
		file2 <- file2[order(-file2$var0),]
		file1$type <- "file1"
		file2$type <- "file2"
		remove(temp.file1)
		remove(temp.file2)
	# проверка на совпадение
		l <- rbind(file1, file2)
		t1 <- duplicated(l$var0, fromLast = TRUE)
		t2 <- duplicated(l$var0)
		file1 <- l[t1,]
		file2 <- l[t2,]
	# вычесление изменения профита и суммы за два периода
		file1$profit2 <- file2$profit
		if (rec == TRUE) {
			file1$recovery2 <- file2$recovery
			}
		if (p.diff == TRUE) {
			file1$profit.dif <- abs(file1$profit - file1$profit2)
			file1$profit.sum <- (file1$profit + file1$profit2)
		}
		file1$var0 <- NULL
		file1$type <- NULL
	#
	return(file1)
}
#
GEN_BotBinNumGenLabsFile <- function (n) {
    # ----------
    # Общее описание:
    # функция для генерации номеров ботов (нужна для анализа тестов из WealthLab)
    # повторяющихся строк; бестолковая, тупо генерит бинарный ряд 2^n-1 и присваивает номера строкам
    # Входные данные:
    # n: степень двойки
    # Выходные данные:
    #   bot.num.table: таблица бинарных номеров роботов с присвоенными десятеричными номерами
    # ----------
    #
    decimals <- seq(0, 2^n-1)
    m <- sapply(decimals,function(x) { as.integer(intToBits(x))})
    m <- head(m ,3)
    m <- t(head(m, n))
    bot.num.table <- cbind(rep(0, nrow(m)), m)
    bot.num.table <- rbind(bot.num.table, cbind(rep(1, nrow(m)), m))
    bot.num.table <- rbind(bot.num.table, cbind(rep(2, nrow(m)), m))
    bot.num.table <- apply(bot.num.table, 1, paste, collapse='')
    bot.num.table <- cbind(seq(1, length(bot.num.table)), bot.num.table)
    return(bot.num.table)
}
#
GEN_BotNumSetLabsFile <- function (file, bot.num.table) {
    # ----------
    # Общее описание:
    # Функция выставляет соответствие бинарных номеров ботов из WealthLab десятиричным номерам из bot.num.table
    # Входные данные:
    # file: считанные данные
    # bot.num.table: сгенерированный ряд бинарных и десятиричных номеров
    # Выходные данные:
    # file$var0: столбей с номерами
    # ----------
    #
    file.temp <- file
    file.temp$var1 <- NULL
    file.temp$var2 <- NULL
    file.temp$var3 <- NULL
    file.temp$profit <- NULL
    file.temp$draw <- NULL
    file$var0 <- apply(file.temp, 1, paste, collapse='')   
    for (i in 1:nrow(bot.num.table)) {
        n <- which(file$var0 == bot.num.table[[i,2]])
        file$var0[n] <- bot.num.table[[i,1]]
    } 
    return (file$var0)
}
#
GEN_DuplicatedRowFilterLabsFile <- function (file) {
    # ----------
    # Общее описание:
    # Функция выделяет уникальные наборы параметров ботов
    # Входные данные:
    # file: данные тестов с присвоенными номерами ботов
    # Выходные данные:
    # file: отфильтрованные данные (только уникальные номера ботов)
    # ----------
    #
    file.temp <- file
    file.temp$profit <- NULL
    file.temp$draw <- NULL
    file.temp$var0 <- NULL
    file.temp$var0 <- apply(file.temp, 1, paste, collapse='')
    file <- file[which(duplicated(file.temp$var0) == FALSE), ]
    return (file)
}
GEN_ProfitNormLabsFile <- function (file, m) {
    # ----------
    # Общее описание:
    # Функция нормировки профита к просадке и к году 
    # Входные данные:
    # file:  данные бэков, содержащие profit и draw 
    # m: количество месяцев торговли
    # Выходные данные:
    # file$profit.norm: столбец нормированных доходностей
    # ----------
    #
    file$profit.norm <- (file$profit*12) / (abs(file$draw)*m)
    return (file$profit.norm)
}
#
GEN_QuantLabsFile <- function (data, var, q.hi = 0, q.low = 0, two = FALSE, low = FALSE, hi = FALSE, abs = FALSE) {
	# ----------
    # Общее описание:
    # Функция вычисление лучших / худших значений (на основе квантиля) 
    # Входные данные:
    # data: данные бэков
    # var: столбец данных с качественной характеристикой
    # q.hi: уровень квантиля для вычисления лучших значений (берётся всё, что выше квантиля)
    # q.low: уровень квантиля для вычисления худших значений (берётся всё, что ниже квантиля)
    # low, hi, two: вычисляем худшие / лучшие / "середину"  
    # abs: если нужно, абсолютное значение квантиля (вычисленное ранее)
    # Выходные данные:
    # data: отфильтрованные данные
    # ----------
    #
	if (two == TRUE) {
		# подготовка данных
		data <- data[order(-data[[var]]), ]
		# вычисление квантилей
		ifelse(abs == FALSE, q.hi.value <- quantile(data[[var]], q.hi), q.hi.value <- as.numeric(q.hi))
		n.hi <- which( data[, var] < q.hi.value )
		ifelse(abs == FALSE, q.low.value <- quantile(data[[var]], q.low), q.low.value <- as.numeric(q.low)) 
		data <- data[n.hi,]
		n.low <- which( data[, var] > q.low.value )
		data <- data[n.low,]
		} 
	if (hi == TRUE) {
		data <- data[order(-data[[var]]), ]
		ifelse(abs == FALSE, q.hi.value <- quantile(data[[var]], q.hi), q.hi.value <- as.numeric(q.hi))
		n.hi <- which( data[, var] > q.hi.value )	
		data <- data[n.hi,]
		}
	if (low==TRUE) {
		data <- data[order(-data[[var]]), ]
		ifelse (abs == FALSE, q.low.value <- as.numeric(quantile(data[[var]], q.low)), q.low.value <- as.numeric(q.low)) 
		n.low <- which( data[, var] < q.low.value )
		data <- data[n.low,]
		}
	#	
	return (data)
}
#
GEN_AllPreparationLabsFile <- function (file.path, sep = ";",
										var.list, profit = profit, draw=draw, m, 
								  		q.hi = FALSE, q.low = FALSE, low = FALSE, hi = FALSE,
								  		one.scale=TRUE,  tslab = TRUE, trend.filter = TRUE) {
    # ----------
    # Общее описание:
    # Итоговая функция для обработки данных из *Lab
    # Входные данные:
    # file.path, sep: путь и разделители для парсинга
    # var.list, profit, draw, m: номера столбцов переменных, профита, просадки и количество месяцев торговли 
    # q.him q.low, loq, hi: параметры квантиля
    # one.scale: нужно ли сохранить max/min заначения (для оптимизации тепловой шкалы)
    # tslab: T/F данные из TSlab
    # trend.filter: фильтровать или нет контртрендовые значения
    # Выходные данные:
    # data: полностью обработанные данные
    # ----------
    #
    data <- GEN_ParseLabsCSV(file.path = file.path, var.list, profit=profit, 
    						 draw=draw, sort=FALSE, var.names=FALSE, sep)
    if (tslab == FALSE) {
        data$var0 <- GEN_BotNumSetLabsFile(data, bot.num.table)    
        data <- GEN_DuplicatedRowFilterLabsFile(data)
    }
    if (m != FALSE) {
    	data$profit.norm <- GEN_ProfitNormLabsFile(data, m = m)	
    	data <- data[which(data$profit.norm > 0),]
    } else {
    	data <- data[which(data$profit > 0),]
    }
    if (trend.filter == TRUE) {
    	data <- data[which(data$var1 < data$var2),]	
    }
    #
   	if (hi | low != FALSE) {
   		data <- GEN_QuantLabsFile(data, var=6, q.hi, q.low, hi, low, abs=FALSE)
   	}
    if (one.scale == TRUE) {
        data[nrow(data)+1, ] <- c(rep(0, length(var.list)+2), data$profit.norm[[which.max(data$profit.norm)]])
        data[nrow(data)+1, ] <- c(rep(0, length(var.list)+3))
    }
    return (data)
}
#