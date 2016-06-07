# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Фукции для парсинга готовых данных бэктеста (из TSlab & WealthLab) и подготовки для анализа:
#
Parse_LabsCSV <- function (file.path = file.path, var.list, profit=profit, 
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
    file <- Read_SimpleCSV(file.path, sep)   
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
Compare_LabsFile <- function (file1, file2, rec=FALSE, p.diff=TRUE) {
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
BotBinNumGenLabsFile <- function (n) {
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
BotNumSetLabsFile <- function (file, bot.num.table) {
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
Filter_DuplicatedRow_LabsFile <- function (file) {
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
NormProfit_LabsFile <- function (file, m) {
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
Quant_LabsFile <- function (data, var, q.hi = 0, q.low = 0, two = FALSE, low = FALSE, hi = FALSE, abs = FALSE) {
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
AllPreparation_LabsFile <- function (file.path, sep = ";",
										var.list, profit = profit, draw=draw, m=FALSE, 
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
    data <- Parse_LabsCSV(file.path = file.path, var.list, profit=profit, 
    						 draw=draw, sort=FALSE, var.names=FALSE, sep)
    if (tslab == FALSE) {
        data$var0 <- BotNumSetLabsFile(data, bot.num.table)    
        data <- Filter_DuplicatedRow_LabsFile(data)
    }
    if (m != FALSE) {
    	data$profit.norm <- NormProfit_LabsFile(data, m = m)	
    	data <- data[which(data$profit.norm > 0),]
    } else {
    	data <- data[which(data$profit > 0),]
    }
    if (trend.filter == TRUE) {
    	data <- data[which(data$var1 < data$var2),]	
    }
    #
   	if (hi | low != FALSE) {
   		data <- Quant_LabsFile(data, var=6, q.hi, q.low, hi, low, abs=FALSE)
   	}
    if (one.scale == TRUE) {
        data[nrow(data)+1, ] <- c(rep(0, length(var.list)+2), data$profit.norm[[which.max(data$profit.norm)]])
        data[nrow(data)+1, ] <- c(rep(0, length(var.list)+3))
    }
    return (data)
}
#