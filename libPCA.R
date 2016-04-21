DataPrepareForPCA <- function (ticker.list, price, description, period, tframe, approx = FALSE) {
	# ----------
	# Общее описание:
	# 	функция подготовки данных к PCA
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	price - тип исследуемых данных 
	# 	tframe - тайм фрейм (ОДНО значение)
	# 	period - период свечей (ОДНО значение)
	# 	description - добавить описание к названию файла 
	# 	approx - необходимость аппроксимации NA-данных
	# Выходные данные:
	#	data - xts ряд объединенных подготовленных значений (по всему портфелю)
	# ----------
	# 
	cat( "Start DataPrepareForPCA...", "\n")
	data <- MergeForPCA(price  =  price, ticker.list = ticker.list, description = description, period = period, tframe = tframe, approx = approx)
	cat( "Merging Data...", "\t", "done", "\n")
	#data <- BindToMatrix(data, load.csv = FALSE, save.filename = "Matrix.csv")
	cat( "Create MatrixForPCA...", "\t", "done", "\n")
	return(data)
} 
#
MergeForPCA <- function (price = "SR", ticker.list, description = FALSE, period, tframe, approx = FALSE) {
	# ----------
	# Общее описание:
	# 	вспомогательная для DataPrepareForPCA()
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
		data <- ReadCSVtoXTS(name = data.name, period = period, tframe = tframe) 
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
	SaveXTStoCSV(data = merged.data, name = filename, period = period, tframe = tframe)
	#write.table(merged.data, file = filename, sep = ",")
	return (merged.data)
}
#
BindToMatrix <- function (data, load.filename = FALSE, save.filename = "Matrix.csv") {
	# ----------
	# Общее описание:
	# 	вспомогательная для DataPrepareForPCA()
	# 	функция преобразования объединенного xts в матрицу
		# на вход подается merged.data xts либо напрямую, либо через чтение .csv (чтение нужно для 
		# независимого от DataPrepareForPCA() использования
	# Входные данные:
	#	data - подготовленный и объединенный xts (merged.data)
	# 	load.filename - если данные необходимо считать из .csv, то это путь
	# Выходные данные:
	#	data - матрица для PCA 
		# + матрица сохраняется в save.filename
	# ----------
	# 
	if (load.filename != FALSE) {
		cat( "Loading Data for BindToMatrix...", "\n")
		data <- ReadCSVtoXTS(name = load.filename)
	}
	#преобразование в матрицу 
	cat( "Create Matrix...", "\n")
	#data <- data.frame(date = index(data), coredata(data))
	data <- as.matrix(data, nrow = nrow(data), ncol = ncol(data))
	write.table(data, file = save.filename, sep = ",")
	return (data)
}
#
ExpandDataPrepareForPCA.toSCV <- function (ticker.list, frame.list, description, period,  approx, price) {
	# ----------
	# Общее описание:
	# 	генерирует большое количество данных для PCA (расширенное по периодам/тайм-фреймам)
	# Входные данные:
	#	ticker.list - файл, сожержащий список нужных тикеров 
	# 	price - тип исследуемых данных 
	# 	frame.list - лист тайм фреймов 
	# 	period - вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
	# 	description - добавить описание к названию файла 
	# 	approx - необходимость аппроксимации NA-данных
	# Выходные данные:
	#	data - матрица для PCA 
		# + матрица сохраняется в save.filename
	# ----------
	# 
	frame.list <- read.csv(frame.list, header  =  F, stringsAsFactors  =  F)
	nframe <- nrow(frame.list)
	tframe <- seq(1, nframe)
	nperiod <- length(period)
	for (i in 1:nperiod) {
		for (t in 1:nframe) {
			data <- DataPrepareForPCA (ticker.list = ticker.list, description = description, period = period[i], tframe = tframe[t], approx = approx, price = price)
		}	
	} 
}
#
PCAcompute <- function(data, period, tframe, price = "SR", KG.test = FALSE, print = TRUE) {
	# ----------
	# Общее описание:
	# 	вычисление PC + тестирует данные
	# Входные данные:
	#	data - xts, содержащий подготовленные данные 
	# 	period - данные по периоду свечей (нужно для корректоного имени .csv)
	#	tframe - данные по тайп-фрейму (нужно для корректоного имени .csv)
	#	KG.test - необходимость теста Кайзера-Гуттмана (проверки PC на стат. значимость)
	# Выходные данные:
	#	equity - ряд equity для PCA 
	#	+ запись весов в файл
	# ----------
	#
	cat ("PCA start:", "\t", ticker.list, "\n")
	pca.data <- princomp(data)
	# нагрузки
	loadings <- pca.data$loadings[]
	# Критерий  Кайзера-Гуттмана  рекомендует оставить для дальнейшего анализа только те главные компоненты,
	# дисперсия которых превышают среднюю:
	if (KG.test == TRUE) {
		ev <- as.numeric(pca.data$sdev)
		ev <- which.min(ev>mean(ev))
		cat("KG factor PCA numbers:", "\t", ev, "\n")	
		#n.PC <- ev
	} else {
		# выбираем не больше первых 10 PC
		n.PC <- ncol(loadings)
		if (n.PC > 10) {
			n.PC <- 10
		}	
	} 
	# выделение компонент
	components <- loadings[, 1:n.PC]
	# нормализуем компоненты 
	n <- ncol(data)
	components <- components / rep.row(colSums(abs(components)), n)
	# note that first component is market, and all components are orthogonal i.e. not correlated to market
	market <- data %*% rep(1/n,n)
	temp <- cbind(market, data %*% components)
    colnames(temp)[1] <- 'Market' 
    temp <- as.xts(temp, index(data))
    # корреляция между PC (по Пирсону)
	cor.table <- round(cor(temp, use = 'complete.obs', method = 'pearson'),2)
 	# суммарная дисперсия компонент
	vol <- round(100*sd(temp,na.rm = T),2)
	# вывод данных
	if (print == TRUE) {
		cat("PC value:", "\t", n.PC, "\n")
		cat("Components after PCA:")
		print(components)
		cat("\n")
		cat("Correlation Table between PCA:")
		print(cor.table)
		cat("\n")
		cat("Vol Summary:", vol, "\n")
	}
	# вычислене equity по каждой из компонент
	if (price == "Close") {
		m <-  -data %*% components
		equity <- apply(m, 2,  cumsum)
		equity <- as.xts(equity, index(data))	
	}
	if (price == "SR") {
		m <- 1 + (-data %*% components)
		equity <- apply(m, 2,  cumprod)
		equity <- as.xts(equity, index(data))
	} 
	if (price == "LR") {
		m <-  -data %*% components
		equity <- apply(m, 2,  cumsum)
		equity <- as.xts(equity, index(data))	
	}
	# сохраняем веса
	cat("Save components")
	components.filename <- paste("Components", ticker.list, period, tframe, "csv", sep = ".")
	write.table(components, file = components.filename, sep = ",")
	#barplot(height = pca.data$sdev[1:10]/pca.data$sdev[1])
	return(equity)
}
PCA.DFtest <- function (data) {
	# ----------
	# Общее описание:
	# 	проверка на стационарность получившихся PC (по их equity)
	# Входные данные:
	#	data - xts, equity главных компонент 
	# Выходные данные:
	#	equity[, statPC] - ряд equity для наиболее стационарной PCA 
	# ----------
	#
	# ДФ-тест на стационарность
	df.value <- rep(NA, n.PC)
	for (i in 1:n.PC) {
		df.value[i] <- adf.test(as.numeric(equity[, i]))$p.value			
	}
	statPC <- which.min(df.value)
	cat("Best DF-test result...", "\t", df.value[statPC], "\t", "PC:", statPC, "\n")
	return (equity[, statPC]) 
}

	# выгрузка данных
	#filename <- paste("MergedData", ticker.list, sep = ".")
	#data <- ReadCSVtoXTS (name = filename, period, tframe)
