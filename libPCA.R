PCA_DataPreparation <- function (ticker.list, price, description, period, tframe, approx = FALSE) {
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
	cat( "Start PCA_DataPreparation...", "\n")
	data <- PCA_MergeData(price  =  price, ticker.list = ticker.list, description = description, period = period, tframe = tframe, approx = approx)
	cat( "Merging Data...", "\t", "done", "\n")
	#data <- PCA_BindToMatrix(data, load.csv = FALSE, save.filename = "Matrix.csv")
	cat( "Create MatrixForPCA...", "\t", "done", "\n")
	return(data)
} 
#
PCA_MergeData <- function (price = "SR", ticker.list, description = FALSE, period, tframe, approx = FALSE) {
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
	data.name.list <- GEN_StocksNameList(ticker.list = ticker.list, description = description) 
	nstocks <- nrow(data.name.list)
	FirstTime <- TRUE
	#  чтение и объединение данных
	for (i in 1:nstocks) {
		data.name <- as.character(data.name.list[i])
		cat( "Processing StocksData:", "\t", data.name, "\n")
		data <- GEN_ReadCSVtoXTS(name = data.name, period = period, tframe = tframe) 
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
	GEN_SaveXTStoCSV(data = merged.data, name = filename, period = period, tframe = tframe)
	#write.table(merged.data, file = filename, sep = ",")
	return (merged.data)
}
#
PCA_BindToMatrix <- function (data, load.filename = FALSE, save.filename = "Matrix.csv") {
	# ----------
	# Общее описание:
	# 	вспомогательная для PCA_DataPreparation()
	# 	функция преобразования объединенного xts в матрицу
		# на вход подается merged.data xts либо напрямую, либо через чтение .csv (чтение нужно для 
		# независимого от PCA_DataPreparation() использования
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
		data <- GEN_ReadCSVtoXTS(name = load.filename)
	}
	#преобразование в матрицу 
	cat( "Create Matrix...", "\n")
	#data <- data.frame(date = index(data), coredata(data))
	data <- as.matrix(data, nrow = nrow(data), ncol = ncol(data))
	write.table(data, file = save.filename, sep = ",")
	return (data)
}
#
PCA_ExpandData <- function (ticker.list, frame.list, description, period,  approx, price) {
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
			data <- PCA_DataPreparation (ticker.list = ticker.list, description = description, period = period[i], tframe = tframe[t], approx = approx, price = price)
		}	
	} 
}
#
PCA_ComputePCA <- function(data, period, tframe, price = "SR", KG.test = FALSE, print = TRUE) {
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
	components <- components / GEN_RepeatRow(colSums(abs(components)), n)
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
		cat("\n")
		cat("PC value:", "\t", n.PC, "\n")
		cat("\n")
		cat("Components after PCA:", "\n")
		print(components)
		cat("\n")
		cat("Correlation Table between PCA:", "\n")
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
#
PCA_DFtestPCA <- function (data) {
	# ----------
	# Общее описание:
	# 	проверка на стационарность получившихся PC (по их equity)
	# Входные данные:
	#	data - xts, equity главных компонент 
	# Выходные данные:
	#	statPC: номер наиболее стационарной PC 
	# ----------
	#
	# ДФ-тест на стационарность
	n.PC <- ncol(data)
	df.value <- rep(NA, n.PC)
	for (i in 1:n.PC) {
		df.value[i] <- adf.test(as.numeric(data[, i]))$p.value			
	}
	statPC <- which.min(df.value)
	cat("Best DF-test result...", "\t", df.value[statPC], "\t", "PC:", statPC, "\n")
	return (statPC) 
}
#
PCA_ComputeData <- function (ticker.list, period, tframe) {
	# ----------
	# Общее описание:
	# 	рассчитывает веса портфеля (полученные из наиболее нужной PC) на периоде времени 
	# Входные данные:
	# 	ticker.list: лист котировок портфеля
	# 	period: период свечек
	#	tframe: номер периода во FrameList 
	# Выходные данные:
	#	data: ряд значений котировок ног портфеля и самого портфеля 
	# ----------
	#
	components.filename <- paste("Components", ticker.list, period, tframe, "csv", sep = ".")
	read.table(components, file = components.filename, sep = ",")
	
}
#
PCA_ScoresData <- function (data, vol = TRUE, sma.period) {
	# ----------
	# Общее описание:
	#  вычисление z-scores для PC (с нормировкой по vol( за sma.period) и без)
	# Входные данные:
	#	data: xts, equity нужной главной компоненты
	#	vol: учёт vol (!отключать только в тестовых целях!) 
	#	sma.period: период SMA, используемой для усреднения  
	# Выходные данные:
	#	data - к ряду equity добавлены значения SMA, vol и z-scores для нужной (стационарной) PCA 
	# Зависимости:
		require(quantmod) 
	# ----------
	#
	names(data) <- c("equity")
	data$SMA <- SMA(data$equity, sma.period)
	if (vol == TRUE) {
		data$vol <- runSD(x, n = sma.period)
		data$z.score <- (data$equity - data$SMA) / data$vol 
	} else {
		data$z.score <- (data$equity - data$SMA)
	}
	data <- na.omit(data)
	return(data)
}
#
PCA_StrategySimpleMeanReversion <- function (data, sma.period, 
											 low.mark, hi.mark, 
											 low.close.mark, hi.close.mark, 
											 price = "LR", state=TRUE) {
	# ----------
	# Общее описание:
	#  вычисление позиций по стратегии простого возврата к стреднему
	# Входные данные:
	#	data - equity нужной главной компоненты с zscores
	# Выходные данные:
	#	data - к исходному ряду добавлен ряд сделок
	# Зависимости:
		require(quantmod) 
	# ----------
	#
	low.mark <- 0.25 
	hi.mark <- 0.75
	low.close.mark <- 0.0
	hi.close.mark <- 0.0
	# точки пересечения PC с границами зоны открытия позиций 
		# пересечение верхней зоны открытия (снизу вверх)
	data$sig.buy <- GEN_CrossForXTS(data$z.score, hi.mark) 
		# пересечение нижней зоны открытия (сверху вниз)
	data$sig.sell <- GEN_CrossForXTS(low.mark, data$z.score)
	# точки пересечения PC с границами зоны закрытия позиций 
		# пересечение верхней зоны закрытия (сверху вниз)
	data$sig.close.buy <- GEN_CrossForXTS(hi.close.mark, data$z.score) 
		# пересечение нижней зоны закрытия (сверху вниз)
	data$sig.close.sell <- GEN_CrossForXTS(data$z.score, low.close.mark)
	# точки в позиции buy
	data$pos.buy <- data$sig.buy - data$sig.close.buy
	# точки в позиции sell
	data$pos.sell <- data$sigUp - data4$sigDn
	
	data$pos <- lag(data$pos)
	if (state==TRUE) {
		data$state <- exrem(data$pos)
	} else {
		colnames(data)[colnames(data)=="pos"] <- "state"
	}




	# убираем лишнее и добавляем очишенные данные в ряд
	data.temp$sig.clean <- exrem(data.temp$sig)
	# убираем лишние столбцы
	data.temp$sig <- NULL
	data.temp <- na.omit (data.temp)
	data$sig <- NULL
}
	
	

	# выгрузка данных
	#filename <- paste("MergedData", ticker.list, sep = ".")
	#data <- GEN_ReadCSVtoXTS(name = filename, period, tframe)
