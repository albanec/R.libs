CLU_CalcClusterParameters <- function (data) {
	# ----------
	# Общее описание:
	#   функция свода временных параметров работы стратегии
	# Входные данные:
	#   data: подготовленные результаты отработки бэктеста (должны содержать в себе только переменные и нужные метрики)
	# Выходные данные:
	#   ss.df: df суммарной дисперсии по кластерам
	# ----------
	#
	cluster.range <- 2:30
	#Isolate required features
	data$profit <- NULL
	data$draw <- NULL
	data <- na.omit(data)
	# расчет дисперсии по тестовым кластерам
	ss <- c()
	p.exp <- c()
	for(i in cluster.range){
  		cluster.data <- kmeans(data, centers = i, iter.max = 100)
  		ss <- c(ss, cluster.data$tot.withinss)
  		p.exp = c(p.exp, 1 - cluster.data$tot.withinss / cluster.data$totss)
	}
	# всё в df
	ss.df <- data.frame(Num.Of.Clusters = cluster.range,
                    	Total.Within.SS = ss,
                    	Pct.Change = c(NA, diff(ss)/ss[1:length(ss)-1]) * 100,
                    	Pct.Exp = p.exp)
	# вычисление оптимального количества кластеров
	# byVar: опт. число кластеров определяется как min число, описывающее 90% пространства
	n.byVar <- min(which(p.exp > 0.9) )
	# byElbow: опт. число определяется "методом локтя"
	#n.byElbow <- GEN_FindMaxDistancePoint(ss.df$p.exp[-1]) + 1
	#n <- c(n.byVar, n.byElbow)
	#n.opt <- n[which.max(n)]
	n.opt <- n.byVar 
	return(list(ss.df, n.opt))
}
#
CLU_ChartSSFrame <- function (ss.df, n.opt) {
	# ----------
	# Общее описание:
	#   функция визуализации вычисления оптимального количества кластеров
	# Входные данные:
	#   data: (=ss.df) df суммарной дисперсии по кластерам
	# n: оптимальное число кластеров для заданного набора данных
	# Выходные данные:
	# p: график
	# Зависимости:
	require(plotly)
	# ----------
	ss.df <- as.data.frame(ss.df)	
	p <- plot_ly(ss.df, x = Num.Of.Clusters, y = Total.Within.SS, mode = "lines+markers", color = Pct.Change, 
        		 marker = list(symbol = "circle-dot", size = 10),
        		 line = list(dash = "2px")) %>% 
  		layout(title = "Суммарная дисперсия по кластерам", 
  			annotations = list(
  							   list(x = n.opt, y = Total.Within.SS[(n.opt - 1)], text = "nOptimal", ax = 30, ay = -40)))
  	#
  	return(p)
}
#
CLU_CalcCluster <- function (data, n.opt) {
	# вычисление кластера
	cluster.data <- kmeans(data, centers = n.opt, iter.max = 100)
	# соотнесение данных по кластерам
	data$cluster <- as.factor(cluster.data$cluster)
	# вычисление центров кластеров 
	cluster.centers <- round(cluster.data$centers[, -ncol(cluster.data$centers)])
	cluster.centers <- cbind(cluster.centers, round(cluster.data$centers[, ncol(cluster.data$centers)], digits = 3))
	colnames(cluster.centers)[ncol(cluster.centers)] <- "profit.norm"
	return (list(data, cluster.centers))
	#return (data)
}
#
	
	p1 <- plot_ly(temp.data, x=var1, y=var2, z=var3, type ="scatter3d", mode="markers",  marker= list(size=10, color = "black"))
	p2 <- plot_ly(data, x=var1, y=var2, z=var3, 
              type="scatter3d", mode="markers", color=cluster, 
              colors=mycolors, marker = list(size = 4))
p <- subplot(p1, p2 )
