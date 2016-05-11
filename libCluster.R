CLU_FindClusterNumbers <- function (data) {
	# ----------
	# Общее описание:
	#   функция свода временных параметров работы стратегии
	# Входные данные:
	#   states.data: ряд сделок
	# Выходные данные:
	#   metric.table: таблица метрик
	# Зависимости:
	library(plotly)
	# ----------
	#Estimate the number of clusters based on the elbow method. Range of clusters to be tested for
	cluster.range <- 2:30
	#Isolate required features
	data$profit <- NULL
	data$draw <- NULL
	data <- na.omit(data)
	#Use Sum of Squares errors across all clusters
	#ss <- (nrow(data)-1)*sum(apply(data,2,var))
	ss <- c()
	p.exp <- c()
	for(i in cluster.range){
  		cluster.data <- kmeans(data, centers = i, iter.max = 100)
  		ss <- c(ss, cluster.data$tot.withinss)
  		p.exp = c(p.exp, 1 - cluster.data$tot.withinss / cluster.data$totss)
	}
	ss.df <- data.frame(Num.Of.Clusters = cluster.range,
                    	Total.Within.SS = ss,
                    	Pct.Change = c(NA, diff(ss)/ss[1:length(ss)-1]) * 100,
                    	Pct.Exp = p.exp)
	#Use plot to find ‘elbow’ visually
	plot_ly(ss.df, x = Num.Of.Clusters, y = Total.Within.SS, mode = "lines+markers", color = Pct.Change, 
        	marker = list(symbol = "circle-dot", size = 10),
        	line = list(dash = "2px")) %>% 
  	layout(title = "Total Sum of Squares", 
  		annotations = list(
  		   	list(x = 20, y = Total.Within.SS[19], text = "Elbow", ax = 30, ay = -40)))
}
# minimum number of clusters that explain at least 90% of variance
min(which(p.exp > 0.9))
# number of clusters based on elbow method
find.maximum.distance.point(p.exp[-1]) + 1

cluster.data <- kmeans(data, centers = 20, iter.max = 100)
data$cluster <- as.factor(cluster.data$cluster)
p1 <- plot_ly(temp.data, x=var1, y=var2, z=var3, type ="scatter3d", mode="markers",  marker= list(size=10, color = "black"))
p2 <- plot_ly(data, x=var1, y=var2, z=var3, 
              type="scatter3d", mode="markers", color=cluster, 
              colors=mycolors, marker = list(size = 4))
p <- subplot(p1, p2 )