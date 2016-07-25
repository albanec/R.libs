CalcKmean_Parameters <- function(data, test.range = 30, iter.max = 100, plusplus = FALSE) {
  # ----------
  # Общее описание:
  # функция определения оптимального числа k-mean кластеров
  # Входные данные:
  # data: подготовленные результаты отработки бэктеста (должны содержать в себе только переменные и нужные метрики)
  # plusplus: использовать простой k-mean или k-mean++
  # iter.max: число итераций k-mean
  # Выходные данные:
  # ss.df: df суммарного отклонения по кластерам
  # ----------
  #
  cluster.range <- 2:test.range
  #Isolate required features
  data$profit <- NULL
  data$draw <- NULL
  data <- na.omit(data)
  # расчет суммарного квадрата расстояния точек внутри тестовых кластеров
  ss <- c()
  p.exp <- c()
  for (i in cluster.range){
    #cat(i , "\n")
    if (plusplus == TRUE) {
      cluster.data <- CalcKmean_PlusPlus(data, n.opt = i, iter.max = iter.max)
    } else {
      cluster.data <- kmeans(data, centers = i, iter.max)  
    }
    ss <- c(ss, cluster.data$tot.withinss)
    p.exp = c(p.exp, 1 - cluster.data$tot.withinss / cluster.data$totss)
  }
  # сводим всё в df
  ss.df <- data.frame(Num.Of.Clusters = cluster.range,
                      Total.Within.SS = ss,
                      Pct.Change = c(NA, diff(ss)/ss[1:length(ss)-1]) * 100,
                      Pct.Exp = p.exp)
  # вычисление оптимального количества кластеров
  # byVar: опт. число кластеров определяется как min число, описывающее 90% пространства
  n.byVar <- min(which(p.exp > 0.9) )
  # byElbow: опт. число определяется "методом локтя"
  #n.byElbow <- FindMaxDistancePoint(ss.df$p.exp[-1]) + 1
  #n <- c(n.byVar, n.byElbow)
  #n.opt <- n[which.max(n)]
  n.opt <- n.byVar 
  #
  return(list(ss.df, n.opt))
}
#
CalcKmean_PlusPlus <- function(data, n.opt, iter.max = 100) {
  # ----------
  # Общее описание:
  #   функция вычисления модного k-mean++ 
  # Входные данные:
  # data: подготовленные данные
  # n.opt: оптимальное число кластеров для заданного набора данных
  # iter.max: количество итераций вычислений кластера
  # Выходные данные:
  # cluster.data: лист с данными кластера
  # ----------
  #
  # количество точек
  n <- nrow(data)
  # число измерений 
  n.dim <- ncol(data)
  # ID центров (номера строк, содержащих точки центров)
  centers <- c()
  # цикл подбора центров
  for (i in 1:n.opt) {
    if (i == 1 ) {
      # ID первого центра
      center.id <- round(runif(1, 1, n))
      centers  <- c(centers, center.id)
      # рассчёт квадратов расстояний от точек до центра
      if (n.dim == 1) {
        data$s <- apply(cbind(data[center.id, ]), 
                        1, 
                        function(center) {
                          rowSums((data - center)^2) 
                        })
      } else {
        data$s <- apply(data[center.id, ], 
                        1, 
                        function(center) {
                          rowSums((data - center)^2) 
                        })
      }
      # рассчёт кум. суммы квадратов расстояний
      data$ss <- cumsum(data$s)
    } else {
      # цикл расчёта остальных центров (с проверкой на совпадения)
      repeat {
        # вероятность выбора нового центра 
        pr <- runif(1, 0, 1)
        ss.step <- pr * data$ss[[n]]
        center.id <- min(which(data$ss > ss.step))  
        if (center.id %in% centers == FALSE) break
      }
      # запись найденного центра
      centers  <- c(centers, center.id)
      # расчет дальностей для найденного центра 
      data$s <- NULL
      data$ss <- NULL
      if (n.dim == 1) {
        data$s <- apply(cbind(data[center.id, ]),
                        1, 
                        function(center) {
                          rowSums((data - center)^2)
                        })
      } else {
        data$s <- apply(data[center.id, ], 
                        1, 
                        function(center) { 
                          rowSums((data - center)^2)
                        })
      }
      data$ss <- cumsum(data$s)  
    }
  }
  data$s <- NULL
  data$ss <- NULL
  # рассчёт кластеров, исходя из найденных центров
  cluster.data <- kmeans(data, data[centers, ], iter.max)
  #
  return(cluster.data)
}
#
CalcKmean <- function(data, n.opt, iter.max = 100, plusplus = FALSE, var.digits = 0) {
  # ----------
  # Общее описание:
  # функция вычисления k-mean кластеров
  # Входные данные:
  # data: подготовленные данные
  # iter.max: количество итераций вычислений кластера
  # n.opt: оптимальное число кластеров для заданного набора данных
  # plusplus: использовать простой k-mean или k-mean++
  # var.digits: количество занаков после точки в значениях центров кластеров
  # Выходные данные:
  # list(data, cluster.centers): лист с данными (сод. номера кластеров) + df с центрами кластеров
  # ----------
  #
  # вычисление кластера
  if (plusplus == TRUE) {
    cluster.data <- CalcKmean_PlusPlus(data, n.opt, iter.max)
  } else {
    cluster.data <- kmeans(data, centers = n.opt, iter.max)
  }
  # соотнесение данных по кластерам
  data$cluster <- as.factor(cluster.data$cluster)
  # вычисление центров кластеров 
  cluster.centers <- round(cluster.data$centers[, -ncol(cluster.data$centers)], digits = var.digits)
  cluster.centers <- cbind(cluster.centers, round(cluster.data$centers[, ncol(cluster.data$centers)], digits = 3))
  colnames(cluster.centers)[ncol(cluster.centers)] <- "profit.norm"
  #
  return(list(data, cluster.centers))
  # return(data)
}
#
PlotKmean_SS <- function(ss.df, n.opt) {
  # ----------
  # Общее описание:
  #   функция визуализации вычисления оптимального количества кластеров
  # Входные данные:
  #   data: (=ss.df) df суммарной дисперсии по кластерам
  # n.opt: оптимальное число кластеров для заданного набора данных
  # Выходные данные:
  # p: график
  # Зависимости:
  require(plotly)
  # ----------
  ss.df <- as.data.frame(ss.df)  
  p <- 
    plot_ly(ss.df, x = Num.Of.Clusters, y = Total.Within.SS, mode = "lines+markers", color = Pct.Change, 
            marker = list(symbol = "circle-dot", size = 10),
            line = list(dash = "2px")) %>% 
    layout(title = "Суммарная ошибка по кластерам", 
           annotations = list(list(x = n.opt, y = Total.Within.SS[(n.opt - 1)], 
                                   text = "nOptimal", ax = 30, ay = -40)))
    #
    return(p)
}  
#
PlotKmean_Clusters <- function(data.list, cluster.color = FALSE, dimension = "3d", 
                               plot.title = "ClustersPlot", xaxis.name = "FastMA", yaxis.name = "SlowMA", 
                               zaxis.name = "PER", 
                               point.size = 4, point.opacity = 0.8, 
                               point.line.width = 2, point.line.opacity = 0.5,
                               center.size = 10, center.color = "black") {
  # ----------
  # Общее описание:
  #   функция визуализации найденных кластеров
  # Входные данные:
  #   data.list: лист, содержащий в себе данные и центры кластеров
  #  3D: FALSE/TRUE 
  #  cluster.color: TRUE/FALSE расцветка точек по профиту или кластеру
  #   plot.title, xaxis.name, yaxis.name, zaxis.name: название гарфика и осей
  # point.size, point.opacity, point.line.width, point.line.opacity: отрисовка точек
  # center.size, center.color: отрисовка центроидов кластеров
  # Выходные данные:
  # p: график
  # Зависимости:
  require(plotly)
  # ----------
  # 
  # подготовка данных
  data <- as.data.frame(data.list[1])
  centers <- as.data.frame(data.list[2])
  mycolors <-  rainbow(30, start=0.3, end=0.95)
  # подсветка точек (по кластерам или стандартная по доходности)
  if (cluster.color == TRUE) {
    point.color <- data$cluster
  } else {
    point.color <- data$profit.norm  
  }
  # стиль шрифта надписей
    font.style <- list(family = "Courier New, monospace", size = 18, color = "#6699ff")
  # выбор 3D / 2D  
  if (dimension == "3d") {
    # базовый график
    p <- 
      plot_ly(data, x = var1, y = var2, z = var3, type = "scatter3d", mode = "markers", name = "Clusters",
              colors = mycolors, opacity = point.opacity, color = point.color,
              hoverinfo = "text", 
              text = paste(xaxis.name, data$var1, "<br>",
                           yaxis.name, data$var2, "<br>",
                           zaxis.name, data$var3, "<br>",
                           "ProfitNorm:", round(data$profit.norm, 3), "<br>",
                           "Cluster:", data$cluster), 
              marker = list(symbol = "circle",  size = point.size, 
                            line = list(color = "#262626", width = point.line.width, opacity = 0.5)),
                            showlegend = FALSE)
    # добавляем центроиды кластеров
    p <- add_trace(centers, x = var1, y = var2, z = var3, 
                   type = "scatter3d", mode = "markers", name = "Cluster Centers",
                   hoverinfo = "text", 
                   text = paste(xaxis.name, centers$var1, "<br>",
                                yaxis.name, centers$var2, "<br>",
                                zaxis.name, centers$var3, "<br>",
                                "CenterID:", centers$cluster),
                   marker = list(color = center.color, symbol = "cross", size = center.size))
    # надписи на графике
    p <- layout(title = plot.title, 
                scene = list(xaxis = list(title = xaxis.name, titlefont = font.style), 
                             yaxis = list(title = yaxis.name, titlefont = font.style), 
                             zaxis = list(title = zaxis.name, titlefont = font.style)))
  } else {
    # базовый график
    p <- plot_ly(data, x = var1, y = var2, mode = "markers", name = "Clusters",
                 colors = mycolors, opacity = point.opacity, color = point.color,
                 hoverinfo = "text", 
                 text = paste(xaxis.name , data$var1, "<br>", 
                              yaxis.name, data$var2, "<br>",
                              "ProfitNorm:", round(data$profit.norm, 3), "<br>", 
                              "Cluster:", data$cluster), 
                 marker = list(symbol = "circle", size = point.size, 
                               line = list(color = "#262626", width = point.line.width, 
                                           opacity = point.line.opacity)),
                 showlegend = FALSE)
    # добавляем центроиды кластеров
    p <- add_trace(centers, x = var1, y = var2, mode = "markers", name = "Cluster Centers",
                   hoverinfo = "text", 
                   text = paste(xaxis.name, centers$var1, "<br>",
                                yaxis.name, centers$var1, "<br>",
                                "CenterID:", centers$cluster),
                   marker = list(color = center.color, symbol = "cross", size = center.size))
    # надписи на графике
    p <- layout(title = plot.title, 
                xaxis = list(title = xaxis.name, titlefont = font.style), 
                yaxis = list(title = yaxis.name, titlefont = font.style))
  }
  return(p)
}
  