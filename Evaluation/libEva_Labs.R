# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для анализа параметров бектеста из Lab'ов
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
BotReitLabsFile <- function(data, bot.num.table) {
  # ----------
  # Общее описание:
  #   функция вычисления рейтингов ботов
  # Входные данные:
  #   data: данные с рядом id-ботов (var0)
  #   bot.num.table: таблица с номерами ботов
  # Выходные данные:
  #   table: таблица номерами ботов и их рейтингами
  # ----------
  FirstTime <- TRUE
  for (i in 1:nrow(bot.num.table)) {
    bot.reit <- 0
    bot.sum <- 0
    for (n in 1:nrow(data)){
      if (data$var0[n] == bot.num.table[i,1]) {
        bot.reit <- bot.reit + 1
        bot.sum <- bot.sum + data$profit.norm[n]
      }   
    }
    x <- c(i, bot.reit, bot.sum)
    if (FirstTime == TRUE) {
      table <- x 
      FirstTime <- FALSE
    } else {
      table <- cbind(table, x)
    }        
  }
  table <- t(table)
  return (table)
}
PerReitLabsFile <- function(data) {
  # ----------
  # Общее описание:
  #   функция вычисления рейтингов периодов
  # Входные данные:
  #   data: данные 
  # Выходные данные:
  #   table: таблица номерами ботов и их рейтингами
  # ----------
  FirstTime <- TRUE
  for (i in 1:300) {
    bot.reit <- 0
    bot.sum <- 0
    for (n in 1:nrow(data)) {
      if (i == data$var3[n]) {
        bot.reit <- bot.reit + 1
        bot.sum <- bot.sum + data$profit.norm[n]
      }
    }        
    x <- c(i, bot.reit, bot.sum)
    if (FirstTime == TRUE) {
      table <- x 
      FirstTime <- FALSE
    } else {
      table <- cbind(table, x)
    }
  }       
  table <- t(table)
  return (table)
}
#
3DChartLabsFile <- function(data, var1, var2, var3, color) {
  # ----------
  # Общее описание:
  #   функция построения 3D графиков для LabsFile
  # Входные данные:
  #   data: данные 
  #   var1, var2, var3, color: номера столбцов переменных и цвета
  # Выходные данные:
  #   p: график
  # Зависимости:
  require(plotly)
  # ----------
  mycolors <-  rainbow(30, start = 0.3, end = 0.95)
  p <- plot_ly(data, x = var1, y = var2, z = var3, 
               type = "scatter3d", mode = "markers", color = color, 
               colors = mycolors, marker = list(size = 4))  
  return(p)
}
#
ReitChartLabsFile <- function(data, bot = FALSE, per = FALSE) {
  # ----------
  # Общее описание:
  #   функция графиков рейтингов
  # Входные данные:
  #   data: таблица с рейтингами (per ил bot)
  # per/bot: какой график строить
  # Выходные данные:
  #   p: график
  # Зависимости:
  require(plotly)
  # ----------
  if (per == TRUE) {
    p <- plot_ly(x = data[, 2], y = data[, 1], mode = "markers", color = data[,3], colors=mycolors)  
  }
  if (bot == TRUE) {
    p <- plot_ly(x = data[,1], y = data[,2], mode = "markers", color = data[,3], colors=mycolors)  
  }
  return(p)
}
#
3Dto2DChartsMatrixLabsFile <- function(data, plot.num) {
  # ----------
  # Общее описание:
  #   функция раскладывает 3D график на набор 2D графиков
  # Входные данные:
  #   data: данные
  # plot.num: рандомное число для загрузки графиков на сервер
  # Выходные данные:
  #   p: матрица гарфиков
  # Зависимости:
  require(plotly)
  # ----------
  data <- data[order(-data$var3), ]
  new.begining <- TRUE
  plot.vector <- c()
  mycolors <-  rainbow(30, start=0.3, end=0.95)
  FirstTime <- TRUE
  for ( i in 1:(nrow(data)-1)) {
    if (data$var3[i] == data$var3[i+1]) {
      per.num <- data$var3[i]
      if (FirstTime == TRUE) {
       temp.data <- data[i, ]
       FirstTime <- FALSE
      } else {
        temp.data <- rbind(temp.data, data[i, ])
      }
    } else {
       per.num <- data$var3[i]
      FirstTime <- TRUE
      new.begining <- TRUE
      if (i == 1) {
        temp.data <- data[i, ]
      } else {
         temp.data <- rbind(temp.data, data[i, ])    
        }
      temp.plot.name <- paste("plot", per.num, sep=".")
      temp.data.name <- paste("temp.data", per.num, sep=".")
      temp.yaxis.name <- paste("yaxis", per.num, sep=".")
      temp.xaxis.name <- paste("xaxis", per.num, sep=".")
      plot.vector <- c(plot.vector, temp.plot.name)
      assign(temp.data.name, temp.data)
      assign(temp.yaxis.name, list (title = paste("PER: ", per.num)))
      assign(temp.xaxis.name, list (title = paste(" ")))
      temp.text <- paste(temp.plot.name, "<- plot_ly(", temp.data.name, ", x = var1, y = var2, 
                         mode = \"markers\", color = profit.norm, colors = mycolors) %>% layout(yaxis =", temp.yaxis.name , 
                         ", xaxis =", temp.xaxis.name, ", showlegend = FALSE)", sep = "")
      eval(parse(text = temp.text)) 
    }
  }
  plot.count <- length(plot.vector)
  cat("numbers of plot:", plot.count, "\n")
  FirstTime <- TRUE
  cat("printing plot:")
  for (i in 1:plot.count) {
    cat(i, " ")
    if (FirstTime == TRUE) {
      FirstTime <- FALSE
      temp.plot.name <- paste(plot.vector[i])
    } else {
      temp.plot.name <- paste(temp.plot.name, plot.vector[i], sep=",")
    }
  }
  cat("\n")
  nrow.plot.matrix <- round(plot.count/6)+1
  cat("building plot matrix with", nrow.plot.matrix, "rows", "\n")
  temp.text <- paste("subplot(", 
                     temp.plot.name, ", nrows = ", nrow.plot.matrix, ")", sep="")
  p <- eval(parse(text = temp.text))
  cloud.filename <- paste("r-docs/public-graph/", plot.num, ".bigplot", sep="")
  plotly_POST(p, filename = cloud.filename)
  return (p)
}