# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Фукции для парсинга готовых данных бэктеста 
# (из TSlab & WealthLab) и подготовки для анализа:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Итоговая функция для обработки данных из *Lab
#' 
#' @param file.path Путь к файлу с данными 
#' @param sep Разделитель в файле
#' @param m Количество месяцев торговли
#' @param profit Номер столбца с данными по профиту (по умолчанию == FALSE, выбор идёт автоматически)  
#' @param draw Номер столбца с данными по просадке (по умолчанию == FALSE, выбор идёт автоматически) 
#' @param var.list Вектор с номерами столбцов переменных (по умолчанию == FALSE, выбор идёт автоматически) 
#' @param q.hi Уровень квантиля для вычисления лучших значений (берётся всё, что выше квантиля)
#' @param q.low Уровень квантиля для вычисления худших значений (берётся всё, что ниже квантиля)
#' @param low Вычисляем худшие значения (классический квантиль) 
#' @param hi Вычисляем лучшие значения (всё, что больше квантиля) 
#' @param one.scale Нужно ли сохранить max/min заначения (для оптимизации тепловой шкалы)
#' @param tslab T/F данные из TSlab
#' @param trend.filter Фильтровать или нет контртрендовые значения
#'
#' @return data Полностью обработанные данные
#'
#' @export 
AllPreparation_LabsFile <- function(file.path, sep = ";",
                                    data = FALSE,
                                    autoParse = TRUE, 
                                    m = FALSE, 
                                    q.hi = FALSE, q.low = FALSE, low = FALSE, hi = FALSE,
                                    one.scale = TRUE,  tslab = TRUE, trend.filter = TRUE,
                                    ...) {
  #
  if (autoParse == TRUE) {
    data <- Parse_LabsCSV(autoParse = TRUE, file.path = file.path, sort = FALSE, var.names = FALSE, sep)
  } else {
    data <- Parse_LabsCSV(autoParse = FALSE, file.path = file.path, sort = FALSE, var.names = FALSE, sep, 
                          var.list, profit, draw)
  }  
  if (tslab == FALSE) {
    data$var0 <- BotNumSetLabsFile(data, bot.num.table)  
    data <- FilterDuplicatedRow_LabsFile(data)
  }
  # создание нормированной метрики
  if (m != FALSE) {
    data$profit.norm <- NormProfit_LabsFile(data, m = m) 
    data <- data[which(data$profit.norm > 0), ]
  } else {
    data <- data[which(data$profit > 0), ]
  }
  # фильтр контр-трендовых значений
  if (trend.filter == TRUE) {
    data <- data[which(data$var1 < data$var2), ] 
  }
  # фильтрация квантилей
  if (hi | low != FALSE) {
    temp.ColName <-
      colnames(data) %>%
      length(.) 
    data <- CalcQuantile(data, var = temp.ColName, q.hi, q.low, hi, low, abs = FALSE)
  }
  # масштабирование измерений
  if (one.scale == TRUE) {
    data[nrow(data)+1, ] <- c(rep(0, length(var.list)+2), data$profit.norm[[which.max(data$profit.norm)]])
    data[nrow(data)+1, ] <- c(rep(0, length(var.list)+3))
  }
  #
  return(data)
}
#
###
#' Функция для парсинга .csv файлов (заточена под выгрузку данных из WealthLab & TSlab)
#' 
#' @param file.path Путь к файлу (абсолютный путь)
#' @param var.list Лист с номерами столбцов нужных переменных 
#' (любое количество, функция подстраивается под нужное число)
#' @param profit Номер столбца с профитом
#' @param draw Номер столбца с данными по просадкам (нужно для нормирования доходностей)
#' @param sort Нужна ли сортировка занчений по доходности
#' @param var.names Выгрузить из исходного файла имена переменных
#'
#' @return data Выгруженные из .csv данные
#'
#' @export
Parse_LabsCSV <- function(file.path = file.path, 
                          autoParse, 
                          sort = FALSE, 
                          var.names = TRUE, sep = ";",
                          ...) {
  #
  # считывание файла 
  file <- read.table(file = file.path, header = F, sep = sep, as.is = T)    
  # выделяем нужные параметры
  #
  if (autoParse == TRUE) {
    # если autoParse включён, то номера столбцов определяются автоматически
      # данные по столбцам выносятся в base enviroment
    cat("Parse_LabsCSV:  Парсинг названий переменных и profit/draw...", "\n")
    # ключ для переменных: "_"
    var.list <<-
      file[1, ] %>%
      grep("_", .) 
    # ключ для профита: "Чистый П/У %"
    profit <<- 
      file[1, ] %>%
      grep("Чистый П/У %", .) 
    # ключ для просадки: "Макс. просадка %"
    draw <<- 
      file[1, ] %>%
      grep("Макс. просадка %", .) 
    #
    if (!exists("var.list")) {
      stop(paste("ERROR(Parse_LabsCSV):  Не сформирован var.list!!!", sep = ""))
    }
    if (!exists("profit")) {
      stop(paste("ERROR(Parse_LabsCSV):  Не найден profit!!!", sep = ""))
    }
    if (!exists("draw")) {
      stop(paste("ERROR(Parse_LabsCSV):  Не найден draw!!!", sep = ""))
    }
  } else {
    cat("Parse_LabsCSV:  Номера столбцов заданы в ручную", "\n") 
    if (!exists("var.list")) {
      stop(paste("ERROR(Parse_LabsCSV):  Не задан var.list!!!", sep = ""))
    }
    if (!exists("profit")) {
      stop(paste("ERROR(Parse_LabsCSV):  Не задан  номер столбца profit!!!", sep = ""))
    }
    if (!exists("draw")) {
      stop(paste("ERROR(Parse_LabsCSV):  Не задан номер столба draw!!!", sep = ""))
    }
  }
  #
  # название столбцов profit/draw
  profit.name <- file[[1, profit]] 
  draw.name <- file[[1, draw]]
  # всего пременных
    # n.vars выносятся в base enviroment
  n.vars <<- length(var.list)
  cat("############", "\n",
      "Парсинг файла:", ".......... ", file.path, "\n")
  cat("############",
      "Выбраны переменные & тепловой параметр:",
      "",
      sep = "\n")
  var.name.list <- c()
  name.raw <- file[1, ]
  file <- file[-1, ]
  file <- file[, colSums(is.na(file)) == 0]
  # temp дата-фрейм 
  temp.frame <- 
    nrow(file) %>%
    rep(NA, .) %>%
    data.frame(.) %>%
    .[, -1]
  for (i in 1:n.vars) {
    temp.var.name <- name.raw[[var.list[i]]]
    cat("var", i, ": ", ".......... ", temp.var.name, "\n")
    var.name.list <- c(var.name.list, temp.var.name)
    temp.frame[, paste("var", i, sep = "")] <- as.numeric(gsub("\\,", ".", file[[var.list[i]]])) 
    temp.frame[, paste("var", i, sep = "")] <- 
      as.numeric(gsub("\\s", "", temp.frame[, paste("var", i, sep = "")]))
  }
  cat("profit: ", ".......... ", profit.name, "\n")
  cat("draw:   ", ".......... ", draw.name, "\n")
  temp.frame$profit <- as.numeric(gsub("\\,", ".", file[[profit]]))
  temp.frame$profit <- as.numeric(gsub("\\s", "", temp.frame$profit))
  temp.frame$draw <- as.numeric(gsub("\\,", ".", file[[draw]]))
  temp.frame$draw <- as.numeric(gsub("\\s", "", temp.frame$draw))
  temp.frame$temp.frame <- NULL
  # чистим от лишнего 
  remove(file)  
  # сортировка по профиту
  if (sort == TRUE) {
    temp.frame <- temp.frame[order(-temp.frame$profit), ]
    cat("############", "\n",
        "Сортировка по данных по profit'у", "\n")  
  }
  if (var.names == TRUE) {
  # colnames(temp.frame) <- c(var1.name, var2.name, var3.name, profit.name)  
    colnames(temp.frame) <- c(var.name.list, profit.name, draw.name)
    cat("############", "\n",
        "Столбцы проименованы", "\n")
  }
  cat("############",
      "Готово.", 
      "############",
      sep = "\n")
  #
  return(temp.frame)
}
#
###
#' Функция для сравнения двух распарсенных данных бэктеста и вывода только повторяющихся строк
#' 
#' @param file1 Считанный из .csv file1
#' @param file2 Считанный из .csv file2
#' @param rec Наличие/отсутствие рековери
#' @param p.diff Нужен ли расчёт сглаженности equity (abs разности профитов)
#'
#' @return file1 Файл содер. в себе только повторяющие параметры роботов (+|- рековери и разница профитов)
#'
#' @export
Compare_LabsFile <- function(file1, file2, rec = FALSE, p.diff = TRUE) {
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
    file1$var0 <- apply(file1, 
                        1, 
                        paste, collapse='')
    file2$var0 <- apply(file2, 
                        1, 
                        paste, collapse='')
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
    file1 <- file1[order(-file1$var0), ]
    file2 <- file2[order(-file2$var0), ]
    file1$type <- "file1"
    file2$type <- "file2"
    remove(temp.file1)
    remove(temp.file2)
  # проверка на совпадение
    l <- rbind(file1, file2)
    t1 <- duplicated(l$var0, fromLast = TRUE)
    t2 <- duplicated(l$var0)
    file1 <- l[t1, ]
    file2 <- l[t2, ]
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
###
#' Функция для генерации номеров ботов (нужна для анализа тестов из WealthLab)
#' 
#' бестолковая, тупо генерит бинарный ряд 2^n-1 и присваивает номера строкам
#'
#' @param n Степень двойки
#'
#' @return bot.num.table Таблица бинарных номеров роботов с присвоенными десятеричными номерами
#'
#' @export
BotBinNumGenLabsFile <- function(n) {
  #
  decimals <- seq(0, 2^n-1)
  m <- sapply(decimals,
              function(x) { 
                as.integer(intToBits(x))
              })
  m <- 
    head(m, 3) %>%
    t(.)
  bot.num.table <- 
    cbind(rep(0, nrow(m)), m) %>%
    rbind(., cbind(rep(1, nrow(m)), m)) %>%
    rbind(., cbind(rep(2, nrow(m)), m))
    apply(., 
          1, 
          paste, collapse='') %>%
    {
      cbind(seq(1, length(.)), .)
    }
  #
  return(bot.num.table)
}
#
###
#' Функция выставляет соответствие бинарных номеров ботов из WealthLab десятиричным номерам из bot.num.table
#' 
#' @param file Считанные данные
#' @param bot.num.table Сгенерированный ряд бинарных и десятиричных номеров
#'
#' @return file$var0 Столбец с номерами
#'
#' @export
BotNumSetLabsFile <- function(file, bot.num.table) {
  #
  file.temp <- file
  file.temp$var1 <- NULL
  file.temp$var2 <- NULL
  file.temp$var3 <- NULL
  file.temp$profit <- NULL
  file.temp$draw <- NULL
  file$var0 <- apply(file.temp, 
                     1, 
                     paste, collapse = '')   
  for (i in 1:nrow(bot.num.table)) {
    n <- which(file$var0 == bot.num.table[[i, 2]])
    file$var0[n] <- bot.num.table[[i, 1]]
  } 
  #
  return(file$var0)
}
#
###
#' Функция выделяет уникальные наборы параметров ботов
#' 
#' @param file Данные тестов с присвоенными номерами ботов
#'
#' @return file Отфильтрованные данные (только уникальные номера ботов)
#'
#' @export
FilterDuplicatedRow_LabsFile <- function(file) {
  #
  file.temp <- file
  file.temp$profit <- NULL
  file.temp$draw <- NULL
  file.temp$var0 <- NULL
  file.temp$var0 <- apply(file.temp, 
                          1, 
                          paste, collapse='')
  file <- file[which(duplicated(file.temp$var0) == FALSE), ]
  #
  return(file)
}
#
###
#' Функция нормировки профита к просадке и к году 
#' 
#' @param file Данные бэков, содержащие profit и draw 
#' @param m Количество месяцев торговли
#'
#' @return file$profit.norm Столбец нормированных доходностей
#'
#' @export
NormProfit_LabsFile <- function(file, m) {
  #
  file$profit.norm <- (file$profit*12) / (abs(file$draw)*m)
  #
  return(file$profit.norm)
}
#
