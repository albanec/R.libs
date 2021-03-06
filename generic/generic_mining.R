# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для data mining'а
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция создает матрицу из n строк вектора x
#'
#' @param x Вектор, который надо повторить
#' @param n Количество нужных строк
#'
#' @return m Матрица, состоящая их n сток вектора x
#'
#' @export
Repeat_Row <- function(x,n) {
  #
  m <- matrix(rep(x,each = n),nrow = n)
  #
  return(m)
}
#
###
#' Функция создает матрицу из n столбцов вектора x
#'
#' @param x Вектор, который надо повторить
#' @param n Количество нужных столбцов
#'
#' @return m Матрица, состоящая их n столбцов вектора x
#'
#' @export
Repeat_Col <- function(x,n) {
  #
  m <- matrix(rep(x,each = n), ncol = n, byrow = TRUE)
  #
  return(m)
}
#
###
#' Функция объединения данных в один XTS 
#'
#' @param data.list Лист, сожержащий XTS нужных тикеров
#' @param col.name Если нужно объединить опред. столбцы, то присвоить название
#'
#' @return list(merged.data) Лист с xts рядом объединенных значений (по всем тикерам)
#'
#' @export
MergeData_inList_byCol <- function(data.list, col.name = FALSE) {
  # 
  n.ticker <- length(data.list) 
  FirstTime <- TRUE
  #  чтение и объединение данных
  for (i in 1:n.ticker) {
    data <- data.list[[i]]
    data.name <- 
      names(data)[grep("Close", names(data))] %>%
      sub(".Close", "", .)
    cat("INFO(MergeData_fromAll_toOne):  Processing StocksData:  ", data.name, "\n")
    if (col.name != FALSE) {
      temp.text <-
      paste(data.name, col.name, sep = ".") %>%
      paste("data <- data$", ., sep = "")
      eval(parse(text = temp.text))
    }
    if (FirstTime == TRUE) {
      FirstTime <- FALSE
      merged.data <- data
    } else {
      merged.data <- merge(merged.data, data)
    }
  }  
  merged.data <- list(merged.data)
  names(merged.data) <- c("merged.data")
  #
  return(merged.data)
}
#
###
#' Функция объединения данных внутри листа построчно
#'
#' @param data.list Лист
#'
#' @return data.list[[1]] Объединенные данные
#'
#' @export
MergeData_inList_byRow <- function(data.list) {
  #
  while (length(data.list) > 1) {
  idxdata.list <- seq(from=1, to=length(data.list), by=2)
  data.list <- lapply(idxdata.list, 
                      function(i) {
                        if(i == length(data.list)) { 
                          return(data.list[[i]]) 
                        }
                        return(rbind(data.list[[i]], data.list[[i+1]]))
                      })
  }
  #
  return(data.list[[1]])
}
#
###
#' Функция выделения столбцов с именами, содержащими target параметр
#'
#' @param data Исходный XTS
#' @param target Ключ для поиска
#' 
#' @return data XTS ряд, очищенный от NA (по всем тикерам)
#'
#' @export
SubsetCol_inXTS_byTarget <- function(data, target) {
  #
  data <-
    colnames(data) %>%
    grep(target, .) %>%
    data[, .]
  #
  return(data)
}
#
###
#' Функция очистки мусора в столбцах данных
#'
#' @param data Исходные данные
#' @param target Ключ для поиска
#' 
#' @return data Данные, очищенный от столбцов с target в названии
#'
#' @export
CleanGarbage_inCols <- function(x, target = "temp") {
  x <- 
    colnames(x) %>%
    grep(target, .) %>%
    {
      x[, -.]
    }
  return(x)
}
#
###
#' Функция вычисление лучших / худших значений (на основе квантиля) 
#' 
#' @param data Данные 
#' @param var Столбец данных с качественной характеристикой
#' @param q.hi Уровень квантиля для вычисления лучших значений (берётся всё, что выше квантиля)
#' @param q.low Уровень квантиля для вычисления худших значений (берётся всё, что ниже квантиля)
#' @param low Вычисляем худшие значения (классический квантиль) 
#' @param hi Вычисляем лучшие значения (всё, что больше квантиля) 
#' @param two Вычисляем "середину" между двумя уровнями
#' @param abs Eсли нужно, задать абсолютное значение квантиля (вычисленное ранее)
#'
#' @return data Отфильтрованные данные
#'
#' @export 
CalcQuantile <- function(data, var, q.hi = 0, q.low = 0, 
                         two = FALSE, low = FALSE, hi = FALSE, abs = FALSE) {
  #
  if (two == TRUE) {
    # подготовка данных
    data <- data[order(-data[[var]]), ]
    # вычисление квантилей
    ifelse(abs == FALSE, 
           q.hi.value <- quantile(data[[var]], q.hi),
           q.hi.value <- as.numeric(q.hi))
    n.hi <- which(data[, var] < q.hi.value)
    ifelse(abs == FALSE, 
           q.low.value <- quantile(data[[var]], q.low), 
           q.low.value <- as.numeric(q.low)) 
    data <- data[n.hi, ]
    n.low <- which(data[, var] > q.low.value )
    data <- data[n.low, ]
    } 
  if (hi == TRUE) {
    data <- data[order(-data[[var]]), ]
    ifelse(abs == FALSE, q.hi.value <- quantile(data[[var]], q.hi), q.hi.value <- as.numeric(q.hi))
    n.hi <- which( data[, var] > q.hi.value )  
    data <- data[n.hi, ]
    }
  if (low==TRUE) {
    data <- data[order(-data[[var]]), ]
    ifelse (abs == FALSE, q.low.value <- as.numeric(quantile(data[[var]], q.low)), q.low.value <- as.numeric(q.low)) 
    n.low <- which( data[, var] < q.low.value )
    data <- data[n.low, ]
    }
  #  
  return(data)
}
#
