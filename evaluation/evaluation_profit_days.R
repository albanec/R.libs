# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета дневных метрик доходности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы с данными по доходностям дней 
#'
#' Функция вычисляет параметры по доходностям дней торговли и формирует итоговый DF
#' 
#' @param data Полные данные (после  отработки стратегии)
#' @param balance Стартовый баланс
#'
#' @return profitTable.byDays DF с данными по profit'у (по дням)
#'
#' @export
ProfitTable_byDays <- function(data, balance, ...) {
  # разбор дней
  ### статистика по дням
  trdaysStatsList <- CalcTradingDaysStats(data = data)
  ### разбор статистики
  # индексы лудших/худших дней
  bestDay.index  <- which.max(trdaysStatsList[[1]]$Return)
  worstDay.index  <- which.min(trdaysStatsList[[1]]$Return)
  goodDay.index <- which(trdaysStatsList[[1]]$Return >= 0)
  badDay.index <- which(trdaysStatsList[[1]]$Return < 0)
  ## средний день в плюс
  meanGoodDayReturn <- 
    trdaysStatsList[[1]]$Return[goodDay.index] %>%
    mean(.)
  meanGoodDayReturn.percent <- 
    trdaysStatsList[[1]]$Return.percent[goodDay.index] %>%
    mean(.)
  ## средний день в минус
  meanBadDayReturn <- 
    trdaysStatsList[[1]]$Return[badDay.index] %>%
    mean(.)
  meanBadDayReturn.percent <- 
    trdaysStatsList[[1]]$Return.percent[badDay.index] %>%
    mean(.)
  ## всего дней в плюс
  numGoogDay <- 
    goodDay.index %>%
    length(.)
  ## всего дней в минус
  numBadDay <- 
    badDay.index %>%
    length(.)  
  ## max дней в плюс
  maxGoodDays <-
    which(trdaysStatsList[[2]]$DayType == 1) %>%
    {
      max(trdaysStatsList[[2]]$SeriesLength[.])
    }
  ## max дней в минус
  maxBadDays <-
    which(trdaysStatsList[[2]]$DayType == -1) %>%
    {
      max(trdaysStatsList[[2]]$SeriesLength[.])
    }  
  ## profit factor
  goodDay.sum <- sum(trdaysStatsList[[1]]$Return[goodDay.index])
  badDay.sum <- sum(trdaysStatsList[[1]]$Return[badDay.index])
  pf.daily <- goodDay.sum / abs(badDay.sum)
  ### Формирование итоговой таблицы
  profitTable.byDays <- data.frame(
                                   # прибыльные дни
                                   BestDay = trdaysStatsList[[1]]$Date[bestDay.index],
                                   BestDayReturn = trdaysStatsList[[1]]$Return[bestDay.index],
                                   BestDayReturnPercent = trdaysStatsList[[1]]$Return.percent[bestDay.index],
                                   MeanGoodDayReturn = meanGoodDayReturn,
                                   MeanGoodDayReturnPercent = meanGoodDayReturn.percent,
                                   NumGoogDay = numGoogDay,
                                   NumGoogDayPercent = last(trdaysStatsList[[1]]$Num) %>%
                                                       {
                                                         numGoogDay * 100 / .
                                                       },
                                   MaxGoodDays = maxGoodDays,
                                   FullGoodDayReturn = goodDay.sum,
                                   # убыточные дни
                                   WorstDay = trdaysStatsList[[1]]$Date[worstDay.index], 
                                   WorstDayReturn = trdaysStatsList[[1]]$Return[worstDay.index], 
                                   WorstDayReturnPercent = trdaysStatsList[[1]]$Return.percent[worstDay.index],
                                   MeanBadDayReturn = meanBadDayReturn,
                                   MeanBadDayReturnPercent = meanBadDayReturn.percent,
                                   NumBadDay = numBadDay,
                                   NumBadDayPercent = last(trdaysStatsList[[1]]$Num) %>%
                                                       {
                                                         numBadDay * 100 / .
                                                       },
                                   MaxBadDays = maxBadDays,
                                   FullBadDayReturn = badDay.sum,
                                   # профит-фактор по дням
                                   ProfitFactorDaily = pf.daily,
                                   #
                                   row.names = NULL)          
  # 
  return(profitTable.byDays)
}
#
###
#' Вычисление данных по торговым дням
#'
#' Функция вычисляет статистику по дням (trdayStats) и по сериям убытка/профита (trdaySeries)
#' 
#' @param data Полные данные отработки стратегии
#'
#' @return result Лист со статистикой по дням (внутри trdayStats и trdaySeries)
#'
#' @export
CalcTradingDaysStats <- function(data) {
  #
  data %<>%
    # очистка от строк c одинаковым индексом (если есть)
    {
      duplicated.ind <- 
        index(.) %>%
        duplicated(.) %>%
        which(.)
      result <-
      {
        .[-duplicated.ind]
      }
      return(result)
    }
  # разметка номеров дней
  data$trday.num <- 
    {
      data$endpoint[endpoints(data, on = "days")] <- 1
      return(data)
    } %>%
    {
      data$endpoint[!is.na(data$endpoint)]
    } %>%
    cumsum(.) 
  data$trday.num <- na.locf(data$trday.num, fromLast = TRUE) 
  ### раcчёт статистики по дням 
  trdayStats <-
    last(data$trday.num) %>%
    1:. %>%
    lapply(.,
           function(x) {
             OneTradingDaysStats_DF(data = data, n = x)
           }) %>%
    # объединение данных внутри листа в один df
    MergeData_inList_byRow(.) 
  trdaySeries <- 
    {
      ifelse(trdayStats$Return >= 0, 
             1,
            -1)
    } %>%
    rle(.) %>%
    {
      data.frame(DayType = .[[2]], SeriesLength = .[[1]])
    }
  result <- list(trdayStats = trdayStats, trdaySeries = trdaySeries)
  #
  return(result)
}
#
###
#' Вычисление данных по одному торговому дню
#'
#' Функция вычисляет 
#' 
#' @param data Полные данные отработки стратегии
#'
#' @return result DF с данными 
#'
#' @export
OneTradingDaysStats_DF <- function(data, n) {
  #
  # если торговые дни не посчитаны
  if (is.null(data$trday.num) == TRUE) {
    # разметка номеров дней
    data$trday.num <- 
      {
        data$endpoint[endpoints(data, on = "days")] <- 1
        return(data)
      } %>%
      {
        data$endpoint[!is.na(data$endpoint)]
      } %>%
      cumsum(.) 
    data$trday.num <- na.locf(data$trday.num, fromLast = TRUE) 
  } 
  ### выборка нужных столбцов
  data <- data[, c("balance", "trday.num")]
  ### расчёт
  result <- 
    # выгружаем данные по dd с номером n
    data[data$trday.num == n] %>%
    Convert_XTStoDF(.) %>%
    {
      df <-
        data.frame(Date = character(1) %>% 
                          as.numeric(.) %>% 
                          as.Date(.),
                   Num = as.numeric(1),
                   Return = as.numeric(1),
                   Return.percent = as.numeric(1)
                   ) 
      df$Date <- 
        first(.$date) %>%
        format(., "%Y-%m-%d") %>%
        as.POSIXct(., origin = "1970-01-01") 
      df$Num <- n
        df$Return <- 
        {
          first(.$balance) - last(.$balance) 
        }
      df$Return.percent <- 
        first(.$balance) %>%
        {
          df$Return * 100 / .
        }
      #
      return(df)
    }
  #
  return(result)
}
#