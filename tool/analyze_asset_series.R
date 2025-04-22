library(modulr)

#' Analyse les métriques clés pour une série de prix complète
#'
#' @param data_xts Objet xts complet OHLCV pour un seul actif
#' @param rf Taux sans risque (ex: 0.01 = 1%) pour Sharpe
#' @return data.frame avec les métriques calculées
#' @export



"tool/analyze_asset_series" %provides% {
  
  # Chargement des packages
  library(PerformanceAnalytics)
  library(quantmod)
  library(dplyr)
  
  analyze_asset_series <- function(data_xts, rf = 0.01) {
    library(PerformanceAnalytics)
    library(quantmod)
    library(dplyr)
    
    prices <- Ad(data_xts)
    volume <- Vo(data_xts)
    returns <- na.omit(Return.calculate(prices, method = "log"))
    
    start_date <- format(start(prices), "%Y-%m-%d")
    end_date <- format(end(prices), "%Y-%m-%d")
    period <- paste0(start_date, " to ", end_date)
    nb_days <- nrow(returns)
    
    annual_return <- Return.annualized(returns)
    annual_vol <- StdDev.annualized(returns)
    sharpe <- SharpeRatio.annualized(returns, Rf = rf / 252)
    drawdown <- maxDrawdown(returns)
    avg_volume <- mean(volume, na.rm = TRUE) / 1e6
    
    result <- data.frame(
      Ticker = colnames(prices),
      Period = period,
      NbJours = nb_days,
      AnnualReturn = round(as.numeric(annual_return), 4),
      AnnualVolatility = round(as.numeric(annual_vol), 4),
      MaxDrawdown = round(as.numeric(drawdown), 4),
      SharpeRatio = round(as.numeric(sharpe), 4),
      AvgDailyVolume_M = round(as.numeric(avg_volume), 2)
    )
    
    return(result)
  }
  
  
}