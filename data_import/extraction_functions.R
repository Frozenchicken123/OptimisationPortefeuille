# Function of extraction of price
"data_imports/extraction_functions" %requires% list() %provides% {
  # load libraries
  library(quantmod)
  library(tidyquant)
  library(tidyverse)
  library(BatchGetSymbols)
  
  get_prices_yahoo_ <- function(ticker, from = "2010-01-01", to = Sys.Date()) {
    tryCatch(
      {
        data <- quantmod::getSymbols(ticker, src = "yahoo",
                                     from = from, to = to,
                                     auto.assign = FALSE)
        
        data <- data %>%
          as_tibble(rownames = "date") %>%
          rename_with(~ gsub(paste0(ticker, "."), "", .x))
        return(data)
      },
      error = function(e) {
        warning(glue::glue("Erreur pour {ticker} : {e$message}"))
        return(NULL)
      }
    )
  }
  
  get_prices_batch_ <- function(tickers, from = "2010-01-01", to = Sys.Date()) {
    BatchGetSymbols::BatchGetSymbols(
      tickers = tickers,
      first.date = as.Date(from),
      last.date = as.Date(to),
      thresh.bad.data = 0.5
    )$df.tickers
  }
  
  
  get_fundamentals_tidyquant_ <- function(tickers) {
    tq_get(tickers, get = "key.stats") # key.stats = Yahoo Finance data (via tidyquant)
  }
  
  safe_get_ <- function(func, ...) {
    tryCatch(
      {
        func(...)
      },
      error = function(e) {
        warning(paste("Erreur :", e$message))
        return(NULL)
      }
    )
  }
  
  list(
    get_prices_yahoo = get_prices_yahoo_,
    get_prices_batch = get_prices_batch_,
    get_fundamentals_tidyquant = get_fundamentals_tidyquant_,
    safe_get = safe_get_
  )
}