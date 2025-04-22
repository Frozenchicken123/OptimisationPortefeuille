# Function of extraction of price
"data_import/extraction_functions" %requires% list() %provides% {
  # load libraries
  library(quantmod)
  library(tidyquant)
  library(BatchGetSymbols)
  library(readxl)
  library(dplyr)
  library(stringr)
  library(here)
  library(progress)
  
  # Fichier : R/data_import/get_price_data.R
  
  get_price_data_enriched_ <- function(
    tickers, start_date = "2020-01-01", 
    end_date = Sys.Date(), 
    sleep_sec = 2) {
    
    prices <- list()
    n <- length(tickers)
    
    # 🟩 Initialisation de la barre de progression
    pb <- progress_bar$new(
      format = "Téléchargement [:bar] :percent | :current/:total | :ticker",
      total = n,
      clear = FALSE,
      width = 60
    )
    
    for (i in seq_along(tickers)) {
      ticker <- tickers[i]
      pb$tick(tokens = list(ticker = ticker))
      
      tryCatch({
        data <- getSymbols(Symbols = ticker, src = "yahoo", 
                           from = start_date, to = end_date,
                           auto.assign = FALSE)
        prices[[ticker]] <- data
      }, error = function(e) {
        warning(paste("❌ Erreur pour le ticker", ticker, ":", e$message))
      })
      
      Sys.sleep(sleep_sec)  # Pour éviter surcharge Yahoo
    }
    
    return(prices)
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
  
  load_allocation_excel_ <- function(relative_path) {
    
    # Résolution du chemin absolu basé sur le projet
    full_path <- here::here(relative_path)
    
    # Tentative de lecture
    df <- tryCatch({
      readxl::read_excel(full_path, sheet = 1) # Skip les 2 premières lignes de titre
    }, error = function(e) {
      stop(paste("❌ Erreur de lecture du fichier Excel :", e$message))
    })
    
    # Vérifier présence des colonnes requises
    colnames(df) <- str_trim(names(df))
    expected <- c("Symbole", "Valeur totale CHF")
    if (!all(expected %in% names(df))) {
      stop("❌ Le fichier doit contenir les colonnes 'Symbole' et 'Valeur totale CHF'.")
    }
    
    # Nettoyage et transformation
    df_clean <- df |>
      filter(!Symbole %in% c("EAPI", "Sous-total Actions en CHF", "Total"), !is.na(Symbole)) |>
      select(Ticker = Symbole, InvestedAmount = `Valeur totale CHF`) |>
      mutate(
        Ticker = str_to_upper(as.character(Ticker)),
        InvestedAmount = as.numeric(InvestedAmount)
      ) |>
      filter(InvestedAmount > 0)
    
    if (nrow(df_clean) == 0) { 
      stop("❌ Aucun actif valide détecté après nettoyage.")
    }
    
    return(df_clean)
  }
  
  # Fichier : R/data_import/map_to_yahoo_tickers.R
  
  #' Mapper les tickers internes (UBS/SIX/EU) vers les tickers Yahoo Finance
  #'
  #' @param tickers Vecteur de tickers locaux (ex: NESN, TTE, AAPL)
  #' @return Vecteur de tickers Yahoo Finance
  #' @export
  map_to_yahoo_tickers_ <- function(tickers) {
    # 🔄 Table de correspondance (à étendre librement)
    custom_map <- c(
      "NESN" = "NESN.SW",   # Nestlé (SIX)
      "UBSG" = "UBSG.SW",   # UBS (SIX)
      "NOVN" = "NOVN.SW",   # Novartis
      "SREN" = "SREN.SW",   # Swiss Re
      "ABBN" = "ABBN.SW", # ABB
      "ACLN" = "ACLN.SW",
      "TTE"  = "TTE.PA"     # TotalEnergies (Euronext Paris)
    )
    
    # ✅ Mapper : si présent dans la table, remplacer ; sinon garder original
    mapped <- sapply(tickers, function(tkr) {
      if (tkr %in% names(custom_map)) {
        return(custom_map[[tkr]])
      } else {
        return(tkr)
      }
    })
    
    return(unname(mapped))
  }
  
  list(
    get_price_data_enriched = get_price_data_enriched_,
    get_prices_batch = get_prices_batch_,
    get_fundamentals_tidyquant = get_fundamentals_tidyquant_,
    safe_get = safe_get_,
    load_allocation_excel = load_allocation_excel_,
    map_to_yahoo_tickers = map_to_yahoo_tickers_
  )
}