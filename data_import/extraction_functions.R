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
    
  
  list(
    get_prices_yahoo = get_prices_yahoo_,
    get_prices_batch = get_prices_batch_,
    get_fundamentals_tidyquant = get_fundamentals_tidyquant_,
    safe_get = safe_get_,
    load_allocation_excel = load_allocation_excel_
  )
}