library(modulr)

"tool/perfcalculator" %provides% {
    # Charger uniquement le package quantmod
    library(quantmod)

    # Fonction pour calculer la performance hebdomadaire
    calculate_weekly_performance <- function(data) {
        data_cl <- Cl(data) # Extraire les prix de clôture
        start_of_week <- as.Date(Sys.Date()) - as.POSIXlt(Sys.Date())$wday + 1
        data_week <- data_cl[index(data_cl) >= start_of_week]
        weekly_return <- (last(data_week) / first(data_week)) - 1
        return(weekly_return)
    }

    # Fonction pour calculer la performance des X derniers jours
    calculate_last_days_performance <- function(data, days = 5) {
        data_cl <- Cl(data)
        data_recent <- tail(data_cl, days)
        recent_return <- (last(data_recent) / first(data_recent)) - 1
        return(recent_return)
    }

    # Fonction pour calculer une performance sur une période personnalisée
    calculate_period_performance <- function(data, start_date, end_date = Sys.Date()) {
        data_cl <- Cl(data)
        data_period <- data_cl[index(data_cl) >= start_date & index(data_cl) <= end_date]
        period_return <- (last(data_period) / first(data_period)) - 1
        return(period_return)
    }

    # Retourner les fonctions dans une liste
    return(list(
        weekly_performance = calculate_weekly_performance,
        last_days_performance = calculate_last_days_performance,
        period_performance = calculate_period_performance
    ))
}

