import yfinance as yf

def get_fundamentals(ticker):
    stock = yf.Ticker(ticker)
    info = stock.info

    return {
        "ticker": ticker,
        "name": info.get("shortName"),
        "sector": info.get("sector"),
        "currency": info.get("currency"),
        "eps": info.get("trailingEps"),
        "forward_eps": info.get("forwardEps"),
        "pe_ratio": info.get("trailingPE"),
        "peg_ratio": info.get("pegRatio"),
        "roe": info.get("returnOnEquity"),
        "roa": info.get("returnOnAssets"),
        "fcf_margin": info.get("freeCashflow", None),
        "payout_ratio": info.get("payoutRatio"),
        "dividend_yield": info.get("dividendYield"),
        "revenue_growth": info.get("revenueGrowth")
    }
