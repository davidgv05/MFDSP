# Downloads all raw series and stores an untouched snapshot in data/raw/.
library(quantmod)
library(readr)
library(here)

start <- as.Date("2017-01-01")
end   <- as.Date("2025-12-31")

raw_dir <- here("data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# ---- helper: xts -> data.frame(date, value) -> CSV ----------------------------
save_series <- function(x, name) {
  x  <- x[paste0(start, "/", end)]
  df <- data.frame(date = index(x), value = as.numeric(x[, 1]))
  write_csv(df, file.path(raw_dir, paste0(name, ".csv")))
  message(sprintf("%-8s %5d rows  %s to %s  (%d NA)",
                  name, nrow(df), min(df$date), max(df$date), sum(is.na(df$value))))
  invisible(df)
}

# ---- 1. FRED series ---------------------------------------
fred <- c(
  BTC   = "CBBTCUSD",   # Bitcoin, Coinbase, USD, 7 days/week
  NDX   = "NASDAQ100",  # Nasdaq-100 index, daily close
  VXN   = "VXNCLS",     # CBOE Nasdaq-100 volatility index (market stress)
  TBILL = "DTB3",       # 3-month T-bill rate, % p.a. (risk-free rate)
  FEDFUNDS = "DFF",     # Effective federal funds rate, % p.a.
  REALYLD  = "DFII10"   # 10-year real yield (TIPS), % p.a.
)

for (name in names(fred)) {
  x <- getSymbols(fred[[name]], src = "FRED", auto.assign = FALSE)
  save_series(x, name)
}

# ---- 2. Gold from Yahoo Finance (FRED no longer carries LBMA gold) ------------
gld <- getSymbols("GLD", src = "yahoo", from = start, to = end + 1,
                  auto.assign = FALSE)
save_series(Ad(gld), "GOLD")   # adjusted close of the SPDR Gold Shares ETF

# ---- 3. Record when the snapshot was taken ------------------------------------
writeLines(c(paste("Downloaded:", Sys.time()),
             paste("Period:", start, "to", end),
             paste("R:", R.version.string),
             paste("quantmod:", packageVersion("quantmod"))),
           file.path(raw_dir, "_download_info.txt"))

message("Done.")